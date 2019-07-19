-- MIT License
--
-- Copyright (c) 2019 nantiamak
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.


{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Sql2tf_translator
  (endToEndTranslate) where

import Data.Aeson

import System.Environment

import Database.Sql.Type
import Database.Sql.Vertica.Type
import Database.Sql.Vertica.Parser
import Database.Sql.Position

import qualified Data.Text.Lazy as L
import qualified Data.Tuple as Tuple
import qualified Data.Map as M
import qualified Data.List as List
import Data.Maybe
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String

import GHC.Int
import Data.List.Split


-- Data type representing a pair of an operator and two operands
-- Constants
-- Column expressions
-- Binary expressions 1+1
-- Unary expressions -1
-- Function operator parameters
data Operator_operands_pair_2 =
  Constant String
  |Column String
  |Function String [Operator_operands_pair_2]
  deriving (Show)



-- Function that extracts the name of a view
extractViewName :: CreateTableName r ~ QTableName f => Either a1 (VerticaStatement r a2) -> L.Text
extractViewName (Right (VerticaStandardSqlStatement (CreateViewStmt
                                                     (CreateView {createViewName=QTableName _ _ n})))) = n
extractViewName _ = ""


-- Function that extracts a list of expressions from a select query
extractSelectExpressions :: Either a1 (VerticaStatement r a2) -> [Selection r a2]
extractSelectExpressions (Right (VerticaStandardSqlStatement stmt)) =
  case stmt of
    (QueryStmt (QuerySelect _ (Select {selectCols=SelectColumns _ s}))) -> s
    (CreateViewStmt (CreateView {createViewQuery=QuerySelect _ (Select {selectCols=SelectColumns _ s})})) -> s
    _ -> []
extractSelectExpressions _ = []



-- Function that extracts group by expressions (if any) from a select query
extractGroupByExpressions (Right (VerticaStandardSqlStatement
                                  (CreateViewStmt
                                   (CreateView {createViewQuery=QuerySelect _ (Select {selectGroup=g})})))) = g
extractGroupByExpressions _ = Nothing


-- These two functions decompose the extractGroupByExpressions function in two steps
extractPositionExpr :: SelectGroup r a -> [Expr r a]
extractPositionExpr (SelectGroup _ [GroupingElementExpr _ (PositionOrExprExpr r)]) = [r]

--ColumnRef r ~ QColumnName f => 
--extractGroupByColumns :: Expr r a -> L.Text
extractGroupByColumns (ColumnExpr _ (QColumnName _ _ f)) = f


-- Function that checks if a select expression involves a numeric expression
isArithmeticSelectExpr :: (ColumnRef r ~ QColumnName Maybe) =>
     Expr r a -> Bool
isArithmeticSelectExpr expr =
  case expr of (UnOpExpr _ (Operator op) expr1) -> True
               (BinOpExpr _ (Operator op) expr1 expr2) -> True
               (FunctionExpr _ (QFunctionName _ _ fn) _ exprs _ _ _) -> True
               _ -> False


-- Extracts operators and operands from an expression
extractOperatorsAndOperandsExpr :: (ColumnRef r ~ QColumnName Maybe) =>
     Expr r a -> [Operator_operands_pair_2]
extractOperatorsAndOperandsExpr expr =
  case expr of
    (ConstantExpr _ (NumericConstant _ c)) ->
            [(Function "to_double" [(Constant (L.unpack c))])]
    -- this doesn't work if there is no table name before column
    (ColumnExpr _ (QColumnName _ (Just (QTableName _ _ r)) _)) ->
            [(Column (L.unpack r))]
    (UnOpExpr _ (Operator op) expr1) ->
            [(Function (L.unpack op) (extractOperatorsAndOperandsExpr expr1))]
    (BinOpExpr _ (Operator op) expr1 expr2) ->
            [(Function (L.unpack op) (extractOperatorsAndOperandsExpr expr1 ++ extractOperatorsAndOperandsExpr expr2))]
    (FunctionExpr _ (QFunctionName _ _ fn) _ exprs _ _ _) ->
        [(Function (L.unpack fn) (concat [extractOperatorsAndOperandsExpr expr | expr <- exprs]))]



-- Function that calls extractOperatorsAndOperandsExpr to extract operators and operands from a select expression
extractOperatorsAndOperandsSelect :: (ColumnRef r ~ QColumnName Maybe) =>
     Selection r a -> [Operator_operands_pair_2]
extractOperatorsAndOperandsSelect (SelectExpr _ _ e) =
  case (isArithmeticSelectExpr e) of True -> extractOperatorsAndOperandsExpr e
                                     False -> []


-- Function that matches each operator extracted from extractOperatorsAndOperands to a TensorFlow operator
matchTensorFlowOperator :: String -> String
matchTensorFlowOperator op =
    case op of
      "sum*" -> "tf.tensordot"
      "sum" -> "tf.reduce_sum"
      "count" -> "tf.size"
      "avg" -> "tf.reduce_mean"
      "exp" -> "tf.exp"
      "ln" -> "tf.log"
      "negative" -> "tf.negative"
      "dot" -> "tf.tensordot"
      "+" -> "tf.add"
      "-" -> "tf.subtract"
      "*" -> "tf.multiply"
      "/" -> "tf.div"
      "pow2" -> "tf.square"
      "to_double" -> "tf.to_double"
      _ -> "Operator not implemented"



-- Function that constructs the right part (after =) of a TensorFlow command
constructTFExpression :: [Operator_operands_pair_2] -> Maybe (SelectGroup RawNames a2) -> String
constructTFExpression pair group_bys =
  case pair of [(Constant s)] -> s
               [(Column s)] -> s
               [Function "sum" [Function "*" [f1,f2]]] ->
                 "tf.tensordot(" ++ (constructTFExpression [f1] group_bys) ++ "," ++ (constructTFExpression [f2] group_bys) ++ ",axes=1)"
               [Function "pow" [(Column f1), (Function "to_double" [(Constant "2")])]] ->
                 (matchTensorFlowOperator "pow2") ++ "(" ++ f1 ++ ")"
               [(Function op [f])]
                 |op == "-" -> (matchTensorFlowOperator "negative") ++ "(" ++ (constructTFExpression [f] group_bys) ++ ")"
                 |op == "sum" -> (matchTensorFlowOperator op) ++ "(" ++ (constructTFExpression [f] group_bys) ++ "," ++ reduce_axes_string ++ ")"
                 |op == "avg" -> (matchTensorFlowOperator op) ++ "(" ++ (constructTFExpression [f] group_bys) ++ "," ++ reduce_axes_string ++ ")"
                 |otherwise -> (matchTensorFlowOperator op) ++ "(" ++ (constructTFExpression [f] group_bys) ++ ")"
               [(Function op [f1, f2])] ->
                 (matchTensorFlowOperator op) ++ "(" ++ (constructTFExpression [f1] group_bys) ++ "," ++ (constructTFExpression [f2] group_bys) ++ ")"
               [(Function op (f:fs))] ->
                 (matchTensorFlowOperator op) ++ "(" ++ (constructTFExpression [f] group_bys) ++ "," ++ (constructTFExpression fs group_bys) ++ ")"

  where reduce_axes_string =
          case reduce_axes of ["None"] -> "None"
                              [x] -> "1"
                              (x:xs) -> show $ (map (\e -> List.elemIndex e reduce_axes) reduce_axes)
        reduce_axes =
          case group_bys of Just group_bys -> (map (\e -> extractGroupByColumns e) (extractPositionExpr group_bys))
                            Nothing -> ["None"]


-- Function that takes a list of strings and generates their lowercase version
toLowerCase :: [String] -> [String]
toLowerCase table = map (\s -> L.unpack $ L.toLower $ L.pack s ) table

-- Function that identifies whether an expression needs to be rewritten in terms of features and weights
needsRewriting :: [Operator_operands_pair_2] -> [String] -> [String] -> [Bool]
needsRewriting pair feature_tables variable_tables =
  case pair of [(Function op [Column f1, Column f2])]
                 |elem f1 feature_tables && elem f2 variable_tables -> [True]
                 |elem f1 variable_tables && elem f2 feature_tables -> [True]
                 |otherwise -> [False]
               [(Function op [Column f])]
                 |elem f feature_tables -> [True]
                 |elem f variable_tables -> [True]
                 |otherwise -> [False]
               [(Function op (f:fs))] ->
                 (needsRewriting [f] feature_tables variable_tables) ++ (needsRewriting fs feature_tables variable_tables)
               _ -> [False]


-- Function that generates indices for rewritten expressions in terms of features and weights
generateFeatureIndices :: Int -> [[String]] -> [[Int]]
generateFeatureIndices n [[]] = [[]]
generateFeatureIndices n [f] = [[n..((length f)+n-1)]]
generateFeatureIndices n (h:t) = [n..((length h)+n-1)] : (generateFeatureIndices ((length h)+n) t)


constructFeatureExpressions :: String -> String -> [Int] -> [Operator_operands_pair_2]
constructFeatureExpressions op1 op2 [h] = [(Function op2 [Column ("features["++show(h)++"]"), Column ("weights["++show(h)++"]")])]
constructFeatureExpressions op1 op2 (h:t) = [Function op1 ([(Function op2 [Column ("features["++show(h)++"]"),
                                                                           Column ("weights["++show(h)++"]")])] ++ (constructFeatureExpressions op1 op2 t))] 


-- Function that rewrites of an expression in terms of features and weights
rewriteOperands :: String -> [String] -> [[Int]] -> [String] -> [Operator_operands_pair_2] -> [Operator_operands_pair_2]
rewriteOperands connector_op feature_tables indices_list variable_tables pair =
  case pair of [(Function op [Column f1, Column f2])]
                 |elem f1 feature_tables && elem f2 variable_tables ->
                  constructFeatureExpressions connector_op op (indices_list !! (fromJust (List.elemIndex f1 feature_tables)))
                 |elem f1 variable_tables && elem f2 feature_tables -> [(Function op [Column "weights", Column "features"])]
                 |otherwise -> [(Function op [Column f1, Column f2])]
               [(Function op [Column f])]
                 |elem f feature_tables -> [(Function op [Column "features"])]
                 |elem f variable_tables -> [(Function op [Column "weights"])]
                 |otherwise -> [(Function op [Column f])]
               [(Function op [f])] -> [Function op (rewriteOperands op feature_tables indices_list variable_tables [f])]
               [(Function op [f1, f2])] ->
                 [Function op ((rewriteOperands op feature_tables indices_list variable_tables [f1]) ++ (rewriteOperands op feature_tables indices_list variable_tables [f2]))]
          

-- Function that initiates the rewriting of an expression in terms of features and weights
rewritePair :: [[Operator_operands_pair_2]] -> [String] -> [String] -> [[String]] -> [[Operator_operands_pair_2]]
rewritePair pair_list feature_tables variable_tables feature_names = rewritten_pair
  where feature_tables_toLower = toLowerCase feature_tables
        variable_tables_toLower = toLowerCase variable_tables
        operators_and_operands_features = filter (\op -> elem True (needsRewriting op feature_tables_toLower variable_tables_toLower)) pair_list
        rewritten_pair =
          case operators_and_operands_features of [] -> pair_list
                                                  _ -> map (\e -> rewriteOperands "" feature_tables_toLower indices variable_tables_toLower e) operators_and_operands_features
                                  
        indices = generateFeatureIndices 0 feature_names

-- Function that constructs the whole TensorFlow command, both left and right part
translateToTensorFlowCommand :: L.Text -> [String] -> [String] -> [[String]] -> String
translateToTensorFlowCommand sql_statement feature_tables variable_tables feature_names = tf_command
  where select_expressions = extractSelectExpressions (Database.Sql.Vertica.Parser.parse sql_statement)
        group_by_expressions =  extractGroupByExpressions $ (Database.Sql.Vertica.Parser.parse sql_statement)
        operators_and_operands = (map (\e -> extractOperatorsAndOperandsSelect e) select_expressions)
        operators_and_operands_arithmetic = filter (\e -> (length e) > 0) operators_and_operands
        tf_command_right_part =
          case feature_tables of [h] -> (map (\e -> (constructTFExpression e group_by_expressions)) operators_and_operands_arithmetic)
                                 (h:t) -> (map (\e -> (constructTFExpression e group_by_expressions)) (rewritePair operators_and_operands_arithmetic feature_tables variable_tables feature_names))
                 
        tf_command_left_part = L.unpack (extractViewName (Database.Sql.Vertica.Parser.parse sql_statement))
        tf_command = tf_command_left_part ++ "=" ++ (tf_command_right_part !! 0)

   
-- Function that creates the initialization commands needed for TensorFlow variables
-- When features are normalized in multiple tables, we use this information to generate
-- a weights initialization that has the same shape as the features tensor. This will be
-- generated as many times as the number of feature tables , but as this does not create a
-- glitch we can live with this redudancy for now.
initializeVariableTensors :: String -> [String] -> M.Map [Char] [Integer] -> String
initializeVariableTensors v features dims = variable_initialization_string
  where variable_initialization_string =
          case (List.length features) of 1 -> v ++ "=tf.Variable(tf.ones(" ++ tensor_shape ++ ", tf.float64))"
                                         _ -> "weights=tf.Variable(tf.ones(tf.shape(features), tf.float64))"
  
        tensor_shape =
          case tensor_dimensions of [x] -> show (tensor_dimensions)
                                    (x:xs) -> "[" ++ tensor_dimensions_string ++ "]"

        tensor_dimensions_string = List.intercalate "," (map (\d -> show d) tensor_dimensions)
        tensor_dimensions = fromJust (M.lookup v dims)


-- Function that creates the initialization commands needed for TensorFlow constants
initializeConstantTensors :: String -> String
initializeConstantTensors v = v ++ "=tf.constant()"



------------------------ Functions for constructing the SQL query that pivots feature table ------------------------ 

-- Checks whether a statement is a create table statement
isCreateTableStatement :: Either a1 (VerticaStatement r a2) -> Bool
isCreateTableStatement (Right (VerticaStandardSqlStatement (CreateTableStmt _))) = True
isCreateTableStatement (Right (VerticaStandardSqlStatement _)) = False

-- Extracts create table statements from a list of statements
extractCreateTableStatements :: [L.Text] -> [L.Text]
extractCreateTableStatements statements = filter (\st ->  isCreateTableStatement (Database.Sql.Vertica.Parser.parse st)) statements


-- Given the name of a table it extracts its create table statement
extractCreateTableStatementFromName :: [Char] -> [L.Text] -> [L.Text]
extractCreateTableStatementFromName tableName tableStatements = statement
  where statement = filter (\st -> L.isPrefixOf (L.pack ("CREATE TABLE " ++ tableName)) st) tableStatements


extractColumnsFromTable (Right (VerticaStandardSqlStatement
                                (CreateTableStmt (CreateTable _ _ _ _ _
                                                  (TableColumns _ ((ColumnOrConstraintColumn
                                                                    (ColumnDefinition _ (QColumnName _ _ n) (PrimitiveDataType _ t _) _ _ _)):|[x])) _)))) = [(n, t)]

extractColumnsFromTable (Right (VerticaStandardSqlStatement
                                (CreateTableStmt (CreateTable a b c d e (TableColumns f ((ColumnOrConstraintColumn
                                                                                          (ColumnDefinition _ (QColumnName _ _ n)
                                                                                           (PrimitiveDataType _ t _) _ _ _)):|(x:xs))) g)))) =
  [(n, t)] ++ extractColumnsFromTable (Right (VerticaStandardSqlStatement (CreateTableStmt (CreateTable a b c d e (TableColumns f (x:|(xs))) g))))


-- Extracts the starting and ending position in the create table statement of a column that is defined as a key
extractKeyConstraint :: Either a (VerticaStatement r Range) -> [(GHC.Int.Int64, GHC.Int.Int64)]
extractKeyConstraint (Right (VerticaStandardSqlStatement
                             (CreateTableStmt (CreateTable _ _ _ _ _ (TableColumns _ ((ColumnOrConstraintColumn (ColumnDefinition _ (QColumnName _ _ n) (PrimitiveDataType _ t _) _ _ _)):|[((ColumnOrConstraintConstraint (ConstraintDefinition (Range (Position _ start _) (Position _ end _)))))])) _)))) = [(start, end)]

extractKeyConstraint (Right (VerticaStandardSqlStatement
                             (CreateTableStmt (CreateTable _ _ _ _ _ (TableColumns _ ((ColumnOrConstraintConstraint (ConstraintDefinition (Range (Position _ start _) (Position _ end _)))):|[])) _)))) = [(start, end)]

extractKeyConstraint (Right (VerticaStandardSqlStatement
                             (CreateTableStmt (CreateTable a b c d e (TableColumns f ((ColumnOrConstraintColumn (ColumnDefinition _ (QColumnName _ _ n) (PrimitiveDataType _ t _) _ _ _)):|(x:xs))) g)))) = extractKeyConstraint (Right (VerticaStandardSqlStatement (CreateTableStmt (CreateTable a b c d e (TableColumns f (x:|(xs))) g))))

extractKeyConstraint (Right (VerticaStandardSqlStatement
                             (CreateTableStmt (CreateTable a b c d e (TableColumns f ((ColumnOrConstraintConstraint (ConstraintDefinition (Range (Position _ start _) (Position _ end _)))):|(x:xs))) g)))) = [(start, end)] ++ extractKeyConstraint (Right (VerticaStandardSqlStatement (CreateTableStmt (CreateTable a b c d e (TableColumns f (x:|(xs))) g))))


-- Extracts the name of a key column whose position is extracted from extractKeyConstraint function
extractKeyColumns :: [(GHC.Int.Int64, GHC.Int.Int64)] -> String -> [String]
extractKeyColumns positions sql_statement = keyColumns
  where keyColumns =
          map (\p -> L.unpack (L.replace ")" "" ((L.replace "(" "" (L.take ((((Tuple.snd p)-2)-(Tuple.fst p))+2) (L.drop (Tuple.fst p) (L.pack sql_statement))))))) positions


-- Extracts the name of the column that stores the value of a feature and is defined as double
extractDoubleColumns :: (Eq a1, IsString a1) => [(L.Text, a1)] -> L.Text
extractDoubleColumns columns = value_column
  where doubleColumns = ((filter (\c -> Tuple.snd c == "DOUBLE") columns))
        value_column =
          case (doubleColumns) of [x] -> (Tuple.fst (doubleColumns !! 0))
                                  [] -> (L.pack "1.0")

-- Constructs the case when part of the pivot query
constructCaseWhen :: [String] -> String -> L.Text -> [String]
constructCaseWhen feature_names featureNameColumn featureValueColumn =
  map (\f -> "SUM(CASE WHEN " ++  featureNameColumn ++ "='" ++ f ++ "'  THEN " ++  (L.unpack featureValueColumn) ++ " ELSE 0.0 END) AS " ++ (filter (/=' ') f) ++ "Value") feature_names


-- Constructs the sql pivot query end to end
constructPivotQuery :: [L.Text] -> [[String]] -> [String] -> [String] -> String -> String
constructPivotQuery statements names table_names column_name observations_table = pivot_query
  where table_statements = extractCreateTableStatements statements
        feature_table_statements = map (\n -> extractCreateTableStatementFromName n table_statements) table_names
        feature_table_statements_parsed = map (\st -> Database.Sql.Vertica.Parser.parse (st !! 0)) feature_table_statements
        statement_columns = map (\st -> extractColumnsFromTable st) feature_table_statements_parsed
        positions = map (\c -> ((extractKeyConstraint c))) feature_table_statements_parsed
        key_columns = map (\(st, p) -> (extractKeyColumns p (L.unpack (st !! 0)))) (zip feature_table_statements positions)
        double_columns = map (\sc -> extractDoubleColumns sc) statement_columns
        case_when = map (\(n, cn, dc) -> constructCaseWhen n cn dc) (zip3 names column_name double_columns)
      
        observation_table_statement = (extractCreateTableStatementFromName observations_table table_statements) !! 0
        observation_table_statement_parsed = Database.Sql.Vertica.Parser.parse observation_table_statement
        observation_statement_columns = extractColumnsFromTable observation_table_statement_parsed
        constraint_positions = (extractKeyConstraint observation_table_statement_parsed)
        observation_keys = map (\key -> observations_table ++ "." ++ key) (extractKeyColumns constraint_positions (L.unpack observation_table_statement))

        one_hot_enconded_features = List.intercalate "," (map (\l -> List.intercalate "," (map (\n -> (filter (/=' ') n) ++ "Value") l)) names)

        observation_keys_no_alias = map (\k -> (splitOn "." k) !! 1) observation_keys

        where_clause =
          List.intercalate " AND " (map (\(kl, t) -> List.intercalate " AND " (map (\key -> observations_table ++ "." ++ key ++ "=" ++ t ++ "_temp." ++ key) kl)) (zip [observation_keys_no_alias] table_names))
        
        pivot_subqueries =
          map (\(k, c, t) -> "(SELECT " ++ (List.intercalate "," k) ++ "," ++ (List.intercalate "," c) ++ " FROM " ++ t ++ " GROUP BY " ++ (List.intercalate ","  k) ++ ") AS " ++ t ++ "_temp") (zip3 [observation_keys_no_alias] case_when table_names)

        pivot_query =
          "SELECT " ++ (List.intercalate "," observation_keys) ++ "," ++ one_hot_enconded_features ++ " FROM " ++ observations_table ++ "," ++ (List.intercalate "," pivot_subqueries) ++ " WHERE " ++ where_clause ++ ";"


------------------------ End of functions for constructing the SQL query that pivots feature table ------------------------ 


-- Function that generates the entire python code, including both the connection to database and the tensorflow commands
endToEndTranslate :: [String] -> [String] -> [String] -> [[String]] -> String -> String -> String -> String -> String -> String -> [String] ->  M.Map [Char] [Integer] -> Int -> Double -> String
endToEndTranslate sql_statements feature_table_names feature_column_names feature_names observations_table labels_table_name objective_view database_user database_pass database_name variable_table_names variable_dimensions iterations step = tensorflow_code_total
  where sql_statements_text = map (\s -> L.pack s) sql_statements

        variable_tensors_initialization = map (\v -> initializeVariableTensors v feature_table_names variable_dimensions) variable_table_names

        variable_tensors_string = List.intercalate "\n" variable_tensors_initialization

        tensorflow_related_statements = filter (\st ->  (isCreateTableStatement (Database.Sql.Vertica.Parser.parse (L.pack st))) == False) sql_statements

        tensorflow_commands = map (\s -> (translateToTensorFlowCommand (L.pack s) feature_table_names variable_table_names feature_names)) tensorflow_related_statements

        tensorflow_commands_string = List.intercalate "\n" tensorflow_commands

        features_pivot_query = constructPivotQuery sql_statements_text feature_names feature_table_names feature_column_names observations_table

        labels_query = "SELECT * FROM " ++ labels_table_name ++ ";"

        objective_view_lowercase = L.unpack (L.toLower (L.pack objective_view))

        import_statements = "import numpy as np\nimport pandas as pd\nimport tensorflow as tf\nimport os"
        
        features_tensors_code =
          "os.system(\"mysql -u " ++ database_user ++ " -p" ++ database_pass ++ " " ++ database_name ++ " -e " ++ "\\\"" ++
          features_pivot_query ++ "\\\"" ++ " > features.csv" ++
          "\")\nfeatures_frame = pd.read_csv(\"features.csv\", sep='\\t')\nfeatures_dataset=features_frame.values\nfeatures_train=features_dataset[:,1:" ++
          show((List.length (concat feature_names)+1)) ++ "].astype(float)\nfeatures=tf.convert_to_tensor(features_train, np.double)"

        labels_tensors_code =
          "os.system(\"mysql -u " ++ database_user ++ " -p" ++ database_pass ++ " " ++ database_name ++ " -e " ++ "\\\"" ++ labels_query ++
          "\\\"" ++ " > labels.csv" ++ "\")\n" ++ "labels_frame = pd.read_csv(\"labels.csv\", sep='\\t')\n" ++
          "labels_dataset=labels_frame.values\n" ++ "labels_train=labels_dataset[:,1:2].astype(float).flatten()\n" ++
          labels_table_name ++ "=tf.convert_to_tensor(labels_train, np.double)"

        session_code =
          "optimizer = tf.train.GradientDescentOptimizer(" ++ show(step) ++ ")\ntrain = optimizer.minimize(" ++ objective_view_lowercase ++
          ")\nwith tf.Session() as session:\n \tsession.run(tf.global_variables_initializer())\n \tfor step in range(" ++
          show(iterations) ++ "):\n \t\tsession.run(train)\n \t\tprint(\"objective:\", session.run(" ++
          objective_view_lowercase ++ "))"

        tensorflow_code_total =
          import_statements ++ "\n" ++ features_tensors_code ++ "\n" ++ labels_tensors_code ++ "\n" ++ variable_tensors_string ++ "\n" ++
          tensorflow_commands_string ++ "\n" ++ session_code

