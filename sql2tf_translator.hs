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


{-identifyPragma decl =
  case decl of
    (In  (Rule (In  (Atom (Name "lang:solver:variable")
                            [(In _ (PredConst (Name p)))] []))
                (In _ (Conj [])))) ->
        Just (p)
    (In  (Rule  _)) -> Nothing
    (In  (P2P    _)) -> Nothing-}


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



main = do
  putStr "Starting translation"

  -- FAMA example
  -- writeFile "/home/nantia/interoperability_paper/tensorflow/generated_FAMA.py" (endToEndTranslate ["CREATE TABLE observations (observationID INT NOT NULL,PRIMARY KEY (observationID));","CREATE TABLE features (observationID INT,featureName VARCHAR(30),featureValue DOUBLE,FOREIGN KEY (observationID) REFERENCES observations(observationID));", "CREATE TABLE w0 (featureName VARCHAR(30),weightValue DOUBLE);", "CREATE TABLE weights (featureName VARCHAR(30),weightValue DOUBLE);", "CREATE TABLE weights_interaction (featureName VARCHAR(30),interactionRank INT,weightValue DOUBLE);", "CREATE TABLE targets (observationID INT,targetValue DOUBLE,FOREIGN KEY (observationID) REFERENCES features(observationID));", "CREATE VIEW w1agg AS SELECT SUM(features.featureValue * weights.weightValue) AS aggValue, observationID AS observationID FROM features, weights WHERE features.featureName = weights.featureName GROUP BY observationID;", "CREATE VIEW weight_interaction_squared AS SELECT POW(weights_interaction.weightValue, 2) AS weightValueSquared, featureName AS featureName, interactionRank AS interactionRank FROM weights_interaction;", "CREATE VIEW feature_squared AS SELECT POW(features.featureValue, 2) AS featureValueSquared, observationID AS observationID, featureName AS featureName FROM features;", "CREATE VIEW square_product AS SELECT observationID AS observationID, interactionRank AS interactionRank, SUM(feature_squared.featureValueSquared * weight_interaction_squared.weightValueSquared) AS productValue FROM feature_squared, weight_interaction_squared WHERE feature_squared.featureName = weight_interaction_squared.featureName GROUP BY feature_squared.observationID, weight_interaction_squared.interactionRank;","CREATE VIEW product AS SELECT observationID AS observationID, interactionRank AS interactionRank, SUM(weights_interaction.weightValue * features.featureValue) AS productValue FROM weights_interaction, features GROUP BY features.observationID, weights_interaction.interactionRank;","CREATE VIEW product_squared AS SELECT observationID AS observationID, interactionRank AS interactionRank, POW(product.productValue, 2) AS productValueSquared FROM product;", "CREATE VIEW diff AS SELECT product_squared.observationID AS observationID, product_squared.interactionRank AS interactionRank, (product_squared.productValueSquared - square_product.productValue) AS diffValue FROM product_squared, square_product WHERE product_squared.observationID = square_product.observationID AND product_squared.interactionRank = square_product.interactionRank;", "CREATE VIEW interactions AS SELECT observationID AS observationID, 0.5*SUM(diff.diffValue) as sumDiffValue FROM diff GROUP BY observationID;", "CREATE VIEW prediction AS SELECT interactions.observationID AS observationID, (w0.weightValue + w1agg.aggValue + interactions.sumDiffValue) AS predictionValue FROM w0, w1agg, interactions WHERE w1agg.observationID = interactions.observationID AND w0.featureName = \"f0\";", "CREATE VIEW errors AS SELECT prediction.predictionValue - targets.targetValue AS errorValue, prediction.observationID AS observationID FROM prediction, targets WHERE prediction.observationID = targets.observationID;", "CREATE VIEW squaredErrors AS SELECT POW(errors.errorValue, 2) AS squaredErrorValue, observationID AS observationID FROM errors;", "CREATE VIEW sumSquaredError AS SELECT AVG(squaredErrors.squaredErrorValue) FROM squaredErrors;"] ["features"] ["featureName"] [["LSTAT", "CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B"]] "observations" "targets" "sumSquaredError" "root" "cs061225" "bostonHousing" ["w0", "weights", "weights_interaction"] (M.fromList [("weights", [13]), ("w0", [1]), ("weights_interaction", [13,2])]) 1000 0.0000003)

  -- Linear Regression example
  --writeFile "/Users/Nantia/Documents/interoperability_paper/tensorflow/generated_LR.py" (endToEndTranslate ["CREATE TABLE observations (observationID VARCHAR(15),PRIMARY KEY (observationID));","CREATE TABLE features (observationID VARCHAR(15),featureName VARCHAR(30),featureValue DOUBLE,PRIMARY KEY (observationID, featureName));","CREATE TABLE weights (featureName VARCHAR(30),weightValue DOUBLE,PRIMARY KEY (featureName));","CREATE TABLE targets (observationID VARCHAR(15),targetValue DOUBLE,PRIMARY KEY (observationID));","CREATE VIEW predictions AS SELECT SUM(features.featureValue * weights.weightValue) AS predictionValue, observationID AS observationID FROM features, weights WHERE features.featureName = weights.featureName GROUP BY observationID;", "CREATE VIEW errors AS SELECT predictions.prediction - targets.targetValue AS errorValue, observationID AS observationID FROM predictions, targets WHERE predictions.observationID = targets.observationID;", "CREATE VIEW squaredErrors AS SELECT POW(errors.errorValue, 2) AS squaredErrorValue, observationID AS observationID FROM errors;", "CREATE VIEW sumSquaredError AS SELECT AVG(squaredErrors.squaredErrorValue) FROM squaredErrors;"] ["features"] ["featureName"] [["LSTAT", "CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B"]] "observations" "targets" "sumSquaredError" "root" "cs061225" "bostonHousing" ["weights"] (M.fromList [("weights", [13])]) 1000 0.0000003)

 -- Logistic Regression example
  writeFile "/Users/Nantia/Documents/interoperability_paper/tensorflow/generated_logistic.py" (endToEndTranslate ["CREATE TABLE observations (observationID INT,PRIMARY KEY (observationID));", "CREATE TABLE features (observationID INT,featureName VARCHAR(10),featureValue DOUBLE,PRIMARY KEY (observationID, featureName));", "CREATE TABLE weights (featureName VARCHAR(10),weightValue DOUBLE,PRIMARY KEY (featureName));","CREATE TABLE labels (observationID INT,labelValue DOUBLE,PRIMARY KEY (observationID));","CREATE VIEW product AS SELECT SUM(features.featureValue * weights.weightValue) AS productValue, features.observationID AS observationID FROM features, weights WHERE features.featureName = weights.featureName GROUP BY observationID;", "CREATE VIEW sigmoid AS SELECT product.observationID AS observationID, (1/(1+EXP(-product.productValue))) AS sigmoidValue FROM product;", "CREATE VIEW log_sigmoid AS SELECT sigmoid.observationID AS observationID, LN(sigmoid.sigmoidValue) AS logSigmoidValue FROM sigmoid;", "CREATE VIEW log2 AS SELECT sigmoid.observationID AS observationID, LN(1-sigmoid.sigmoidValue) AS log2Value FROM sigmoid;", "CREATE VIEW first_part AS SELECT labels.observationID AS observationID,(labels.labelValue * log_sigmoid.logSigmoidValue) AS firstPartValue FROM labels, log_sigmoid WHERE labels.observationID = log_sigmoid.observationID;", "CREATE VIEW second_part AS SELECT labels.observationID AS observationID, ((1-labels.labelValue) * log2.log2Value) AS secondPartValue FROM labels, log2 WHERE labels.observationID = log2.observationID;", "CREATE VIEW loss AS SELECT (-1)*SUM((first_part.firstPartValue + second_part.secondPartValue)) AS lossValue FROM first_part, second_part;"] ["features"] ["featureName"] [["f1", "f2", "f3", "f4"]] "observations" "labels" "loss" "root" "cs061225" "iris" ["weights"] (M.fromList [("weights", [4])]) 1000 0.01)



  -- Linear Regression with normalized feature tables example
  --writeFile "/home/nantia/interoperability_paper/tensorflow/generated_LR_brand_city.py" (endToEndTranslate ["CREATE TABLE skus (skuID VARCHAR(30) NOT NULL, PRIMARY KEY (skuID));", "CREATE TABLE stores (storeID VARCHAR(30) NOT NULL, PRIMARY KEY (storeID));", "CREATE TABLE dates (dateID VARCHAR(30) NOT NULL,PRIMARY KEY (dateID));", "CREATE TABLE observations (skuID VARCHAR(30) NOT NULL,storeID VARCHAR(30) NOT NULL,dateID VARCHAR(30) NOT NULL,FOREIGN KEY (skuID) REFERENCES skus(skuID),FOREIGN KEY (storeID) REFERENCES stores(storeID),FOREIGN KEY (dateID) REFERENCES dates(dateID));", "CREATE TABLE brands (skuID VARCHAR(30),brandName VARCHAR(30),FOREIGN KEY (skuID) REFERENCES skus(skuID));", "CREATE TABLE cities (storeID VARCHAR(30),cityName VARCHAR(30),FOREIGN KEY (storeID) REFERENCES stores(storeID));", "CREATE TABLE brandFeat (skuID VARCHAR(30),brandName VARCHAR(30),hotEncondingValue INT,FOREIGN KEY (skuID) REFERENCES skus(skuID));", "CREATE TABLE cityFeat (storeID VARCHAR(30),cityName VARCHAR(30),hotEncondingValue INT,FOREIGN KEY (storeID) REFERENCES stores(storeID));", "CREATE TABLE brandWeights (brandName VARCHAR(30),weightValue DOUBLE);", "CREATE TABLE cityWeights (cityName VARCHAR(30),weightValue DOUBLE);", "CREATE TABLE targets (skuID VARCHAR(30),storeID VARCHAR(30),dateID VARCHAR(30),targetValue DOUBLE,FOREIGN KEY (skuID) REFERENCES skus(skuID),FOREIGN KEY (storeID) REFERENCES stores(storeID),FOREIGN KEY (dateID) REFERENCES dates(dateID));", "CREATE VIEW predictions AS SELECT observations.skuID, observations.storeID, observations.dateID, ((brandFeat.hotEncondingValue * brandWeights.weightValue) + (cityFeat.hotEncondingValue * cityWeights.weightValue)) FROM observations, brandFeat, brandWeights, cityFeat, cityWeights, brands, cities WHERE brandFeat.brandName = brandWeights.brandName AND cityFeat.cityName = cityWeights.cityName AND brands.skuID = observations.skuID AND brands.brandName = brandFeat.brandName AND cities.storeID = observations.storeID AND cities.cityName = cityFeat.cityName AND brandFeat.skuID = observations.skuID AND cityFeat.storeID = observations.storeID;","CREATE VIEW errors AS SELECT predictions.prediction - targets.targetValue AS errorValue, observationID AS observationID FROM predictions, targets WHERE predictions.observationID = targets.observationID;", "CREATE VIEW squaredErrors AS SELECT POW(errors.errorValue, 2) AS squaredErrorValue, observationID AS observationID FROM errors;", "CREATE VIEW sumSquaredError AS SELECT AVG(squaredErrors.squaredErrorValue) FROM squaredErrors;"] ["brandFeat", "cityFeat"] ["brandName", "cityName"] [["sony", "philips"], ["Athens", "LA"]] "observations" "targets" "sumSquaredError" "root" "cs061225" "normalizedFeaturesExample" ["brandWeights", "cityWeights"] (M.fromList [("brandWeights", [2]), ("cityWeights", [2])]) 1000 0.0000003)

 -- Linear Regression based on Favorita (family, city)
  --writeFile "/home/nantia/interoperability_paper/tensorflow/generated_LR_item_city.py" (endToEndTranslate ["CREATE TABLE items (item_nbr VARCHAR(10), family VARCHAR(50), class VARCHAR(10), perishable NUMERIC, PRIMARY KEY (item_nbr));", "CREATE TABLE stores (store_nbr VARCHAR(10), city VARCHAR(30), state VARCHAR(50), type VARCHAR(2), cluster NUMERIC, PRIMARY KEY (store_nbr));", "CREATE TABLE observations (id NUMERIC, date VARCHAR(20), store_nbr VARCHAR(10), item_nbr VARCHAR(10), PRIMARY KEY (id), FOREIGN KEY (item_nbr) REFERENCES items(item_nbr),FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));", "CREATE TABLE familyFeat (item_nbr VARCHAR(10), family VARCHAR(50),hotEncondingValue INT,FOREIGN KEY (item_nbr) REFERENCES items(item_nbr));", "CREATE TABLE cityFeat (store_nbr VARCHAR(10),city VARCHAR(30),hotEncondingValue INT,FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));", "CREATE TABLE familyWeights (family VARCHAR(50),weightValue DOUBLE);", "CREATE TABLE cityWeights (cityName VARCHAR(30),weightValue DOUBLE);", "CREATE TABLE targets (id NUMERIC, unit_sales FLOAT, PRIMARY KEY (id));", "CREATE VIEW predictions AS SELECT observations.id, observations.item_nbr, observations.store_nbr, observations.date, ((familyFeat.hotEncondingValue * familyWeights.weightValue) + (cityFeat.hotEncondingValue * cityWeights.weightValue)) AS prediction FROM observations, familyFeat, familyWeights, cityFeat, cityWeights, items, stores WHERE familyFeat.family = familyWeights.family AND cityFeat.city = cityWeights.city AND familyFeat.item_nbr = observations.item_nbr AND items.family = familyFeat.family AND cities.store_nbr = observations.store_nbr AND stores.city = cityFeat.city;","CREATE VIEW errors AS SELECT predictions.prediction - targets.unit_sales AS errorValue, id AS id FROM predictions, targets WHERE predictions.id = targets.id;", "CREATE VIEW squaredErrors AS SELECT POW(errors.errorValue, 2) AS squaredErrorValue, id AS id FROM errors;", "CREATE VIEW sumSquaredError AS SELECT AVG(squaredErrors.squaredErrorValue) FROM squaredErrors;"] ["familyFeat", "cityFeat"] ["family", "city"] [["BREADBAKERY","FROZEN FOODS","PREPARED FOODS","DELI","SCHOOL AND OFFICE SUPPLIES","HOME AND KITCHEN II","DAIRY","PET SUPPLIES","HOME APPLIANCES","LADIESWEAR","MEATS","LINGERIE","HOME AND KITCHEN I","MAGAZINES","PLAYERS AND ELECTRONICS","AUTOMOTIVE","POULTRY","GROCERY II","HARDWARE","EGGS","HOME CARE","PERSONAL CARE","LAWN AND GARDEN","SEAFOOD","CLEANING","LIQUORWINEBEER","GROCERY I","BEAUTY","BOOKS","BABY CARE","PRODUCE","BEVERAGES","CELEBRATION"], ["Ibarra","Ambato","Santo Domingo","Cuenca","Latacunga","Salinas","Babahoyo","Riobamba","Daule","Guaranda","Quito","Manta","Puyo","Cayambe","Machala","Loja","Playas","Guayaquil","Esmeraldas","El Carmen","Quevedo","Libertad"]] "observations" "targets" "sumSquaredError" "root" "cs061225" "favorita" ["familyWeights", "cityWeights"] (M.fromList [("familyWeights", [33]), ("cityWeights", [22])]) 1000 0.0000003)

  -- Linear Regression based on Favorita (family)
  --writeFile "/home/nantia/interoperability_paper/tensorflow/generated_LR_item_city.py" (endToEndTranslate ["CREATE TABLE items (item_nbr VARCHAR(10), family VARCHAR(50), class VARCHAR(10), perishable NUMERIC, PRIMARY KEY (item_nbr));", "CREATE TABLE stores (store_nbr VARCHAR(10), city VARCHAR(30), state VARCHAR(50), type VARCHAR(2), cluster NUMERIC, PRIMARY KEY (store_nbr));", "CREATE TABLE observations (id NUMERIC, date VARCHAR(20), store_nbr VARCHAR(10), item_nbr VARCHAR(10), PRIMARY KEY (id), FOREIGN KEY (item_nbr) REFERENCES items(item_nbr),FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));", "CREATE TABLE familyFeat (item_nbr VARCHAR(10), family VARCHAR(50),hotEncondingValue INT,FOREIGN KEY (item_nbr) REFERENCES items(item_nbr));", "CREATE TABLE familyWeights (family VARCHAR(50),weightValue DOUBLE);", "CREATE TABLE targets (id NUMERIC, unit_sales FLOAT, PRIMARY KEY (id));", "CREATE VIEW predictions AS SELECT observations.id, observations.item_nbr, observations.store_nbr, observations.date, ((familyFeat.hotEncondingValue * familyWeights.weightValue)) AS prediction FROM observations, familyFeat, familyWeights, items WHERE familyFeat.family = familyWeights.family AND familyFeat.item_nbr = observations.item_nbr AND items.family = familyFeat.family GROUP BY observations.id;","CREATE VIEW errors AS SELECT predictions.prediction - targets.unit_sales AS errorValue, id AS id FROM predictions, targets WHERE predictions.id = targets.id;", "CREATE VIEW squaredErrors AS SELECT POW(errors.errorValue, 2) AS squaredErrorValue, id AS id FROM errors;", "CREATE VIEW sumSquaredError AS SELECT AVG(squaredErrors.squaredErrorValue) FROM squaredErrors;"] ["familyFeat"] ["family"] [["BREADBAKERY","FROZEN FOODS","PREPARED FOODS","DELI","SCHOOL AND OFFICE SUPPLIES","HOME AND KITCHEN II","DAIRY","PET SUPPLIES","HOME APPLIANCES","LADIESWEAR","MEATS","LINGERIE","HOME AND KITCHEN I","MAGAZINES","PLAYERS AND ELECTRONICS","AUTOMOTIVE","POULTRY","GROCERY II","HARDWARE","EGGS","HOME CARE","PERSONAL CARE","LAWN AND GARDEN","SEAFOOD","CLEANING","LIQUORWINEBEER","GROCERY I","BEAUTY","BOOKS","BABY CARE","PRODUCE","BEVERAGES","CELEBRATION"]] "observations" "targets" "sumSquaredError" "root" "cs061225" "favorita" ["familyWeights"] (M.fromList [("familyWeights", [33])]) 1000 0.0000003)

  -- Linear Regression based on Favorita (family, city, state, type)
  --writeFile "/home/nantia/interoperability_paper/tensorflow/generated_LR_item_city_state_type.py" (endToEndTranslate ["CREATE TABLE items (item_nbr VARCHAR(10), family VARCHAR(50), class VARCHAR(10), perishable NUMERIC, PRIMARY KEY (item_nbr));", "CREATE TABLE stores (store_nbr VARCHAR(10), city VARCHAR(30), state VARCHAR(50), type VARCHAR(2), cluster NUMERIC, PRIMARY KEY (store_nbr));", "CREATE TABLE observations (id NUMERIC, date VARCHAR(20), store_nbr VARCHAR(10), item_nbr VARCHAR(10), PRIMARY KEY (id), FOREIGN KEY (item_nbr) REFERENCES items(item_nbr),FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));", "CREATE TABLE familyFeat (item_nbr VARCHAR(10), family VARCHAR(50),hotEncondingValue INT,FOREIGN KEY (item_nbr) REFERENCES items(item_nbr));", "CREATE TABLE cityFeat (store_nbr VARCHAR(10),city VARCHAR(30),hotEncondingValue INT,FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));", "CREATE TABLE stateFeat (store_nbr VARCHAR(10),state VARCHAR(50),hotEncondingValue INT,FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));","CREATE TABLE typeFeat (store_nbr VARCHAR(10),type VARCHAR(2),hotEncondingValue INT,FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));","CREATE TABLE familyWeights (family VARCHAR(50),weightValue DOUBLE);", "CREATE TABLE cityWeights (city VARCHAR(30),weightValue DOUBLE);", "CREATE TABLE stateWeights (state VARCHAR(50),weightValue DOUBLE);","CREATE TABLE typeWeights (type VARCHAR(2),weightValue DOUBLE);","CREATE TABLE targets (id NUMERIC, unit_sales FLOAT, PRIMARY KEY (id));", "CREATE VIEW predictions AS SELECT observations.id, observations.item_nbr, observations.store_nbr, observations.date, ((familyFeat.hotEncondingValue * familyWeights.weightValue) + (cityFeat.hotEncondingValue * cityWeights.weightValue) + (stateFeat.hotEncondingValue * stateWeights.weightValue) + (typeFeat.hotEncondingValue * typeWeights.weightValue)) AS prediction FROM observations, familyFeat, familyWeights, cityFeat, cityWeights, stateFeat, stateWeights, typeFeat, typeWeights, items, stores WHERE familyFeat.family = familyWeights.family AND cityFeat.city = cityWeights.city AND stateFeat.state = stateWeights.state AND typeFeat.type = typeWeights.type AND familyFeat.item_nbr = observations.item_nbr AND items.family = familyFeat.family AND cities.store_nbr = observations.store_nbr AND stores.city = cityFeat.city AND stores.state=stateFeat.state AND stores.type=typeFeat.type;","CREATE VIEW errors AS SELECT predictions.prediction - targets.unit_sales AS errorValue, id AS id FROM predictions, targets WHERE predictions.id = targets.id;", "CREATE VIEW squaredErrors AS SELECT POW(errors.errorValue, 2) AS squaredErrorValue, id AS id FROM errors;", "CREATE VIEW sumSquaredError AS SELECT AVG(squaredErrors.squaredErrorValue) FROM squaredErrors;"] ["familyFeat", "cityFeat", "stateFeat", "typeFeat"] ["family", "city", "state", "type"] [["BREADBAKERY","FROZEN FOODS","PREPARED FOODS","DELI","SCHOOL AND OFFICE SUPPLIES","HOME AND KITCHEN II","DAIRY","PET SUPPLIES","HOME APPLIANCES","LADIESWEAR","MEATS","LINGERIE","HOME AND KITCHEN I","MAGAZINES","PLAYERS AND ELECTRONICS","AUTOMOTIVE","POULTRY","GROCERY II","HARDWARE","EGGS","HOME CARE","PERSONAL CARE","LAWN AND GARDEN","SEAFOOD","CLEANING","LIQUORWINEBEER","GROCERY I","BEAUTY","BOOKS","BABY CARE","PRODUCE","BEVERAGES","CELEBRATION"], ["Ibarra","Ambato","Santo Domingo","Cuenca","Latacunga","Salinas","Babahoyo","Riobamba","Daule","Guaranda","Quito","Manta","Puyo","Cayambe","Machala","Loja","Playas","Guayaquil","Esmeraldas","El Carmen","Quevedo","Libertad"], ["Esmeraldas","Manabi","Los Rios","El Oro","Guayas","Santo Domingo de los Tsachilas","Imbabura","Santa Elena","Chimborazo","Pastaza","Bolivar","Loja","Azuay","Cotopaxi","Tungurahua","Pichincha"], ["E","D","A","C","B"]] "observations" "targets" "sumSquaredError" "root" "cs061225" "favorita" ["familyWeights", "cityWeights", "stateWeights", "typeWeights"] (M.fromList [("familyWeights", [33]), ("cityWeights", [22]), ("stateWeights", [16]), ("typeWeights", [5])]) 1000 0.0000003)

  -- Linear Regression based on Favorita (family, city, state, type, perishable, cluster) - throws error
  -- writeFile "/home/nantia/interoperability_paper/tensorflow/generated_LR_item_city_state_type_perish_cluster.py" (endToEndTranslate ["CREATE TABLE items (item_nbr VARCHAR(10), family VARCHAR(50), class VARCHAR(10), perishable NUMERIC, PRIMARY KEY (item_nbr));", "CREATE TABLE stores (store_nbr VARCHAR(10), city VARCHAR(30), state VARCHAR(50), type VARCHAR(2), cluster NUMERIC, PRIMARY KEY (store_nbr));", "CREATE TABLE observations (id NUMERIC, date VARCHAR(20), store_nbr VARCHAR(10), item_nbr VARCHAR(10), PRIMARY KEY (id), FOREIGN KEY (item_nbr) REFERENCES items(item_nbr),FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));", "CREATE TABLE familyFeat (item_nbr VARCHAR(10), family VARCHAR(50),hotEncondingValue INT,FOREIGN KEY (item_nbr) REFERENCES items(item_nbr));", "CREATE TABLE cityFeat (store_nbr VARCHAR(10),city VARCHAR(30),hotEncondingValue INT,FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));", "CREATE TABLE stateFeat (store_nbr VARCHAR(10),state VARCHAR(50),hotEncondingValue INT,FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));","CREATE TABLE typeFeat (store_nbr VARCHAR(10),type VARCHAR(2),hotEncondingValue INT,FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));","CREATE TABLE perishableFeat (item_nbr VARCHAR(10),perishable DOUBLE,FOREIGN KEY (item_nbr) REFERENCES stores(item_nbr));","CREATE TABLE clusterFeat (store_nbr VARCHAR(10),cluster DOUBLE, FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));","CREATE TABLE familyWeights (family VARCHAR(50),weightValue DOUBLE);", "CREATE TABLE cityWeights (city VARCHAR(30),weightValue DOUBLE);", "CREATE TABLE stateWeights (state VARCHAR(50),weightValue DOUBLE);","CREATE TABLE typeWeights (type VARCHAR(2),weightValue DOUBLE);","CREATE TABLE perishableWeights (weightValue DOUBLE);","CREATE TABLE clusterWeights (weightValue DOUBLE);","CREATE TABLE targets (id NUMERIC, unit_sales FLOAT, PRIMARY KEY (id));", "CREATE VIEW predictions AS SELECT observations.id, observations.item_nbr, observations.store_nbr, observations.date, ((familyFeat.hotEncondingValue * familyWeights.weightValue) + (cityFeat.hotEncondingValue * cityWeights.weightValue) + (stateFeat.hotEncondingValue * stateWeights.weightValue) + (typeFeat.hotEncondingValue * typeWeights.weightValue) + (perishableFeat.perishable * perishableWeights.weightValue) + (clusterFeat.cluster * clusterWeights.weightValue)) AS prediction FROM observations, familyFeat, familyWeights, cityFeat, cityWeights, stateFeat, stateWeights, typeFeat, typeWeights, perishableFeat, perishableWeights, clusterFeat, clusterWeights, items, stores WHERE familyFeat.family = familyWeights.family AND cityFeat.city = cityWeights.city AND stateFeat.state = stateWeights.state AND typeFeat.type = typeWeights.type AND familyFeat.item_nbr = observations.item_nbr AND items.family = familyFeat.family AND cities.store_nbr = observations.store_nbr AND stores.city = cityFeat.city AND stores.state=stateFeat.state AND stores.type=typeFeat.type AND perishableFeat.item_nbr=observations.item_nbr AND clusterFeat.store_nbr=observations.store_nbr;","CREATE VIEW errors AS SELECT predictions.prediction - targets.unit_sales AS errorValue, id AS id FROM predictions, targets WHERE predictions.id = targets.id;", "CREATE VIEW squaredErrors AS SELECT POW(errors.errorValue, 2) AS squaredErrorValue, id AS id FROM errors;", "CREATE VIEW sumSquaredError AS SELECT AVG(squaredErrors.squaredErrorValue) FROM squaredErrors;"] ["familyFeat", "cityFeat", "stateFeat", "typeFeat", "perishableFeat", "clusterFeat"] ["family", "city", "state", "type", "perishable", "cluster"] [["BREADBAKERY","FROZEN FOODS","PREPARED FOODS","DELI","SCHOOL AND OFFICE SUPPLIES","HOME AND KITCHEN II","DAIRY","PET SUPPLIES","HOME APPLIANCES","LADIESWEAR","MEATS","LINGERIE","HOME AND KITCHEN I","MAGAZINES","PLAYERS AND ELECTRONICS","AUTOMOTIVE","POULTRY","GROCERY II","HARDWARE","EGGS","HOME CARE","PERSONAL CARE","LAWN AND GARDEN","SEAFOOD","CLEANING","LIQUORWINEBEER","GROCERY I","BEAUTY","BOOKS","BABY CARE","PRODUCE","BEVERAGES","CELEBRATION"], ["Ibarra","Ambato","Santo Domingo","Cuenca","Latacunga","Salinas","Babahoyo","Riobamba","Daule","Guaranda","Quito","Manta","Puyo","Cayambe","Machala","Loja","Playas","Guayaquil","Esmeraldas","El Carmen","Quevedo","Libertad"], ["Esmeraldas","Manabi","Los Rios","El Oro","Guayas","Santo Domingo de los Tsachilas","Imbabura","Santa Elena","Chimborazo","Pastaza","Bolivar","Loja","Azuay","Cotopaxi","Tungurahua","Pichincha"], ["E","D","A","C","B"]] "observations" "targets" "sumSquaredError" "root" "cs061225" "favorita" ["familyWeights", "cityWeights", "stateWeights", "typeWeights", "perishableWeights", "clusterWeights"] (M.fromList [("familyWeights", [33]), ("cityWeights", [22]), ("stateWeights", [16]), ("typeWeights", [5]), ("perishableWeights", [1]), ("clusterWeights", [1])]) 1000 0.0000003)


  -- Linear Regression based on Favorita (family, city, state, type, holtype, locale, locale_name)
   --writeFile "/home/nantia/interoperability_paper/tensorflow/generated_LR_item_city_state_type.py" (endToEndTranslate ["CREATE TABLE items (item_nbr VARCHAR(10), family VARCHAR(50), class VARCHAR(10), perishable NUMERIC, PRIMARY KEY (item_nbr));", "CREATE TABLE stores (store_nbr VARCHAR(10), city VARCHAR(30), state VARCHAR(50), type VARCHAR(2), cluster NUMERIC, PRIMARY KEY (store_nbr));", "CREATE TABLE holidays (date VARCHAR(20), type VARCHAR(20), locale VARCHAR(20), locale_name VARCHAR(30), description VARCHAR(100), transferred boolean);","CREATE TABLE observations (id NUMERIC, date VARCHAR(20), store_nbr VARCHAR(10), item_nbr VARCHAR(10), PRIMARY KEY (id), FOREIGN KEY (item_nbr) REFERENCES items(item_nbr),FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));", "CREATE TABLE familyFeat (item_nbr VARCHAR(10), family VARCHAR(50),hotEncondingValue INT,FOREIGN KEY (item_nbr) REFERENCES items(item_nbr));", "CREATE TABLE cityFeat (store_nbr VARCHAR(10),city VARCHAR(30),hotEncondingValue INT,FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));", "CREATE TABLE stateFeat (store_nbr VARCHAR(10),state VARCHAR(50),hotEncondingValue INT,FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));","CREATE TABLE typeFeat (store_nbr VARCHAR(10),type VARCHAR(2),hotEncondingValue INT,FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));","CREATE TABLE holTypeFeat (date VARCHAR(20), type VARCHAR(20),hotEncondingValue INT,FOREIGN KEY (date) REFERENCES holidays(date));","CREATE TABLE localeFeat (date VARCHAR(20), locale VARCHAR(20),hotEncondingValue INT,FOREIGN KEY (date) REFERENCES holidays(date));","CREATE TABLE localeNameFeat (date VARCHAR(20), locale_name VARCHAR(20),hotEncondingValue INT,FOREIGN KEY (date) REFERENCES holidays(date));","CREATE TABLE familyWeights (family VARCHAR(50),weightValue DOUBLE);", "CREATE TABLE cityWeights (city VARCHAR(30),weightValue DOUBLE);", "CREATE TABLE stateWeights (state VARCHAR(50),weightValue DOUBLE);","CREATE TABLE typeWeights (type VARCHAR(2),weightValue DOUBLE);","CREATE TABLE holTypeWeights (type VARCHAR(20),weightValue DOUBLE);","CREATE TABLE localeWeights (locale VARCHAR(20),weightValue DOUBLE);","CREATE TABLE ocaleNameWeights (locale_name VARCHAR(30),weightValue DOUBLE);","CREATE TABLE targets (id NUMERIC, unit_sales FLOAT, PRIMARY KEY (id));", "CREATE VIEW predictions AS SELECT observations.id, observations.item_nbr, observations.store_nbr, observations.date, ((familyFeat.hotEncondingValue * familyWeights.weightValue) + (cityFeat.hotEncondingValue * cityWeights.weightValue) + (stateFeat.hotEncondingValue * stateWeights.weightValue) + (typeFeat.hotEncondingValue * typeWeights.weightValue) + (holTypeFeat.hotEncondingValue * holTypeWeights.weightValue) + (LocaleFeat.hotEncondingValue * LocaleWeights.weightValue) + (LocaleNameFeat.hotEncondingValue * LocaleWeights.weightValue)) AS prediction FROM observations, familyFeat, familyWeights, cityFeat, cityWeights, stateFeat, stateWeights, typeFeat, typeWeights, holTypeFeat, holTypeWeights, LocaleFeat, LocaleWeights, LocaleNameFeat, LocaleNameWeights, items, stores WHERE familyFeat.family = familyWeights.family AND cityFeat.city = cityWeights.city AND stateFeat.state = stateWeights.state AND typeFeat.type = typeWeights.type AND familyFeat.item_nbr = observations.item_nbr AND items.family = familyFeat.family AND cities.store_nbr = observations.store_nbr AND stores.city = cityFeat.city AND stores.state=stateFeat.state AND stores.type=typeFeat.type AND holTypeFeat.type=holTypeWeights.type AND LocaleFeat.locale=LocaleWeights.locale AND localeNameFeat.locale_name=localeNameWeights.locale_name AND holidays.date=observations.date;","CREATE VIEW errors AS SELECT predictions.prediction - targets.unit_sales AS errorValue, id AS id FROM predictions, targets WHERE predictions.id = targets.id;", "CREATE VIEW squaredErrors AS SELECT POW(errors.errorValue, 2) AS squaredErrorValue, id AS id FROM errors;", "CREATE VIEW sumSquaredError AS SELECT AVG(squaredErrors.squaredErrorValue) FROM squaredErrors;"] ["familyFeat", "cityFeat", "stateFeat", "typeFeat", "holTypeFeat", "localeFeat", "localeNameFeat"] ["family", "city", "state", "type", "type", "locale", "localeName"] [["BREADBAKERY","FROZEN FOODS","PREPARED FOODS","DELI","SCHOOL AND OFFICE SUPPLIES","HOME AND KITCHEN II","DAIRY","PET SUPPLIES","HOME APPLIANCES","LADIESWEAR","MEATS","LINGERIE","HOME AND KITCHEN I","MAGAZINES","PLAYERS AND ELECTRONICS","AUTOMOTIVE","POULTRY","GROCERY II","HARDWARE","EGGS","HOME CARE","PERSONAL CARE","LAWN AND GARDEN","SEAFOOD","CLEANING","LIQUORWINEBEER","GROCERY I","BEAUTY","BOOKS","BABY CARE","PRODUCE","BEVERAGES","CELEBRATION"], ["Ibarra","Ambato","Santo Domingo","Cuenca","Latacunga","Salinas","Babahoyo","Riobamba","Daule","Guaranda","Quito","Manta","Puyo","Cayambe","Machala","Loja","Playas","Guayaquil","Esmeraldas","El Carmen","Quevedo","Libertad"], ["Esmeraldas","Manabi","Los Rios","El Oro","Guayas","Santo Domingo de los Tsachilas","Imbabura","Santa Elena","Chimborazo","Pastaza","Bolivar","Loja","Azuay","Cotopaxi","Tungurahua","Pichincha"], ["E","D","A","C","B"], ["Holiday","Event","Bridge","Work Day","Additional","Transfer"], ["Regional","Local","National"], ["Esmeraldas","Puyo","Cayambe","Ibarra","Ecuador","Quito","Santo Domingo de los Tsachilas","Santo Domingo","Riobamba","Machala","Imbabura","Santa Elena","Quevedo","Libertad","Cuenca","Loja","Latacunga","Manta","Ambato","Cotopaxi","Guayaquil","El Carmen","Guaranda","Salinas"]] "observations" "targets" "sumSquaredError" "root" "cs061225" "favorita" ["familyWeights", "cityWeights", "stateWeights", "typeWeights", "holTypeWeights", "localeWeights", "localeNameWeights"] (M.fromList [("familyWeights", [33]), ("cityWeights", [22]), ("stateWeights", [16]), ("typeWeights", [5]),("holTypeWeights", [6]), ("localeWeights", [3]), ("localeNameWeights", [24])]) 1000 0.0000003)




  -- Linear Regression based on Favorita (family, city, state, type, holtype, locale, locale_name, class)
  --writeFile "/home/nantia/interoperability_paper/tensorflow/generated_LR_item_city_state_type.py" (endToEndTranslate ["CREATE TABLE items (item_nbr VARCHAR(10), family VARCHAR(50), class VARCHAR(10), perishable NUMERIC, PRIMARY KEY (item_nbr));", "CREATE TABLE stores (store_nbr VARCHAR(10), city VARCHAR(30), state VARCHAR(50), type VARCHAR(2), cluster NUMERIC, PRIMARY KEY (store_nbr));", "CREATE TABLE holidays (date VARCHAR(20), type VARCHAR(20), locale VARCHAR(20), locale_name VARCHAR(30), description VARCHAR(100), transferred boolean);","CREATE TABLE observations (id NUMERIC, date VARCHAR(20), store_nbr VARCHAR(10), item_nbr VARCHAR(10), PRIMARY KEY (id), FOREIGN KEY (item_nbr) REFERENCES items(item_nbr),FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));", "CREATE TABLE familyFeat (item_nbr VARCHAR(10), family VARCHAR(50),hotEncondingValue INT,FOREIGN KEY (item_nbr) REFERENCES items(item_nbr));", "CREATE TABLE classFeat (item_nbr VARCHAR(10), class VARCHAR(10),hotEncondingValue INT,FOREIGN KEY (item_nbr) REFERENCES items(item_nbr));","CREATE TABLE cityFeat (store_nbr VARCHAR(10),city VARCHAR(30),hotEncondingValue INT,FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));", "CREATE TABLE stateFeat (store_nbr VARCHAR(10),state VARCHAR(50),hotEncondingValue INT,FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));","CREATE TABLE typeFeat (store_nbr VARCHAR(10),type VARCHAR(2),hotEncondingValue INT,FOREIGN KEY (store_nbr) REFERENCES stores(store_nbr));","CREATE TABLE holTypeFeat (date VARCHAR(20), type VARCHAR(20),hotEncondingValue INT,FOREIGN KEY (date) REFERENCES holidays(date));","CREATE TABLE localeFeat (date VARCHAR(20), locale VARCHAR(20),hotEncondingValue INT,FOREIGN KEY (date) REFERENCES holidays(date));","CREATE TABLE localeNameFeat (date VARCHAR(20), locale_name VARCHAR(20),hotEncondingValue INT,FOREIGN KEY (date) REFERENCES holidays(date));","CREATE TABLE familyWeights (family VARCHAR(50),weightValue DOUBLE);", "CREATE TABLE classWeights (class VARCHAR(10),weightValue DOUBLE);","CREATE TABLE cityWeights (city VARCHAR(30),weightValue DOUBLE);", "CREATE TABLE stateWeights (state VARCHAR(50),weightValue DOUBLE);","CREATE TABLE typeWeights (type VARCHAR(2),weightValue DOUBLE);","CREATE TABLE holTypeWeights (type VARCHAR(20),weightValue DOUBLE);","CREATE TABLE localeWeights (locale VARCHAR(20),weightValue DOUBLE);","CREATE TABLE ocaleNameWeights (locale_name VARCHAR(30),weightValue DOUBLE);","CREATE TABLE targets (id NUMERIC, unit_sales FLOAT, PRIMARY KEY (id));", "CREATE VIEW predictions AS SELECT observations.id, observations.item_nbr, observations.store_nbr, observations.date, ((familyFeat.hotEncondingValue * familyWeights.weightValue) + (cityFeat.hotEncondingValue * cityWeights.weightValue) + (stateFeat.hotEncondingValue * stateWeights.weightValue) + (typeFeat.hotEncondingValue * typeWeights.weightValue) + (holTypeFeat.hotEncondingValue * holTypeWeights.weightValue) + (LocaleFeat.hotEncondingValue * LocaleWeights.weightValue) + (LocaleNameFeat.hotEncondingValue * LocaleNameWeights.weightValue) + (classFeat.hotEncondingValue * classWeights.weightValue)) AS prediction FROM observations, familyFeat, familyWeights, cityFeat, cityWeights, stateFeat, stateWeights, typeFeat, typeWeights, holTypeFeat, holTypeWeights, LocaleFeat, LocaleWeights, LocaleNameFeat, LocaleNameWeights, classFeat, classWeights, items, stores WHERE familyFeat.family = familyWeights.family AND classFeat.class = classWeights.class AND cityFeat.city = cityWeights.city AND stateFeat.state = stateWeights.state AND typeFeat.type = typeWeights.type AND familyFeat.item_nbr = observations.item_nbr AND items.family = familyFeat.family AND cities.store_nbr = observations.store_nbr AND stores.city = cityFeat.city AND stores.state=stateFeat.state AND stores.type=typeFeat.type AND holTypeFeat.type=holTypeWeights.type AND LocaleFeat.locale=LocaleWeights.locale AND localeNameFeat.locale_name=localeNameWeights.locale_name AND holidays.date=observations.date;","CREATE VIEW errors AS SELECT predictions.prediction - targets.unit_sales AS errorValue, id AS id FROM predictions, targets WHERE predictions.id = targets.id;", "CREATE VIEW squaredErrors AS SELECT POW(errors.errorValue, 2) AS squaredErrorValue, id AS id FROM errors;", "CREATE VIEW sumSquaredError AS SELECT AVG(squaredErrors.squaredErrorValue) FROM squaredErrors;"] ["familyFeat", "cityFeat", "stateFeat", "typeFeat", "holTypeFeat", "localeFeat", "localeNameFeat", "classFeat"] ["family", "city", "state", "type", "type", "locale", "localeName", "class"] [["BREADBAKERY","FROZEN FOODS","PREPARED FOODS","DELI","SCHOOL AND OFFICE SUPPLIES","HOME AND KITCHEN II","DAIRY","PET SUPPLIES","HOME APPLIANCES","LADIESWEAR","MEATS","LINGERIE","HOME AND KITCHEN I","MAGAZINES","PLAYERS AND ELECTRONICS","AUTOMOTIVE","POULTRY","GROCERY II","HARDWARE","EGGS","HOME CARE","PERSONAL CARE","LAWN AND GARDEN","SEAFOOD","CLEANING","LIQUORWINEBEER","GROCERY I","BEAUTY","BOOKS","BABY CARE","PRODUCE","BEVERAGES","CELEBRATION"], ["Ibarra","Ambato","Santo Domingo","Cuenca","Latacunga","Salinas","Babahoyo","Riobamba","Daule","Guaranda","Quito","Manta","Puyo","Cayambe","Machala","Loja","Playas","Guayaquil","Esmeraldas","El Carmen","Quevedo","Libertad"], ["Esmeraldas","Manabi","Los Rios","El Oro","Guayas","Santo Domingo de los Tsachilas","Imbabura","Santa Elena","Chimborazo","Pastaza","Bolivar","Loja","Azuay","Cotopaxi","Tungurahua","Pichincha"], ["E","D","A","C","B"], ["Holiday","Event","Bridge","Work Day","Additional","Transfer"], ["Regional","Local","National"], ["Esmeraldas","Puyo","Cayambe","Ibarra","Ecuador","Quito","Santo Domingo de los Tsachilas","Santo Domingo","Riobamba","Machala","Imbabura","Santa Elena","Quevedo","Libertad","Cuenca","Loja","Latacunga","Manta","Ambato","Cotopaxi","Guayaquil","El Carmen","Guaranda","Salinas"], ["3015","1026","1318","6352","4250","2024","1046","3104","1115","2806","4228","2116","1148","4122","2156","1320","6322","1040","2966","4254","2032","6328","6338","3034","5192","3102","4130","4255","1124","2636","6412","7002","1058","2650","5250","2904","2642","6393","1084","1154","4162","2420","2010","1330","1064","2306","1004","3020","2238","2956","1079","1066","6517","4114","1045","2756","1036","3108","2130","1074","3024","4140","1050","1033","2302","2662","6482","2340","7016","5244","3090","6329","6918","1087","1078","1028","4214","3022","1025","6230","2954","6706","2172","2854","1236","1042","3110","2864","3038","2304","2504","4141","2644","4198","2782","1093","2166","1153","2716","1122","1114","2226","6824","2108","2008","1314","6954","6414","2012","6258","2690","1092","2152","1370","3060","6206","2712","1039","2720","6426","2104","2026","2070","1008","2246","3012","1083","6448","2034","6242","2632","1334","2174","2646","1306","1190","1044","1156","2103","1029","5324","2214","1034","1328","4139","1060","6810","1116","1316","1364","2014","2750","2016","2970","2986","2506","3040","2630","3030","1082","1380","1142","2102","2242","1067","6233","2168","1022","1024","2906","1138","2112","1035","6404","4118","1070","3046","4126","1003","1056","6808","1072","1048","1062","1386","2164","7034","3026","4104","4222","1336","2124","6222","6022","1002","6266","2220","3044","1052","2210","4138","2030","2178","1038","1010","3032","7780","2122","1086","1041","1016","6248","1054","6922","2752","3029","3004","3010","3018","2114","6260","6301","2714","5322","6353","5224","6269","1005","2722","2708","2372","1032","2022","1302","2004","1030","2018","6155","1312","1073","1338","6232","3106","4252","4210","2960","2654","6238","1068","2786","5325","6262","1126","2228","1076","2652","1075","6267","6516","1080","3028","6253","5308","6212","4176","1063","6350","2664","6241","6246","2980","2718","1027","1132","1088","2222","2704","3008","2170","2412","2028","1077","1096","2784","6848","1089","5222","3035","1094","1013","6960","2640","1144","1018","6344","1326","2850","1152","1012","2002","1014","1146","6330","2162","1150","2020","6257","2218","6924","2142","1118","2802","6223","1120","6392","1006","2416"]] "observations" "targets" "sumSquaredError" "root" "cs061225" "favorita" ["familyWeights", "cityWeights", "stateWeights", "typeWeights", "holTypeWeights", "localeWeights", "localeNameWeights", "classWeights"] (M.fromList [("familyWeights", [33]), ("cityWeights", [22]), ("stateWeights", [16]), ("typeWeights", [5]),("holTypeWeights", [6]), ("localeWeights", [3]), ("localeNameWeights", [24]), ("classWeights", [337])]) 1000 0.0000003)



