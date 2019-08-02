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

module Main where

import Sql4ml_translator
import System.Environment
import qualified Data.Map as M

main = do

  args <- getArgs
  let algorithm = (head args)

  -- Logistic Regression example
  case algorithm of
    
    "logistic" -> do

      putStrLn "Starting translation..."

      writeFile "generated_logistic.py" (endToEndTranslate ["CREATE TABLE observations (observationID INT,PRIMARY KEY (observationID));", "CREATE TABLE features (observationID INT,featureName VARCHAR(10),featureValue DOUBLE,PRIMARY KEY (observationID, featureName));", "CREATE TABLE weights (featureName VARCHAR(10),weightValue DOUBLE,PRIMARY KEY (featureName));","CREATE TABLE labels (observationID INT,labelValue DOUBLE,PRIMARY KEY (observationID));","CREATE VIEW product AS SELECT SUM(features.featureValue * weights.weightValue) AS productValue, features.observationID AS observationID FROM features, weights WHERE features.featureName = weights.featureName GROUP BY observationID;", "CREATE VIEW sigmoid AS SELECT product.observationID AS observationID, (1/(1+EXP(-product.productValue))) AS sigmoidValue FROM product;", "CREATE VIEW log_sigmoid AS SELECT sigmoid.observationID AS observationID, LN(sigmoid.sigmoidValue) AS logSigmoidValue FROM sigmoid;", "CREATE VIEW log2 AS SELECT sigmoid.observationID AS observationID, LN(1-sigmoid.sigmoidValue) AS log2Value FROM sigmoid;", "CREATE VIEW first_part AS SELECT labels.observationID AS observationID,(labels.labelValue * log_sigmoid.logSigmoidValue) AS firstPartValue FROM labels, log_sigmoid WHERE labels.observationID = log_sigmoid.observationID;", "CREATE VIEW second_part AS SELECT labels.observationID AS observationID, ((1-labels.labelValue) * log2.log2Value) AS secondPartValue FROM labels, log2 WHERE labels.observationID = log2.observationID;", "CREATE VIEW loss AS SELECT (-1)*SUM((first_part.firstPartValue + second_part.secondPartValue)) AS lossValue FROM first_part, second_part;"] ["features"] ["featureName"] [["f1", "f2", "f3", "f4"]] "observations" "labels" "loss" "root" "1234" "iris" ["weights"] (M.fromList [("weights", [4])]) 1000 0.01)

      putStrLn "Translation finished."
    
    "linear" -> do

      putStrLn "Starting translation..."

      writeFile "generated_LR.py" (endToEndTranslate ["CREATE TABLE observations (observationID VARCHAR(15),PRIMARY KEY (observationID));","CREATE TABLE features (observationID VARCHAR(15),featureName VARCHAR(30),featureValue DOUBLE,PRIMARY KEY (observationID, featureName));","CREATE TABLE weights (featureName VARCHAR(30),weightValue DOUBLE,PRIMARY KEY (featureName));","CREATE TABLE targets (observationID VARCHAR(15),targetValue DOUBLE,PRIMARY KEY (observationID));","CREATE VIEW predictions AS SELECT SUM(features.featureValue * weights.weightValue) AS predictionValue, observationID AS observationID FROM features, weights WHERE features.featureName = weights.featureName GROUP BY observationID;", "CREATE VIEW errors AS SELECT predictions.prediction - targets.targetValue AS errorValue, observationID AS observationID FROM predictions, targets WHERE predictions.observationID = targets.observationID;", "CREATE VIEW squaredErrors AS SELECT POW(errors.errorValue, 2) AS squaredErrorValue, observationID AS observationID FROM errors;", "CREATE VIEW meanSquaredError AS SELECT AVG(squaredErrors.squaredErrorValue) FROM squaredErrors;"] ["features"] ["featureName"] [["LSTAT", "CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B"]] "observations" "targets" "meanSquaredError" "root" "1234" "bostonHousing" ["weights"] (M.fromList [("weights", [13])]) 1000 0.0000003)

      putStrLn "Translation finished."
    
    _ -> do putStrLn "Invalid argument"
    

  
