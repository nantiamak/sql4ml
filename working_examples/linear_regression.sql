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


-- Observations table
CREATE TABLE observations (
       observationID VARCHAR(15),
       PRIMARY KEY (observationID)
);

-- Features table
CREATE TABLE features (
       observationID VARCHAR(15),
       featureName VARCHAR(30),
       featureValue DOUBLE,
       PRIMARY KEY (observationID, featureName)
);      

-- Linear regression model weights table
CREATE TABLE weights (
       featureName VARCHAR(30),
       weightValue DOUBLE,
       PRIMARY KEY (featureName)
);

-- Targets table
CREATE TABLE targets (
       observationID VARCHAR(15),
       targetValue DOUBLE,
       PRIMARY KEY (observationID)
);     

-- Prediction function of linear regression
CREATE VIEW predictions AS
SELECT SUM(features.featureValue * weights.weightValue) AS predictionValue, features.observationID AS observationID
FROM features, weights
WHERE features.featureName = weights.featureName
GROUP BY observationID;

CREATE VIEW errors AS
SELECT predictions.prediction - targets.targetValue AS errorValue, targets.observationID AS observationID
FROM predictions, targets
WHERE predictions.observationID = targets.observationID;

CREATE VIEW squaredErrors AS
SELECT POW(errors.errorValue, 2) AS squaredErrorValue, errors.observationID AS observationID
FROM errors;

-- Mean squared error - objective function of linear regression
CREATE VIEW meanSquaredError AS
SELECT AVG(squaredErrors.squaredErrorValue) FROM squaredErrors;
