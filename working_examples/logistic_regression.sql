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
       observationID INT,
       PRIMARY KEY (observationID)
);

-- Features table
CREATE TABLE features (
       observationID INT,
       featureName VARCHAR(10),
       featureValue DOUBLE,
       PRIMARY KEY (observationID, featureName)
);      

-- Logistic regression model weights table
CREATE TABLE weights (
       featureName VARCHAR(10),
       weightValue DOUBLE,
       PRIMARY KEY (featureName)
);

-- Labels table
CREATE TABLE labels (
       observationID INT,
       labelValue DOUBLE,
       PRIMARY KEY (observationID)
);


-- Products between features and weights
CREATE VIEW product AS SELECT SUM(features.featureValue * weights.weightValue) AS productValue, features.observationID AS observationID
FROM features, weights
WHERE features.featureName = weights.featureName
GROUP BY observationID;

-- Sigmoid function
CREATE VIEW sigmoid AS SELECT product.observationID AS observationID,
(1/(1+EXP(-product.productValue))) AS sigmoidValue
FROM product;

CREATE VIEW log_sigmoid AS SELECT sigmoid.observationID AS observationID,
LN(sigmoid.sigmoidValue) AS logSigmoidValue
FROM sigmoid;

CREATE VIEW log2 AS SELECT sigmoid.observationID AS observationID,
LN(1-sigmoid.sigmoidValue) AS log2Value
FROM sigmoid;

CREATE VIEW first_part AS SELECT labels.observationID AS observationID,
(labels.labelValue * log_sigmoid.logSigmoidValue) AS firstPartValue
FROM labels, log_sigmoid
WHERE labels.observationID = log_sigmoid.observationID;

CREATE VIEW second_part AS SELECT labels.observationID AS observationID,
((1-labels.labelValue) * log2.log2Value) AS secondPartValue
FROM labels, log2
WHERE labels.observationID = log2.observationID;

-- Logistic loss - objective function of logistic regression
CREATE VIEW loss AS SELECT (-1)*SUM((first_part.firstPartValue + second_part.secondPartValue)) AS lossValue
FROM first_part, second_part
WHERE first_part.observationID = second_part.observationID
