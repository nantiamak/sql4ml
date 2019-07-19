
CREATE TABLE observations (
       observationID INT,
       PRIMARY KEY (observationID)
);

CREATE TABLE features (
       observationID INT,
       featureName VARCHAR(10),
       featureValue DOUBLE,
       PRIMARY KEY (observationID, featureName)
);      

CREATE TABLE weights (
       featureName VARCHAR(10),
       weightValue DOUBLE,
       PRIMARY KEY (featureName)
);

CREATE TABLE labels (
       observationID INT,
       labelValue DOUBLE,
       PRIMARY KEY (observationID)
);


CREATE VIEW product AS SELECT SUM(features.featureValue * weights.weightValue) AS productValue, features.observationID AS observationID
FROM features, weights
WHERE features.featureName = weights.featureName
GROUP BY observationID;

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

CREATE VIEW loss AS SELECT (-1)*SUM((first_part.firstPartValue + second_part.secondPartValue)) AS lossValue
FROM first_part, second_part
WHERE first_part.observationID = second_part.observationID




----------- Proxeiro ------------
-- Pivoting features table, all features of an observation in a single row
SELECT features.observationID AS observationID, SUM(CASE WHEN features.`featureName`='price' THEN features.featureValue ELSE NULL END) AS priceValue, SUM(CASE WHEN features.`featureName`='day' THEN features.featureValue ELSE NULL END) AS dayValue FROM features GROUP BY features.observationID
