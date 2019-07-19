
CREATE TABLE observations (
       observationID VARCHAR(15),
       PRIMARY KEY (observationID)
);

CREATE TABLE features (
       observationID VARCHAR(15),
       featureName VARCHAR(30),
       featureValue DOUBLE,
       PRIMARY KEY (observationID, featureName)
);      

CREATE TABLE weights (
       featureName VARCHAR(30),
       weightValue DOUBLE,
       PRIMARY KEY (featureName)
);

CREATE TABLE targets (
       observationID VARCHAR(15),
       targetValue DOUBLE,
       PRIMARY KEY (observationID)
);     

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

CREATE VIEW sumSquaredError AS
SELECT AVG(squaredErrors.squaredErrorValue) FROM squaredErrors;
