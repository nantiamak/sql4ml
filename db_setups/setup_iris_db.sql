CREATE DATABASE iris;

USE iris;

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

LOAD DATA LOCAL INFILE '/Users/Nantia/Downloads/iris_observations.csv' INTO TABLE observations 
LINES TERMINATED BY '\r\n'
(observationID);

LOAD DATA LOCAL INFILE '/Users/Nantia/Downloads/iris_features.csv' INTO TABLE features
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
(observationID, featureName, featureValue);

LOAD DATA LOCAL INFILE '/Users/Nantia/Downloads/iris_labels.csv' INTO TABLE labels
FIELDS TERMINATED BY ',' 
LINES TERMINATED BY '\n'
(observationID, labelValue);
