CREATE DATABASE bostonHousing;

USE bostonHousing;

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

LOAD DATA LOCAL INFILE '/Users/Nantia/Downloads/housing_data_observations.csv' INTO TABLE observations 
LINES TERMINATED BY '\r\n'
(observationID);

LOAD DATA LOCAL INFILE '/Users/Nantia/Downloads/housing_data_features.csv' INTO TABLE features
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
(observationID, featureName, featureValue);

LOAD DATA LOCAL INFILE '/Users/Nantia/Downloads/housing_data_labels.csv' INTO TABLE targets
FIELDS TERMINATED BY ',' 
LINES TERMINATED BY '\n'
(observationID, targetValue);
