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
