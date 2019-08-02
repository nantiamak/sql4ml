# sql4ml

Sql4ml is a Haskell project that takes as input SQL code which defines the objective/cost/loss function of a
supervised machine learning model (ML), as well as a set of parameters, and generates TensorFlow (Python API) code
that trains this model.

It is the prototype that is used in the paper
"Nantia Makrynioti, Ruy Ley-Wild and Vasilis Vassalos. sql4ml: a declarative end-to-end workflow for machine learning."
(https://arxiv.org/abs/1907.12415).

## Requirements

The software has been tested on macOS High Sierra and Ubuntu 16.04.

To compile it you need to install the Haskell Platform (https://www.haskell.org/platform/).
See also the section on installation on how to build dependent projects.

## Installation

Sql4ml uses the open source project queryparser (https://github.com/uber/queryparser), also in Haskell.
To build queryparser, follow the instructions on the Github page of the project.

The sql4ml module is in file sql4ml_translator.hs.
You can load the module via ghci.

File main.hs containts two examples on how to translate SQL to TensorFlow code end-to-end.

To compile main.hs, run in a terminal:

    ghc -o main main.hs

Sql4ml uses the MySQL database (https://www.mysql.com/) for storing data.
To run the generated TensorFlow code, you need to install MySQL.
To install it follow the instructions in https://www.mysql.com/.

After installing MySQL, you can run the SQL scripts in the directory /db_setups to create two toy databases, one based
on the Boston Housing dataset (https://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html) and the other based
on the Iris dataset (https://archive.ics.uci.edu/ml/datasets/iris), which you can find in the directory /data.

## Use

In file main.hs you can find two examples on how to call function endToEndTranslate, which takes the following input parameters:

* A list of SQL queries defining the objective function of a supervised ML model.
You can find two examples of SQL code defining Linear and Logistic Regression in directory working_examples.
* A list of names of the tables storing the features of the model.
* A list of columns in the aforementioned tables that store names of features.
* A list of lists, each of which has the actual names of features stored in each table.
* The name of the table storing the training observations.
* The name of the table storing the labels/targets of training observations.
* The name of the view computing the objective function of the ML model.
* The username of a user to connect to the database where training data are stored.
* The password of the same user.
* The name of the database storing training data.
* A list of names of the tables storing the weights of the model.
* A map whose elements consist of the name of a weight table and its dimenions as a list.
* The number of gradient descent iterations.
* The learning rate used in gradient descent.

Currently sql4ml supports the translation of SQL create view queries of the following form,

    CREATE VIEW $(name) AS
    SELECT $(columns), $(numericExpr)
    FROM $(tables)
    WHERE $(joinElement)
    GROUP BY $(groupingElement)

and generate an equivalent TensorFlow expression as below:

    $(name) = $(translateNumericExpr(numericExpr))

For examples, check SQL files in the directory /working_examples.


To run main, type in a terminal:

    ./main linear //To generate TensorFlow code for the Linear Regression model in working_examples/linear_regression.sql
    ./main logistic //To generate TensorFlow code for the Logistic Regression model in working_examples/logistic_regression.sql

The files with the generated TensorFlow/Python code can be executed like any other TensorFlow program.

You can also try to translate individual SQL queries by loading the sql4ml_translator module in ghci and type:

    translateToTensorFlowCommand (L.pack "CREATE VIEW squaredErrors AS SELECT POW(errors.errorValue, 2) AS squaredErrorValue, errors.observationID AS observationID FROM errors;") ["features"] ["weights"] [["f1", "f2"]]

where

    translateToTensorFlowCommand :: L.Text -> [String] -> [String] -> [[String]] -> String
    translateToTensorFlowCommand sql_statement feature_tables variable_tables feature_names

and
* sql_statement: a SQL create view query in type Text
* feature_tables: a list of names of the tables storing the features of the model.
* variable_tables: a list of names of the tables storing the weights of the model.
* feature_names: a list of lists, each of which has the actual names of features stored in each table.



