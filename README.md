# sql2tf

sql2tf is a Haskell module that takes as input SQL code which defines a supervised machine
learning model (ML), as well as a set of parameters, and generates TensorFlow (Python API) code
that trains this model.

It is the prototype that is used in the paper
"Nantia Makrynioti, Ruy Ley-Wild and Vasilis Vassalos. sql4ml: a declarative end-to-end workflow for machine learning.".

## Requirements

The software has been tested on macOS High Sierra and Ubuntu 16.04.

To compile it you need to install the Haskell Platform (https://www.haskell.org/platform/).
See also the section on installation on how to build dependent projects.

## Installation

sql2tf uses the open source project queryparser (https://github.com/uber/queryparser), also in Haskell.
To build queryparser, follow the instructions on the Github page of the project.

The sql2tf module is in file sql2tf_translator.hs.
You can load the module via ghci.

File main.hs containts two examples on how to translate SQL to TensorFlow code end-to-end.

To compile main.hs, run in a terminal:

   ghc -o main main.hs

Sql2tf uses the MySQL database (https://www.mysql.com/) for storing data.
To run the generated TensorFlow code, you need to install it.
To install MySQL follow the instructions in https://www.mysql.com/.

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

To run main, type in a terminal:

   ./main linear // To generate TensorFlow code for the Linear Regression model in working_examples/linear_regression.sql
   ./main logistic // To generate TensorFlow code for the Logistic Regression model in working_examples/logistic_regression.sql

The files with the generated TensorFlow/Python code can be executed like any other TensorFlow program.







