# sql2tf

sql2tf is a Haskell program that takes as input SQL code which defines a supervised machine
learning model (ML), as well as a set of parameters, and generates TensorFlow (Python API) code
that trains this model.

It is the prototype that is used in the paper
"Nantia Makrynioti, Ruy Ley-Wild and Vasilis Vassalos. sql4ml: a declarative end-to-end workflow for machine learning.".

## Installation

sql2tf uses the open source project queryparser (https://github.com/uber/queryparser), also in Haskell.
To build queryparser, follow the instructions in the Github page of the project.

The sql2tf module is in file sql2tf_translator.hs.
You can load the module via ghci and call its functions.

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



To compile it, run in a terminal:

ghc -o sql2tf_translator sql2tf_translator.hs

In file main.hs 





