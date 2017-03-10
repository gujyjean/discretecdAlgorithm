# discretecdAlgorithm 0.0.3

## Major changes

* Added `generate_discrete_data()` method to generate categorical data set from multi-logit model from a DAG given coefficient list.
* Added `data_gen()` method to generate a list of coefficients. And calls `generate_discrete_data()` to generate a categorical data set from multi-logit model from a DAG.

## Bug fixes
* Added input check for `indata`, so that if a node has only one level, cd.run() will remind users to remove the column. 

# discretecdAlgorithm 0.0.2

## Major changes

* Added option `adaptive` in `cd.run()` algorithm, so that users can run both adaptive cd algorithm and regular cd algorithm. With `adaptive = TRUE`, function run adaptive cd algorithm, whith `adaptive = FALSE`, function will run regular cd algorithm.

## Features

* Added a `NEWS.md` file to track changes to the package

## Bug fixes

* Updated line search in C++ function. Adjusted parameters used in line search.
* Updated initialization of intercept and parameters in C++ function. CDAlgo(), so that it can now take discrete variables when missing base level.

# discretecdAlgorithm 0.0.1

* Initial stable release
