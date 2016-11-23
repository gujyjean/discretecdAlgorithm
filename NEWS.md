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
