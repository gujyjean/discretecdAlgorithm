## Test environments
* local OS X install, R 3.4.1
* ubuntu 12.04.5 (travis-ci: oldrel, devel, and release)
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 
  
There was 1 note from travis-ci:
* checking installed package size ... NOTE
  installed size is  9.6Mb
  sub-directories of 1Mb or more:
    libs   9.5Mb
    
This package imports from RcppEigen, and RcppEigen has installed size 27.0MB. Therefore this package can be relatively big.

## Reverse dependencies
* sparsebn: I have tested current version of sparsebn on CRAN, there is no ERRORs, WARNINGs or NOTES.
