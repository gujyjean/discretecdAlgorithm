## Test environments
* local OS X install, R 3.3.3
* ubuntu 12.04.5 (travis-ci: oldrel, devel, and release)
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 note from win-builder:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Jiaying Gu <gujy.lola@gmail.com>'

Possibly mis-spelled words in DESCRIPTION:
  Fu (9:39)
  Gu (9:35)
  Zhou (9:46)
  logit (8:24)
  multi (8:18)
  multinomial (7:75)

Fu, Gu, Zhou are all last names of authors of the paper referenced in this package. logit, multi, and multinomial are all technical terms.
  
There was 1 note from travis-ci:
* checking installed package size ... NOTE
  installed size is  9.4Mb
  sub-directories of 1Mb or more:
    libs   9.3Mb
    
This package imports from RcppEigen, and RcppEigen has installed size 27.0MB. Therefore this package can be relatively big.

## Reverse dependencies
* sparsebn: I have tested current version of sparsebn on CRAN, there is no ERRORs, WARNINGs or NOTES. You will receive update for this package very soon from Bryon. I have also tested the latest developer's version and have run R CMD check, there is no ERRORs, WARNINGs, or NOTEs.
