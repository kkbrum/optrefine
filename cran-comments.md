## Resubmission
This is a resubmission. In this version I have:
* Made the examples for `refine()` and `plot.strat()` shorter to take less time
* Removed extra spaces from the package description
* Added documentation for the value returned by `plot.strat()` 

I am unable to fix the NOTE about 'gurobi' not being available for checking since it is a commercial software and the R package must be downloaded from their website. However, it is only suggested and the package works without it. Installation instructions for the R package 'gurobi' are in the description of the package.

## Test environments
- local macOS install, R 4.2.1
- win-builder (devel and release)
- github actions:
    - {os: macos-latest,   r: 'release'}
    - {os: windows-latest, r: 'release'}
    - {os: ubuntu-latest,   r: 'devel', http-user-agent: 'release'}
    - {os: ubuntu-latest,   r: 'release'}
    - {os: ubuntu-latest,   r: 'oldrel-1'}

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Katherine Brumberg <kbrum@wharton.upenn.edu>'
New submission
Possibly misspelled words in DESCRIPTION:
  covariate (7:70)
Suggests or Enhances not in mainstream repositories:
  gurobi
    
* checking package dependencies ... NOTE
Package suggested but not available for checking: 'gurobi'

'gurobi' is a commercial optimization software, and is not required for the use of my package, although it is recommended. Instructions for installation are included in the description.

## Downstream dependencies
There are currently no downstream dependencies for this package.
