# Analysis of Project FeederWatch data demonstrating observer shift

## Introduction
The purpose of this repository is to present the code for demonstrating the observer shift in the Project FeederWatch dataset. 

## Directions

### Observer shift

The actual analysis is conducted entirely in script 05. That script (05) loads an SQLite database file, which must be downloaded from Zenodo (https://zenodo.org/records/10215261). You can recreate the observer shift figure by simply downloading the database file from Zenodo and running script 05. Scripts 01-04 have the sole purpose of creating that SQLite databse file from publicly available data. That publicly available data is accessible at the Project FeederWatch website (https://feederwatch.org/explore/raw-dataset-requests/).

### Variance explained per covariate

The variance explained figure can be recreated from script 06. As with script 05, script 06 also requires the SQLite database file, which is downloadable from Zenodo.