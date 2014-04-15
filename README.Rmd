Mostly Harmless Econometrics in R
========================================================

The R package RcompAngrist is a R companion to Angrist and Pischke's **Mostly Harmless Econometrics**. 

It contains some of the datasets, code snippets showig how to reproduce, and new functions.

The new functions are:

* Moulton standard errors
* The LIML estimator
* A R version of STATA webuse()

Demo
-----------------------
See the [wiki](wiki) for the exhaustive list. 

Figure [4.1.1](https://github.com/MatthieuStigler/RCompAngrist/wiki/FIG_4.1.1)


Installing **RcompAngrist**
-----------------------

This github website hosts the source code. One of the easiest ways to install the package from github is by using the R package **RcompAngrist**:


```r
library(devtools)
install_github(repo = "RCompAngrist", username = "MatthieuStigler", subdir = "RcompAngrist")
```
