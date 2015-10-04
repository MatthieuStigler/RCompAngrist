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
The [wiki](http://github.com/MatthieuStigler/RCompAngrist/wiki/FIG_4.1.1) contains web examples, with code, of following tables/figures:

* Figure [4.1.1](http://github.com/MatthieuStigler/RCompAngrist/wiki/FIG_4.1.1)
* Table [4.6.2](http://github.com/MatthieuStigler/RCompAngrist/wiki/TAB_4.6.2)


Installing **RcompAngrist**
-----------------------

This github website hosts the source code. One of the easiest ways to install the package from github is by using the R package **RcompAngrist**:


```r
library(devtools)
install_github(repo = "MatthieuStigler/RCompAngrist", subdir = "RCompAngrist")
```
