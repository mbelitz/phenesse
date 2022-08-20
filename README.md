# `phenesse`
`phenesse`: Estimate phenological metrics using presence-only data 

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/phenesse)](https://cran.r-project.org/package=phenesse)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3964458.svg)](https://doi.org/10.5281/zenodo.3964458)

<img style="float: left;" align = "left" src = "hexsticker.png" width = 180>

The `phenesse` package provides tools in `R` to estimate phenological metrics using presence only data. The package includes a new Weibull-parameterized estimator described in Belitz et al. (2020) (https://doi.org/10.1111/2041-210X.13448). Additionally, the package provides a non-parametric bootstrap approach to estimating confidence intervals for this estimator as well as quantile and mean estimates. 

Note that generating confidence intervals of a Weibull-parameterized estimate is very computationally expensive. We recommend exploring parallelization options when generating many CIs of Weibull-parameterized estimates. An example of parallelization can be found in the vignette. 
<br><br>

# Installation 

## CRAN

`phenesse` is available on CRAN. Install using
`install.packages("phenesse")`

## GitHub

The development version of `phenesse` is available on GitHub. To install without a vignette, use:
`devtools::install_github("mbelitz/phenessse")`
