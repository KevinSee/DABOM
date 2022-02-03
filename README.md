
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DABOM <a href='https://github.com/KevinSee/DABOM'><img src='man/figures/logo.png' align="right" height="139" /></a>

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/KevinSee/DABOM/master?urlpath=rstudio)

**D**am **A**dult **B**ranch **O**ccupancy **M**odel

## Description

`DABOM` is an R package for estimating abundance of anadromous fishes
using fish tagged at a primary sampling facility (e.g., dam) and later
detected in upstream tributaries. It incorporates the branching
structure of a stream network, and simultaneously estimates imperfect
detection probabilities at all detection locations and movement
probabilities past detection locations. The movement probabilities, when
multiplied correctly and combined with an estimate of total abundance at
the tagging site, can be used to estimate abundance at a variety of
spatial scales. Further mathematical details of the model can be found
in [Waterhouse, 2020](https://doi.org/10.1002/eap.2202).

The user can find more information related to installation and use of
this package on the [package
website](https://kevinsee.github.io/DABOM/).

## Installation instructions

The `DABOM` compendium can be downloaded as a zip from from this URL:
<https://github.com/BiomarkABS/DABOM/archive/master.zip>

Or the user can install the compendium as an R package from GitHub by
using Hadley Wickham’s `devtools` package:

``` r
# install and load remotes, if necessary
install.packages("devtools")
devtools::install_github("BiomarkABS/DABOM", 
                         build_vignettes = TRUE)
```

`devtools` may require the downloading and installation of Rtools. The
latest version of Rtools can be found
[here](https://cran.r-project.org/bin/windows/Rtools/).

For the latest development version:

``` r
devtools::install_github("BiomarkABS/DABOM@develop")
```

### JAGS Software

The user will also need the [JAGS](http://mcmc-jags.sourceforge.net/)
software to run DABOM. They can download that from
[SourceForge](https://sourceforge.net/projects/mcmc-jags/files/). JAGS
(**J**ust **A**nother **G**ibbs **S**ampler) software is used by `DABOM`
for Bayesian inference. Please download version \>= 4.0.0.

### Other R Packages

The user will also need to install `tidyverse`, a series of R packages
that work together for data science (i.e. data cleaning and
manipulation), as well as the `rjags` package to interface with JAGS.
The `tidyverse` and `rjags` packages are all available from the R
community and can be installed by typing the following into your R
console:

``` r
install.packages("tidyverse")
install.packages("rjags")
```

## Authors

DABOM is a collaborative project, with the primary contributors being:

-   Kevin See (WDFW)
-   Ryan N. Kinzer (Nez Perce Tribe)

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.
