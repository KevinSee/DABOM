
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DABOM

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/BiomarkABS/DABOM/master?urlpath=rstudio)

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
spatial scales.

## Installation instructions

`DABOM` requires several packages that are available through the
`tidyverse` package. You can install all the necessary packages by
using:

``` r
install.packages("tidyverse", "rjags", "jagsUI")
```

To install `DABOM` you can use Hadley Wickham’s `devtools` package. To
install and load the devtools package use:

``` r
install.packages("devtools")
library(devtools)
```

NOTE: To use devtools, you may also have to download and install Rtools
(although you shouldn’t). The latest version on Rtools can be found at
<https://cran.r-project.org/bin/windows/Rtools/>

You can download the compendium as a zip from from this URL:
<https://github.com/BiomarkABS/DABOM/archive/master.zip>

Or you can install this compendium as an R package, DABOM, from GitHub
with:

``` r
# install.packages("devtools")
remotes::install_github("BiomarkABS/DABOM", build_vignettes = TRUE)
```

DABOM requires the JAGS software (**J**ust **A**nother **G**ibbs
**S**ampler). This can be downloaded here:

<https://sourceforge.net/projects/mcmc-jags/files/>

Please download version \>= 4.0.0

A vignette describing how to use the DABOM package is in the works.

## Authors

DABOM is a collaborative project, with the primary contributors being:

  - Kevin See (Biomark, Inc.)
  - Ryan N. Kinzer (Nez Perce Tribe)

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
