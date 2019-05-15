# DABOM
Dam Adult Branch Occupancy Model

`DABOM` is an R package for estimating abundance of anadromous fishes using fish tagged at a primary sampling facility (e.g., dam) and later interrogated in upstream tributaries.

## Getting Started

`STADEM` requires several packages that are available through the `tidyverse` package. You can install all the necessary packages by using:

```{r}
install.packages("tidyverse", "rjags", "jagsUI")
```

To install `DABOM` you can use Hadley Wickham's `devtools` package. To install and load the `devtools` package use:
```
install.packages("devtools")
library(devtools)
```
NOTE: To use `devtools`, you may also have to download and install Rtools (although you shouldn't). The latest version on Rtools can be found at
https://cran.r-project.org/bin/windows/Rtools/

Once `devtools` is successfully installed, use the following to install DABOM:
```
devtools::install_github("KevinSee/DABOM")
```

DABOM requires the JAGS software (**J**ust **A**nother **G**ibbs **S**ampler). This can be downloaded here:

https://sourceforge.net/projects/mcmc-jags/files/

Please download version >= 4.0.0

# Contributions

If you are interested in making contributions to DABOM, consider getting a GitHub account, fork this repository, clone to a local directory, modify, and send me a pull request. I can then review any changes and merge.

