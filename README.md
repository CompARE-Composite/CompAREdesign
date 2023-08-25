
# CompARE <img src="man/figures/logo.png" align="right" alt="" width="250" />

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

<!-- badges: end -->

[**CompARE**](https://cinna.upc.edu/compare/)
(<https://cinna.upc.edu/compare/>) is a web-platform inspired to provide
help on issues relating to trials with composite endpoints. CompARE may
be used as a tool for calculating the elements needed in the planning
phase of clinical trials involving composite endpoints. With its
user-friendly interface, CompARE allows to input the main parameters
included in the trial -such as the treatment effect on the components of
the composite endpoint, and its frequencies of occurrence- and helps
provide power and sample size calculations among others.

[**CompARE**](https://cinna.upc.edu/compare/) comprises two different
Shiny apps: one devoted to time-to-event endpoints, the other to binary
endpoints.

- **CompARE for Time-to-event endpoints**: [Time-to-event
  app](http://cinna.upc.edu:3838/compare/CompARETimeToEvent/).
- **CompARE for Binary endpoints**: [Binary
  app](http://cinna.upc.edu:3838/compare/CompAREBinary/).

You can also find the R-package version of CompARE in CRAN
[CompAREdesign](https://cran.r-project.org/web/packages/CompAREdesign/index.html).

## Getting Started

If you are a newcomer, we recommend starting with the tutorial
vignettes. These vignettes provide an introduction to CompARE:

- **Time-to-event Tutorial**: Guide document of CompARE for
  Time-to-event endpoints [Time-to-event
  Tutorial](http://cinna.upc.edu:3838/compare/CompARETimeToEvent/help_Tutorial.html).
- **Binary Tutorial**: Guide document of CompARE for Binary endpoints
  [Binary
  Tutorial](http://cinna.upc.edu:3838/compare/CompAREBinary/Help-Tutorial.html).

## `CompAREdesign` R package

The `CompAREdesign` package contains the following functions:

- *prob_cbe* to compute the probability of a composite binary
  endpoint;  
- *lower_corr* to compute the lower bound of Pearson’s correlation;
- *upper_corr* to compute the upper bound for Pearson’s correlation;
- *effectsize_cbe* to compute the treatment effect on the composite
  binary endpoint;
- *samplesize_cbe* to compute the sample size for composite binary
  endpoint;
- *ARE_cbe* to compute the ARE method for composite binary endpoint;
- *surv_tte* to compute the survival function for the composite endpoint
  and both components;
- *effectsize_tte* to compute the treatment effect for the composite
  endpoint;
- *samplesize_tte* to compute the sample size for the composite
  endpoint;
- *ARE_tte* to compute the ARE method for time-to-event endpoints;
- *plot_tte* Returns four plots related to previous features;
- *simula_tte* to simulate time-to-event data for the composite and its
  components.

## Installation

``` r
require(devtools)
install_github("CompARE-Composite/CompARE-package") #developers version

install.packages("CompAREdesign") #stable version from CRAN
library(CompAREdesign)
```

## References

- Jordi Cortes-Martinez, Marta Bofill Roig, Guadalupe Gómez Melis
  (2022). Design of Trials with Composite Endpoints with the R Package
  CompAREdesign. \[[Preprint](https://arxiv.org/abs/2211.02535)\].
