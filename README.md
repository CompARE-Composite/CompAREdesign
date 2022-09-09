
# CompARE <img src="man/figures/logo.png" align="right" alt="" width="250" />

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

<!-- badges: end -->

[**CompARE**](https://cinna.upc.edu/compare/)
(<https://cinna.upc.edu/compare/>) is a web-platform inspired to 
help on the design of trials with composite endpoints. CompARE may
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

  - *prob\_cbe* to compute the probability of a composite binary
    endpoint;  
  - *lower\_corr* to compute the lower bound of Pearson’s correlation;
  - *upper\_corr* to compute the upper bound for Pearson’s correlation;
  - *effectsize\_cbe* to compute the treatment effect on the composite
    binary endpoint;
  - *samplesize\_cbe* to compute the sample size for composite binary
    endpoint;
  - *ARE\_cbe* to compute the ARE method for composite binary endpoint.

## Installation

``` r
# install.packages("devtools")
library(devtools)
install_github("CompARE-Composite/CompARE-package")
```

See also [https://cran.r-project.org/package=CompAREdesign](https://cran.r-project.org/package=CompAREdesign)
