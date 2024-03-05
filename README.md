
# CompARE <img src="man/figures/CompAREdesign_logo.png" align="right" alt="" width="250" />

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

[**CompARE**](https://www.grbio.eu/compare/)
(<https://www.grbio.eu/compare/>) is a initiative inspired to provide
help on issues relating to trials with composite endpoints. CompARE may
be used as a tool for calculating the elements needed in the planning
phase of clinical trials involving composite endpoints. CompARE comprises 
two diferent tools:

- A couple of [**shiny**](https://www.rstudio.com/products/shiny/) apps for time-to-event and binary endpoints, respectively. You can found more info [here](https://github.com/CompARE-Composite/Functions-shiny-apps)
- The R package [**CompAREdesign**](https://CRAN.R-project.org/package=CompAREdesign)


# CompAREdesign R package

## Functions

The `CompAREdesign` package contains the following functions:

  - __*surv\_tte*__ to compute the survival function for the composite time-to-event endpoint and both components
  - __*effectsize\_tte*__ to compute the treatment effect for the composite time-to-event endpoint
  - __*samplesize\_tte*__ to compute the sample size for the composite time-to-event endpoint
  - __*ARE\_tte*__ to compute the ARE method for time-to-event endpoints
  - __*plot\_tte*__ to get four plots related to previous features
  - __*simula\_tte*__ to simulate time-to-event data for the time-to-event composite endpoint and its components
  - __*prob\_cbe*__ to compute the probability of a composite binary
    endpoint 
  - __*lower\_corr*__ to compute the lower bound of Pearson’s correlation
  - __*upper\_corr*__ to compute the upper bound for Pearson’s correlation
  - __*effectsize\_cbe*__ to compute the treatment effect on the composite
    binary endpoint
  - __*samplesize\_cbe*__ to compute the sample size for composite binary
    endpoint
  - __*ARE\_cbe*__ to compute the ARE method for composite binary endpoint
  - __*simula\_cbe*__ to simulate time-to-event data for the binary composite endpoint and its components
  

## Installation

``` r
install.packages("CompAREdesign")
library("CompAREdesign")
```

