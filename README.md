
# CompARE <img src="man/figures/logo.png" align="right" alt="" width="250" />

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

<!-- badges: end -->

[**CompARE**](https://www.grbio.eu/compare/)
(<https://www.grbio.eu/compare/>) is a initiative inspired to provide
help on issues relating to trials with composite endpoints. CompARE may
be used as a tool for calculating the elements needed in the planning
phase of clinical trials involving composite endpoints. CompARE comprises 
two diferrent tools:

- A couple of [**shiny**](https://www.rstudio.com/products/shiny/) apps for time-to-event and binary endpoints, respectively.
- The R package [**CompAREdesign**](https://CRAN.R-project.org/package=CompAREdesign)


# CompARE shiny apps

[**CompARE**](https://www.grbio.eu/compare/) comprises two different
Shiny apps: one devoted to time-to-event endpoints, the other to binary
endpoints.

  - **CompARE for Time-to-event endpoints**: [Time-to-event
    app](https://www.grbio.eu/compare/CompARETimeToEvent/).
  - **CompARE for Binary endpoints**: [Binary
    app](https://www.grbio.eu/compare/CompAREBinary/).


Their user-friendly interface allows to input the main parameters
included in the trial -such as the treatment effect on the components of
the composite endpoint, and the frequencies of occurrence- and the app
provides sample size calculations among others.

## Getting Started

If you are a newcomer, we recommend starting with the tutorial
vignettes. These vignettes provide an introduction to CompARE:

  - **Time-to-event Tutorial**: Guide document of CompARE for
    Time-to-event endpoints [Time-to-event
    Tutorial](https://www.grbio.eu/compare/CompARETimeToEvent/help_Tutorial.html).
  - **Binary Tutorial**: Guide document of CompARE for Binary endpoints
    [Binary
    Tutorial](https://www.grbio.eu/compare/CompAREBinary/Help-Tutorial.html).

# CompARE design R package

## Functions

The `CompAREdesign` package contains the following functions:

  - *surv\_tte* to compute the survival function for the composite time-to-event endpoint and both components
  - *effectsize\_tte* to compute the treatment effect for the composite time-to-event endpoint
  - *samplesize\_tte* to compute the sample size for the composite time-to-event endpoint
  - *ARE\_tte* to compute the ARE method for time-to-event endpoints
  - *plot\_tte* to get four plots related to previous features
  - *simula\_tte* to simulate time-to-event data for the time-to-event composite endpoint and its components
  - *prob\_cbe* to compute the probability of a composite binary
    endpoint;  
  - *lower\_corr* to compute the lower bound of Pearson’s correlation;
  - *upper\_corr* to compute the upper bound for Pearson’s correlation;
  - *effectsize\_cbe* to compute the treatment effect on the composite
    binary endpoint;
  - *samplesize\_cbe* to compute the sample size for composite binary
    endpoint;
  - *ARE\_cbe* to compute the ARE method for composite binary endpoint.
  - *simula\_cbe* to simulate time-to-event data for the binary composite endpoint and its components
  

## Installation

``` r
install.packages("CompAREdesign")
library("CompAREdesign")
```

