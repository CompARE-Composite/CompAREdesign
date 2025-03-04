% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effectsize_tte.R
\name{effectsize_tte}
\alias{effectsize_tte}
\title{Effect for composite time-to-event endpoints}
\usage{
effectsize_tte(
  p0_e1,
  p0_e2,
  HR_e1,
  HR_e2,
  beta_e1 = 1,
  beta_e2 = 1,
  case,
  copula = "Frank",
  rho = 0.3,
  rho_type = "Spearman",
  followup_time = 1,
  subdivisions = 1000,
  plot_print = FALSE,
  plot_save = FALSE
)
}
\arguments{
\item{p0_e1}{numeric parameter between 0 and 1, expected proportion of observed events for the endpoint E1}

\item{p0_e2}{numeric parameter between 0 and 1, expected proportion of observed events for the endpoint E2}

\item{HR_e1}{numeric parameter between 0 and 1, expected cause specific hazard Ratio the endpoint E1}

\item{HR_e2}{numeric parameter between 0 and 1, expected cause specific hazard Ratio the endpoint E2}

\item{beta_e1}{numeric positive parameter, shape parameter (\eqn{\beta_1}) for a Weibull distribution for the endpoint E1 in the control group. See details for more info.}

\item{beta_e2}{numeric positive parameter, shape parameter (\eqn{\beta_2}) for a Weibull distribution for the endpoint E2 in the control group. See details for more info.}

\item{case}{integer parameter in \{1,2,3,4\}: (1) none of the endpoints is death; (2) endpoint 2 is death; (3) endpoint 1 is death; (4) both endpoints are death by different causes.}

\item{copula}{character indicating the copula to be used: "Frank" (default), "Gumbel" or "Clayton". See details for more info.}

\item{rho}{numeric parameter between -1 and 1, Spearman's correlation coefficient o Kendall Tau between the marginal distribution of the times to the two events E1 and E2. See details for more info.}

\item{rho_type}{character indicating the type of correlation to be used: "Spearman" (default) or "Kendall". See details for more info.}

\item{followup_time}{numeric parameter indicating the maximum follow up time (in any unit). Default is 1.}

\item{subdivisions}{integer parameter greater than or equal to 10. Number of subintervals to estimate the effect size. The default is 1000.}

\item{plot_print}{logical indicating if the HR over time should be displayed. The default is FALSE}

\item{plot_save}{logical indicating if the plot of HR over time should is stored for future customization. The default is FALSE}
}
\value{
A list formed by two lists: \code{effect_size}, which contains the 
expected treatment effect measures and \code{measures_by_group}, which contains 
some relevant measures for each group

\code{effect_size} list:

\describe{
    \item{\code{gAHR}}{geometric Average Hazard Ratio}
    \item{\code{AHR}}{Average Hazard Ratio}
    \item{\code{RMST_ratio}}{Restricted Mean Survival Time Ratio}
    \item{\code{Median_ratio}}{Median Survival Time Ratio}
}

\code{measures_by_group} list:

\describe{
    \item{\code{pstar}}{array with the probability of observing the composite event for each group}
    \item{\code{p1}}{array with the probability of observing the first event for each group}
    \item{\code{p2}}{array with the probability of observing the second event for each group}
    \item{\code{RMST}}{array with the restricted mean survival time for each group}
    \item{\code{Median}}{array with the median surival time for each group}
}

In addition, if \code{plot_save=TRUE} an object of class \code{ggplot} with
the HR over time for composite endpoint is stored in the list.
}
\description{
This function calculates different effect measures for time-to-event composite outcomes. 
The composite endpoint is assumed to be a time-to-event endpoint formed by a combination of two events (E1 and E2).
The effect size is calculated on the basis of anticipated information on the composite components and the correlation between them.
Marginal distributions are assumed weibull for both endpoints.
The function allows to compute the effect size in terms of the geometric average hazard ratio, the average hazard ratio, 
the ratio of restricted mean survival times and the median survival time ratio.
}
\details{
Some parameters might be difficult to anticipate, especially the shape parameters of Weibull distributions and those referred to the relationship between the marginal distributions. 
For the shape parameters (beta_e1, beta_e2) of the Weibull distribution, we recommend to use \eqn{\beta_j=0.5}, \eqn{\beta_j=1} or \eqn{\beta_j=2} if a decreasing, constant or increasing rates over time are expected, respectively.
For the correlation (rho) between both endpoints, generally a positive value is expected as it has no sense to design an study with two endpoints negatively correlated. We recommend to use \eqn{\rho=0.1}, \eqn{\rho=0.3} or \eqn{\rho=0.5} for weak, mild and moderate correlations, respectively.
For the type of correlation (rho_type), although two different type of correlations are implemented, we recommend the use of the Spearman's correlation.
In any case, if no information is available on these parameters, we recommend to use the default values provided by the function.

All returned expected effect sizes for the composite endpoint should be interpreted in relative terms (treated to control). 
gAHR and AHR represent the risk reduction that will be achieved with the new therapy, while RMST_ratio and Median_ratio represent the gain in time gain terms until the event.
}
\examples{
effectsize_tte(p0_e1   = .59, p0_e2   = .74, 
               HR_e1   = .91, HR_e2   = .77, 
               beta_e1 = 1,   beta_e2 = 2, 
               case    = 3,   rho     = .5,
               copula  = 'Frank', rho_type   = 'Spearman',
               plot_print = TRUE, plot_save = FALSE) 
}
\references{
Schemper, M., Wakounig, S., Heinze, G. (2009). The estimation of average hazard ratios by weighted Cox regression. Stat. in Med. 28(19): 2473--2489. doi:10.1002/sim.3623
}
