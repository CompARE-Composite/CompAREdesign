% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ARE_tte.R
\name{ARE_tte}
\alias{ARE_tte}
\title{ARE method for composite time to event endpoints}
\usage{
ARE_tte(
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
  subdivisions = 50,
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

\item{subdivisions}{integer parameter greater than or equal to 10. Number of points used to plot the ARE according to correlation. The default is 50. Ignored if plot_print=FALSE and plot_save=FALSE.}

\item{plot_print}{logical indicating if the ARE according to the correlation should be displayed. The default is FALSE}

\item{plot_save}{logical indicating if the plot of ARE according to the correlation is stored for future customization. The default is FALSE}
}
\value{
Returns the ARE value along with the fixed correlation. If the ARE 
value is larger than 1 then the composite endpoint is preferred over the 
relevant endpoint. Otherwise, the endpoint 1 is preferred as the primary 
endpoint of the study. In addition, if \code{plot_save=TRUE} an object of 
class \code{ggplot} with the ARE according to the correlation is stored in the output.
}
\description{
The composite endpoint is assumed to be a time to event endpoint formed by a combination of two events (E1 and E2). We assume that the endpoint 1 is more relevant for the clinical question than endpoint 2. 
This function calculates the ARE (Asymptotic Relative Efficiency) method for time to event endpoints. The method quantifies the differences in efficiency of using the composite or the relevant as primary endpoint to lead the trial and, moreover, provides a decision rule to choose the primary endpoint. If the ARE is larger than 1, the composite endpoint may be considered the best option as primary endpoint. Otherwise, the relevant endpoint is preferred.
}
\details{
Some parameters might be difficult to anticipate, especially the shape parameters of Weibull distributions and those referred to the relationship between the marginal distributions. 
For the shape parameters (beta_e1, beta_e2) of the Weibull distribution, we recommend to use \eqn{\beta_j=0.5}, \eqn{\beta_j=1} or \eqn{\beta_j=2} if a decreasing, constant or increasing rates over time are expected, respectively.
For the correlation (rho) between both endpoints, generally a positive value is expected as it has no sense to design an study with two endpoints negatively correlated. We recommend to use \eqn{\rho=0.1}, \eqn{\rho=0.3} or \eqn{\rho=0.5} for weak, mild and moderate correlations, respectively.
For the type of correlation (rho_type), although two different type of correlations are implemented, we recommend the use of the Spearman's correlation.
In any case, if no information is available on these parameters, we recommend to use the default values provided by the function.
}
\examples{
# ARE for a specific study where the composite endpoint is recommended
ARE_tte(p0_e1=0.1, p0_e2=0.1, HR_e1=0.9, HR_e2=0.8, beta_e1 = 1, beta_e2 = 1, 
case=1, copula = "Frank", rho = 0.3, rho_type = "Spearman")
# ARE for a specific study where the composite endpoint is not recommended
ARE_tte(p0_e1=0.1, p0_e2=0.05, HR_e1=0.6, HR_e2=0.8, beta_e1 = 1, beta_e2 = 1, 
case=1, copula = "Frank", rho = 0.3, rho_type = "Spearman")
}
\references{
Gomez Melis, G. and Lagakos, S.W. (2013). Statistical considerations when using a composite endpoint for comparing treatment groups. Statistics in Medicine. Vol 32(5), pp. 719-738. https://doi.org/10.1002/sim.5547
}
