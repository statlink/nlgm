\name{Non-linear growth models}
\alias{nlgm}
\title{
Non-linear growth models
}

\description{
Non-linear growth models.
}

\usage{
nlgm(y, ti, type, ini, ti.ahead = NULL)
}

\arguments{
\item{y}{
A vector with the cumulative number of cases.
}
\item{ti}{
A vector with the time, e.g. days.
}
\item{ini}{
A vector with the initial values. The length varies according to the type of growth model.
}
\item{type}{
The type of the growth model. "richards", "3logistic", "4logistic", "5logistic", "gompertz" or "weibull". See Reddy et al. (2021) for more information.

For the Richards model a vector with the parameters \eqn{(\alpha, \gamma, \eta, \kappa)}.

For the 3 parameter logistic model a vector with the parameters \eqn{(\alpha, \gamma, \eta)}.

For the 4 parameter logistic model a vector with the parameters \eqn{(\alpha, \beta, \gamma, \eta)}.

For the 5 parameter logistic model a vector with the 5 parameters \eqn{(\alpha, \alpha_0, \kappa, \gamma, \eta)}.

For the Gompertz model a vector with 4 parameters \eqn{(\alpha, \alpha_0, \gamma, \eta)}.

For the Weibull model a vector with 3 parameters \eqn{(\alpha, \alpha_0, \eta)}.
}
\item{ti.ahead}{
If you want to make predictions for a number of days ahead, provide the number of days, a single number.
}
}

\details{
Non-linear growth curves are fitted using least squares.

Richards model:
\deqn{
y(t)=\alpha[1+\kappa e^{\gamma/\kappa(t-\eta)}].
}

3 parameter logistic model:
\deqn{
y(t)=\frac{\alpha}{1+e^{-\gamma(t-\eta)}}.
}

4 parameter logistic model:
\deqn{
y(t)=\beta+\frac{\alpha-\beta}{1+e^{-\gamma(t-\eta)}}.
}

5 parameter logistic model:
\deqn{
y(t)=\alpha+\frac{\alpha_0 - \alpha}{[1+(2^{1/\kappa}-1)(\frac{t}{\gamma})^{\gamma}]^{\kappa}}.
}

Gompertz model:
\deqn{
y(t)=\alpha_0 + (\alpha-\alpha_0)e^{-e^{-(\gamma(t-\eta))}}.
}

Weibull model:
\deqn{
y(t)=\alpha_0 + (\alpha-\alpha_0)e^{-(\frac{t}{\eta})^{\kappa}}.
}

}

\value{
A list including:
\item{mod}{
The object as returned by the function \code{\link{nls}}.
}
\item{param}{
A vector with the parameters of the chosen model.
}
\item{fit}{
The fitted values.
}
\item{pred}{
The future predictions if values for the argument "ti.ahead" were given, otherwise this is NULL.
}
}

\references{
Reddy T., Shkedy Z., van Rensburg C. J., Mwambi H., Debba P., Zuma K. and Manda, S. (2021).
Short-term real-time prediction of total number of reported COVID-19 cases and deaths in South Africa: a data driven approach. BMC Medical Research Methodology, 21(1), 1-11.
}

\author{
Michail Tsagris and Nikolaos Pandis.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr} and Nikolaos Pandis \email{npandis@yahoo.com}.
}

\seealso{
\code{ \link{boot.pred}, \link{fit.plot}
}
}

\examples{
## Data on Covid 19 for the first 96 days of Belgium
y <- c( 19, 38, 72, 125, 206, 316, 343, 407, 501, 600, 774, 1024, 1362, 1541, 1755, 2142,
        2564, 3098, 3811, 4473, 4942, 5428, 6756, 7951, 9150, 10513, 12031, 12875, 13558,
        15296, 16977, 18493, 19971, 21665, 22587, 23252, 25186, 26701, 28299, 30538, 32874,
        33903, 34427, 34964, 36524, 38157, 39831, 41225, 41947, 42390, 43666, 44936, 45713,
        46689, 47500, 47888, 48093, 48848, 49417, 49939, 50525, 50762, 51048, 51188, 51858,
        52404, 52956, 53398, 53881, 54121, 54239, 54715, 55110, 55431, 55736, 56082, 56229,
        56310, 56627, 56919, 57304, 57374, 57622, 57745, 57820, 58134, 58336, 58518, 58689,
        58854, 58915, 58964, 59023, 59204, 59363, 59535 )
ti <- 1:96
## Apply the 4-parameter logistic model
mod1 <- nlgm(y, ti, type = "4logistic", ini = c(60000, 1, 1, 35) )
## predict the cases 10 days ahead
mod2 <- nlgm(y, ti, type = "4logistic", ini = c(60000, 1, 1, 35), ti.ahead = 10 )

}


