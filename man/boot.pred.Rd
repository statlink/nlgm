\name{Parametric bootstrap prediction intervals}
\alias{boot.pred}
\title{
Parametric bootstrap prediction intervals
}

\description{
Parametric bootstrap prediction intervals.
}

\usage{
boot.pred(mod, type, ti, ti.ahead = 10, B = 1000, conf = 0.95, seed = NULL)
}

\arguments{
\item{mod}{
The "nls" object of the \code{\link{nlgm}} function.
}
\item{type}{
The type of the growth model. "richards", "3logistic", "4logistic", "5logistic", "gompertz" or "weibull".
See Reddy et al. (2021) for more information.
}
\item{ti}{
A vector with the time, e.g. days.
}
\item{ti.ahead}{
The future time points.
}
\item{B}{
The number of boostrap samples to draw. These samples are drawn from a Poisson distribution.
}
\item{conf}{
The prediction level, set to 95\% by default.
}
\item{seed}{
Provide a seed number if you want, otherwise leave it NULL.
}
}

\details{
Non-linear growth curves are fitted using least squares. Based on the model a parametric bootstrap is applied in order
to construct prediction intervals. The fitted values act as the mean from which Poisson samples are drawn and the nlgm
is fitted. Using this fitted model, the predicted number of cumulative cases, at the selected number of days ahead, are calculated.
This process is repated B times and in the end the prediction interval is computed by returning the tails of this bootstrap
predicted values distribution.
}

\value{
A list including:
\item{est}{
A column with 3 columns, the mean of the bootstrap based preditions and the prediction interval.
}
\item{pred}{
A matrix with B columns. Each column represents a bootstrap based set of prediction values.
}
}

\references{
Reddy T., Shkedy Z., van Rensburg C. J., Mwambi H., Debba P., Zuma K. and Manda, S. (2021). Short-term real-time prediction of total number of reported COVID-19 cases and deaths in South Africa: a data driven approach. BMC Medical Research Methodology, 21(1), 1-11.
}

\author{
Michail Tsagris and Nikolaos Pandis.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr} and Nikolaos Pandis \email{npandis@yahoo.com}.
}

\seealso{
\code{ \link{nlgm}, \link{fit.plot}
}
}

\examples{
## Data on the 96 first days of Belgium
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
mod <- nlgm(y, ti, type = "4logistic", ini = c(60000, 1, 1, 35) )
preds <- boot.pred(mod, type = "4logistic", ti = ti, B = 100)
}


