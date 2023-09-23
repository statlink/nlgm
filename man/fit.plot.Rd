\name{Plot of the fitted values of the non-linear growth models}
\alias{fit.plot}
\title{
Plot of the fitted values of the non-linear growth models
}

\description{
Plot of the fitted values of the non-linear growth models.
}

\usage{
fit.plot(mat)
}

\arguments{
\item{mat}{
A data.frame object or a matrix with three columns at this specific order, the observed cumulative cases, the time and
the fitted values from a model. See the examples below for an illustration.
}
}

\details{
The scatter plot of the cumulative cases versus time is plotted. Then, lines with the fitted cases are added.
}

\value{
A scatter plot of the cumulative cases versus time, including lines with the fitted cases.
}

\references{
Reddy T., Shkedy Z., van Rensburg C. J., Mwambi H., Debba P., Zuma K. and Manda, S. (2021).
Short-term real-time prediction of total number of reported COVID-19 cases and deaths in South Africa: a data driven approach.
BMC Medical Research Methodology, 21(1), 1-11.
}

\author{
Nikolaos Pandis and Michail Tsagris.

R implementation and documentation: Nikolaos Pandis \email{npandis@yahoo.com} and Michail Tsagris \email{mtsagris@uoc.gr}.
}

\seealso{
\code{ \link{nlgm}, \link{boot.pred}
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
mod <- nlgm(y, ti, type = "4logistic", ini = c(60000, 1, 1, 35) )
mat <- cbind(y, ti, mod$fit)
fit.plot(mat)
}


