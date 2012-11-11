\name{dailyPower}
\alias{dailyPower}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dailyPower(dayData)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dayData}{
%%     ~~Describe \code{dayData} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dayData) 
{
    rows <- nrow(dayData)
    evenRow <- seq(2, rows, by = 1)
    oddRow <- seq(1, rows - 1, by = 1)
    timeLag <- as.numeric(with(dayData, difftime(time[evenRow], 
        time[oddRow], units = "hours")))
    powerColumns <- grep("^pot", names(dayData), value = TRUE)
    tmp.power <- matrix(data = NA, nrow = rows - 1, ncol = length(powerColumns), 
        dimnames = list(c(), powerColumns))
    tmp.power <- sapply(powerColumns, function(x) {
        tmp.power[, x] <- (dayData[evenRow, x] + dayData[oddRow, 
            x])/2 * timeLag
    })
    cbind(as.data.frame(as.list(colSums(tmp.power/1000))), dayData[1, 
        grep("^year", colnames(dayData), value = TRUE)])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
