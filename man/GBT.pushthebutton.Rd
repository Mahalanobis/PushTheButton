\name{GBT.pushthebutton}
\alias{GBT.pushthebutton}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GBT.pushthebutton(Y, X, par.range, num.iterations, num.repeats)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{par.range}{
%%     ~~Describe \code{par.range} here~~
}
  \item{num.iterations}{
%%     ~~Describe \code{num.iterations} here~~
}
  \item{num.repeats}{
%%     ~~Describe \code{num.repeats} here~~
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
function (Y, X, par.range, num.iterations, num.repeats) 
{
    temp.tune <- data.frame()
    for (i in 1:num.iterations) {
        temp <- GBT.try.params.uniform.step1(Y = Y, X = X, par.range = par.range, 
            num.repeats = num.repeats)
        temp.tune <- rbind(temp.tune, temp)
    }
    temp.tune <- temp.tune[order(-temp.tune$auc), ]
    return(temp.tune)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
