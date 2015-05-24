\name{GBT.try.params.uniform.step1}
\alias{GBT.try.params.uniform.step1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GBT.try.params.uniform.step1(Y, X, par.range, num.repeats)
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
function (Y, X, par.range, num.repeats) 
{
    temp.params <- GBT.generate.params.uniform.step1(par.range = par.range)
    eta.par <- as.numeric(temp.params["eta.par"])
    max.depth.par <- as.integer(temp.params["max.depth.par"])
    colsample.bytree.par <- as.numeric(temp.params["colsample.bytree.par"])
    subsample.par <- as.numeric(temp.params["subsample.par"])
    nrounds.par <- as.integer(temp.params["nrounds.par"])
    gamma.par <- 0
    min.child.weight.par <- 1
    max.delta.step.par <- 0
    temp.tune <- data.frame()
    for (r in 1:num.repeats) {
        cat("\n***\n")
        cat("\nRep", r, "of", num.repeats, "\n")
        train.idx <- sample(1:nrow(X), floor(nrow(X) * 0.7))
        valid.idx <- setdiff(c(1:nrow(X)), train.idx)
        dtrain <- xgb.DMatrix(data = X[train.idx, ], label = Y[train.idx])
        dvalid <- xgb.DMatrix(data = X[valid.idx, ])
        param <- list(objective = "binary:logistic", eval_metric = "auc", 
            eta = eta.par, gamma = gamma.par, max_depth = max.depth.par, 
            min_child_weight = min.child.weight.par, max_delta_step = max.delta.step.par, 
            colsample_bytree = colsample.bytree.par, subsample = subsample.par, 
            booster = "gbtree")
        bst <- xgboost(param = param, data = dtrain, nround = nrounds.par)
        raw <- xgb.save.raw(bst)
        rm(bst)
        gc()
        bst <- xgb.load(raw)
        preds <- predict(bst, dvalid)
        bst.auc <- Metrics::auc(Y[valid.idx], preds)
        cat("\nAUC", round(bst.auc, 4), "\n")
        cat("\n***\n\n\n")
        bst.res <- cbind(bst.auc, eta.par, gamma.par, max.depth.par, 
            min.child.weight.par, max.delta.step.par, colsample.bytree.par, 
            subsample.par, nrounds.par)
        temp.tune <- rbind(temp.tune, bst.res)
    }
    temp.params <- names(temp.tune)[-1]
    temp.tune.final <- ddply(temp.tune, c(temp.params), summarize, 
        auc = mean(bst.auc), auc.sd = sd(bst.auc))
    return(temp.tune.final)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
