
### GENERATE PARAMETERS

# Gradient Boosting Trees 
GBT.generate.params.step1.uniform <- function( par.range ){

eta.par.dist <- ( par.range[which(rownames(par.range)=="eta.par"),2] - par.range[which(rownames(par.range)=="eta.par"),1] )/20
eta.par.quali	<- seq(from=par.range[which(rownames(par.range)=="eta.par"),1], to=par.range[which(rownames(par.range)=="eta.par"),2], by=eta.par.dist)
eta.par.prob <- rev(log(eta.par.quali+1)/sum(log(eta.par.quali+1)))
eta.par <- sample( eta.par.quali , size=1 , prob=eta.par.prob)

max.depth.par <- sample( seq(from=round(par.range[which(rownames(par.range)=="max.depth.par"),1],0), to=round(par.range[which(rownames(par.range)=="max.depth.par"),2],0),by=1) , 1 )

colsample.bytree.par <- sample( seq(from=par.range[which(rownames(par.range)=="colsample.bytree.par"),1], to=par.range[which(rownames(par.range)=="colsample.bytree.par"),2], by=0.05) , 1 )

subsample.par <- sample( seq(from=par.range[which(rownames(par.range)=="subsample.par"),1], to=par.range[which(rownames(par.range)=="subsample.par"),2], by=0.05) , 1 )

nrounds.par.quali <- seq(from=round(par.range[which(rownames(par.range)=="nrounds.par"),1],0), to=round(par.range[which(rownames(par.range)=="nrounds.par"),2],0),by=1)
nrounds.par.prob <- rev(log(nrounds.par.quali+1)/sum(log(nrounds.par.quali+1)))
nrounds.par <- sample( nrounds.par.quali , size=1 , prob=nrounds.par.prob)


top.params <- list(	eta.par = eta.par,
			max.depth.par = max.depth.par,
			colsample.bytree.par = colsample.bytree.par,
			subsample.par = subsample.par,
			nrounds.par = nrounds.par )

return(top.params)

}

