
### GENERATE PARAMETERS

# Gradient Boosting Trees - Uniform Step1
GBT.generate.params.uniform.step1 <- function( par.range ){

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



### ONE SHOT MODEL

# Gradient Boosting Trees - Uniform Step1
GBT.try.params.uniform.step1 <- function( num.repeats , par.range ){

temp.params <- GBT.generate.params.uniform.step1(par.range=par.range)

eta.par		 	<- as.numeric(temp.params['eta.par'])
max.depth.par		<- as.integer(temp.params['max.depth.par'])
colsample.bytree.par	<- as.numeric(temp.params['colsample.bytree.par'])
subsample.par		<- as.numeric(temp.params['subsample.par'])
nrounds.par 		<- as.integer(temp.params['nrounds.par'])

gamma.par		<- 0
min.child.weight.par	<- 1
max.delta.step.par	<- 0

temp.tune  <- data.frame()

for( r in 1:num.repeats ){

	cat("\n***\n")
	cat("\nRep",r,"of",num.repeats,"\n")
	
train.idx <- sample(1:nrow(X),floor(nrow(X)*0.7))
valid.idx <- setdiff(c(1:nrow(X)),train.idx)

dtrain <- xgb.DMatrix(data = X[train.idx,], label = Y[train.idx])
dvalid <- xgb.DMatrix(data = X[valid.idx,])

param <- list(	"objective" = "binary:logistic",
		"eval_metric" = "auc",
		"eta" = eta.par,
		"gamma" = gamma.par,
		"max_depth" = max.depth.par,
		"min_child_weight" = min.child.weight.par,
		"max_delta_step" = max.delta.step.par,
		"colsample_bytree" = colsample.bytree.par,
		"subsample" = subsample.par,
		"booster" = "gbtree")	
				
bst <- xgboost(param=param, data = dtrain, nround = nrounds.par)

raw <- xgb.save.raw(bst)
rm(bst)
gc()
bst <- xgb.load(raw)
preds <- predict(bst,dvalid)	
bst.auc <- Metrics::auc(Y[valid.idx],preds)	

	cat("\nAUC",round(bst.auc,4),"\n")
	cat("\n***\n\n\n")
		
bst.res <- cbind(	bst.auc,
					eta.par,
					gamma.par,
					max.depth.par,
					min.child.weight.par,
					max.delta.step.par,
					colsample.bytree.par,
					subsample.par,
					nrounds.par )
				
temp.tune <- rbind(temp.tune,bst.res)

}

temp.params <- names(temp.tune)[-1]
temp.tune.final <- ddply(temp.tune,c(temp.params),summarize,auc=mean(bst.auc),auc.sd=sd(bst.auc))
return(temp.tune.final)

}



