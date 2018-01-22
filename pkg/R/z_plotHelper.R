#' @include z_runAlgorithm.R

#' @title Get best expert index
#'
#' @description Return the weights corresponding to the best expert (...0,1,0...)
#'
#' @param r Output of \code{\link{runAlgorithm}}
#'
#' @export
getBestExpert = function(r)
{
	X = as.matrix(r$data[,names(r$data) %in% r$experts])
	Y = r$data[,"Measure"]

	bestIndex = which.min(colMeans(abs(X - Y)^2, na.rm=TRUE))
	res = rep(0.0, length(r$experts))
	res[bestIndex] = 1.0
	return (res)
}

#' @title Get best convex combination
#'
#' @description Return the weights p minimizing the quadratic error ||X*p-Y||^2 under convexity contraint.
#'
#' @param r Output of \code{\link{runAlgorithm}}
#'
#' @export
getBestConvexCombination = function(r)
{
	X = r$data[,r$experts]
	Y = as.double(r$data[,"Measure"])
	indices = getNoNAindices(X) & !is.na(Y)
	X = as.matrix(X[indices,])
	Y = Y[indices]

	K = length(r$experts)
	return (constrOptim(theta=rep(1.0/K,K),
		method="Nelder-Mead", #TODO: others not better... why?
		f=function(p){return(sum((X%*%p-Y)^2))}, 
		grad=NULL, #function(p){return(2.*t(X)%*%(X%*%p-Y))}, 
		ui=rbind(rep(1.,K),rep(-1.,K),diag(K)), ci=c(0.99999,-1.00001, rep(0.,K)), 
		control=list(ndeps=1e-3,maxit=10000))$par)
}

#' @title Get best linear combination
#'
#' @description Return the weights u minimizing the quadratic error ||r$X*u-r$Y||^2
#'
#' @param r Output of \code{\link{runAlgorithm}}
#'
#' @export
getBestLinearCombination = function(r)
{
	X = r$data[,r$experts]
	Y = r$data[,"Measure"]
	indices = getNoNAindices(X) & !is.na(Y)
	X = as.matrix(X[indices,])
	Y = Y[indices]

	return (mpPsInv(X) %*% Y)
}

#' @title Get statistical indicators
#'
#' @description Return respectively the TS, FA, MA, RMSE, EV indicators in a list.
#'
#' @param r Output of \code{\link{runAlgorithm}}
#' @param thresh Threshold to compute alerts indicators.
#' @param station Name or index of the station to consider. Default: the first one
#' @param noNA TRUE to show only errors associated with full lines (old behavior)
#'
#' @export
getIndicators = function(r, thresh, station=1, noNA=TRUE)
{
	if (is.character(station))
		station = match(station, r$stations)

	#TODO: duplicated block (same in plotCloud())
	XY = subset(r$data, subset = (Station == station), select = c(r$experts,"Measure","Prediction"))
	Y = XY[,"Measure"]
	hatY = XY[,"Prediction"]
	indices = !is.na(Y) & !is.na(hatY)
	if (noNA)
	{
		X = XY[,names(XY) %in% r$experts]
		indices = indices & getNoNAindices(X)
	}
	Y = Y[indices]
	hatY = hatY[indices]

	RMSE = round(sqrt(sum((Y - hatY)^2) / length(Y)),2)
	EV = round(1 - var(Y-hatY) / var(Y), 2)
	A = sum(hatY >= thresh & Y >= thresh, na.rm=TRUE) #right alarm
	B = sum(hatY >= thresh & Y < thresh, na.rm=TRUE) #false alarm
	C = sum(hatY < thresh & Y >= thresh, na.rm=TRUE) #missed alert
	TS = round(A/(A+B+C),2)
	FA = B/(A+B)
	MA = C/(A+C)
	return (list("TS"=TS, "FA"=FA, "MA"=MA, "RMSE"=RMSE, "EV"=EV))
}
