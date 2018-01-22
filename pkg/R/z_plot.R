#' @include z_plotHelper.R

#' @title Plot forecasts/observations
#'
#' @description Plot the measures at one station versus all experts forecasts.
#'
#' @param r Output of \code{\link{runAlgorithm}}.
#' @param station Name or index of the station to consider. Default: the first one
#' @param interval Time interval for the plot. Default: all time range.
#' @param experts Subset of experts for the plot. Default: all experts.
#' @param ... Additional arguments to be passed to graphics::plot method.
#'
#' @export
plotCurves = function(r, station=1, interval=1:(nrow(r$data)/length(r$stations)), experts=r$experts, cols=rainbow(length(experts)), ...)
{
	if (is.character(station))
		station = match(station, r$stations)
	if (is.numeric(experts))
		experts = r$experts[experts]

	XY = subset(r$data[interval,], subset = (Station == station), select = c(experts,"Measure"))
	indices = getNoNAindices(XY)
	XY = XY[indices,]
	X = as.matrix(XY[,names(XY) %in% experts])
	Y = XY[,"Measure"]

	yRange = range(XY)
	par(mar=c(5,4.5,1,1), cex=1.5)
	for (i in 1:length(experts))
	{
		plot(X[,i],ylim=yRange,type="l",lty="dotted",col=cols[i],xlab="",ylab="",xaxt="n",yaxt="n", lwd=2, ...)
		par(new=TRUE)
	}
	plot(Y, type="l", ylim=yRange, xlab="", ylab="", lwd=2, cex.axis=1.5, ...)
	title(xlab="Time",ylab="Forecasts / Measures", cex.lab=1.6)
	legend("topright", lwd=c(2,1),lty=c("solid","dotted"),horiz=TRUE,legend=c("Measures","Forecasts"))
}

#' @title Plot error
#'
#' @description Plot the absolute error over time at one station.
#'
#' @param r Output of \code{\link{runAlgorithm}}.
#' @param station Name or index of the station to consider. Default: the first one
#' @param start First index to consider (too much variability in early errors)
#' @param noNA TRUE to show only errors associated with full lines (old behavior)
#' @param ... Additional arguments to be passed to graphics::plot method.
#'
#' @export
plotError = function(r, station=1, start=1, noNA=TRUE, ...)
{
	if (is.character(station))
		station = match(station, r$stations)

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

	error = abs(Y - hatY)
	par(mar=c(5,4.5,1,1), cex=1.5)
	plot(error, type="l", xaxt="n", xlab="Time",ylab="L1 error", cex.lab=1.6, cex.axis=1.5, ...)
	axis(side=1, at=(seq(from=start,to=length(Y),by=30) - start), labels=seq(from=start,to=length(Y),by=30), cex.axis=1.5)
}

#' @title Plot regret
#'
#' @description Plot the regret over time at one station.
#'
#' @param r Output of \code{\link{runAlgorithm}}.
#' @param vs Linear weights to compare with. Can be obtained by the \code{getBestXXX} methods, or by any other mean.
#' @param station Name or index of the station to consider. Default: the first one
#' @param start First index to consider (too much variability in early errors)
#' @param ... Additional arguments to be passed to graphics::plot method.
#'
#' @export
plotRegret = function(r, vs, station=1, start=1, ...)
{
	if (is.character(station))
		station = match(station, r$stations)

	XY = subset(r$data, subset = (Station == station), select = c(r$experts,"Measure","Prediction"))
	X = XY[,names(XY) %in% r$experts]
	Y = XY[,"Measure"]
	hatY = XY[,"Prediction"]

	indices = !is.na(Y) & !is.na(hatY) & getNoNAindices(X)
	X = as.matrix(X[indices,])
	Y = Y[indices]
	hatY = hatY[indices]

	error2 = abs(Y - hatY)^2
	vsError2 = abs(Y - X %*% vs)^2
	cumErr2 = cumsum(error2) / seq_along(error2)
	cumVsErr2 = cumsum(vsError2) / seq_along(vsError2)
	regret = cumErr2 - cumVsErr2

	par(mar=c(5,4.5,1,1), cex=1.5)
	plot(regret, type="l", xaxt="n", xlab="Time", ylab="Regret", cex.lab=1.6, cex.axis=1.5, ...)
	abline(a=0., b=0., col=2)
	axis(side=1, at=(seq(from=start,to=length(Y),by=30) - start), labels=seq(from=start,to=length(Y),by=30), cex.axis=1.5)
}

#' @title Plot predicted/expected cloud
#'
#' @description Plot the cloud of forecasts/observations + statistical indicators.
#'
#' @param r Output of \code{\link{runAlgorithm}}.
#' @param thresh Threshold to consider for alerts (usually 30 or 50)
#' @param hintThresh thresholds to draw on the plot to help visualization. Often \code{c(30,50,80)}
#' @param station Name or index of the station to consider. Default: the first one
#' @param noNA TRUE to show only errors associated with full lines (old behavior)
#' @param ... Additional arguments to be passed to graphics::plot method.
#'
#' @export
plotCloud = function(r, thresh=30, hintThresh=c(30,50,80), station=1, noNA=TRUE, ...)
{
	if (is.character(station))
		station = match(station, r$stations)

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

	indics = getIndicators(r, thresh, station, noNA)

	par(mar=c(5,5,3,2), cex=1.5)
	plot(Y, hatY, xlab="Measured PM10", ylab="Predicted PM10",
		cex.lab=1.6, cex.axis=1.5, xlim=c(0,120), ylim=c(0,120), ...)
	abline(0,1,h=hintThresh,v=hintThresh,col=2,lwd=2)
	legend("topleft",legend=paste("RMSE ",indics$RMSE))
	legend("bottomright",legend=c(paste("TS ",indics$TS)))
}
