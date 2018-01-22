#' @include b_Algorithm.R

#' @title Linear Algorithm
#'
#' @description Generic class to represent a linear algorithm. 
#' TODO: not needed in production environment; weights growing infinitely. 
#' Inherits \code{\link{Algorithm}}
#'
#' @field weights The matrix of weights (in rows) associated to each expert (in columns)
#'
LinearAlgorithm = setRefClass(
	Class = "LinearAlgorithm",

	fields = c(
		weights = "matrix"
	),

	contains = "Algorithm",

	methods = list(
		initialize = function(...)
		{
			callSuper(...)
			weights <<- matrix(nrow=0, ncol=ncol(data)-3)
		},

		appendWeight = function(weight)
		{
			"Append the last computed weights to the weights matrix, for further plotting"

			n = nrow(data)
			nx = n - nrow(subset(data, subset = (Date == data[n,"Date"])))
			x = data[(nx+1):n, !names(data) %in% c("Date","Measure","Station")]
			iy = getNoNAindices(x, 2)

			completedWeight = rep(NA, ncol(x))
			completedWeight[iy] = weight
			weights <<- rbind(weights, completedWeight)
		},

		plotWeights = function(station=1, start=1, ...)
		{
			"Plot the weights of each expert over time"

			if (is.character(station))
				station = match(station, stations)

			#keep only full weights (1 to K)
			weights_ = weights[getNoNAindices(weights),]
			weights_ = weights_[start:nrow(weights_),]

			yRange = range(weights_, na.rm=TRUE)
			K = ncol(weights_)
			cols = rainbow(K)
			par(mar=c(5,4.5,1,1), cex=1.5)
			for (i in 1:K)
			{
				plot(weights_[,i], type="l", xaxt="n", ylim=yRange, col=cols[i], xlab="", ylab="",cex.axis=1.5, ...)
				par(new=TRUE)
			}
			axis(side=1, at=seq(from=1,to=nrow(weights_),by=30), labels=seq(from=0,to=nrow(weights_),by=30) + start, cex.axis=1.5)
			title(xlab="Time",ylab="Weight", cex.lab=1.6)
		}
	)
)
