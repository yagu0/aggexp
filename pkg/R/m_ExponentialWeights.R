#' @include b_LinearAlgorithm.R

#' @title Exponential Weights Algorithm
#'
#' @description Exponential Weights Algorithm.
#' Inherits \code{\link{LinearAlgorithm}}
#'
#' @field alpha Importance of weights redistribution, in [0,1]. Default: 0
#' @field grad Whether to use or not the (sub)gradient trick. Default: FALSE
#'
ExponentialWeights = setRefClass(
	Class = "ExponentialWeights",

	fields = c(
		alpha = "numeric",
		grad = "logical"
	),

	contains = "LinearAlgorithm",

	methods = list(
		initialize = function(...)
		{
			callSuper(...)
			if (length(alpha) == 0 || alpha < 0. || alpha > 1.)
				alpha <<- 0. #no redistribution
			if (length(grad) == 0)
				grad <<- FALSE
		},
		predict_noNA = function(XY, x)
		{
			K = ncol(XY) - 1
			if (K == 1)
			{
				#shortcut: nothing to combine
				finalWeight = 1.
			}

			else
			{
				X = XY[,names(XY) != "Measure"]
				Y = XY[,"Measure"]
				finalWeight = .C("ew_predict_noNA", X = as.double(t(X)), Y = as.double(Y), n = as.integer(nrow(XY)), 
					K = as.integer(K), alpha=as.double(alpha), grad = as.integer(grad), weight=double(K))$weight
			}

			appendWeight(finalWeight)
			return (matricize(x) %*% finalWeight)
		}
	)
)
