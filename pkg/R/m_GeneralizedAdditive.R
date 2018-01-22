#' @include b_Algorithm.R

#' @title Generalized Additive Model
#'
#' @description Generalized Additive Model using the \code{gam} package.
#' Inherits \code{\link{Algorithm}}
#'
#' @field family Family of the distribution to be used. Default: gaussian().
#'
GeneralizedAdditive = setRefClass(
	Class = "GeneralizedAdditive",

	fields = c(
		"family" #class "family"
	),

	contains = "Algorithm",

	methods = list(
		initialize = function(...)
		{
			callSuper(...)
			if (class(family) == "uninitializedField")
				family <<- gaussian()
		},
		predict_noNA = function(XY, x)
		{
			#GAM need some data to provide reliable results
			if (nrow(XY) < 30)
			{
				X = XY[,names(XY) != "Measure"]
				Y = XY[,"Measure"]
				weight = ridgeSolve(X, Y, LAMBDA)
				return (matricize(x) %*% weight)
			}

			suppressPackageStartupMessages( require(gam) )
			g = gam(Measure ~ ., data=XY, family=family)
			return (stats::predict(g, x))
		}
	)
)
