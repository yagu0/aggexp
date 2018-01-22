#' @include b_LinearAlgorithm.R

#' @title Ridge Regression Algorithm
#'
#' @description Ridge Regression Algorithm.
#' Inherits \code{\link{LinearAlgorithm}}
#'
#' @field lambda Value of lambda (let undefined for cross-validation). Default: undefined
#' @field lambdas Vector of "optimal" lambda values over time. TODO: remove for production
#'
RidgeRegression = setRefClass(
	Class = "RidgeRegression",

	fields = c(
		lambda = "numeric",
		lambdas = "numeric"
	),

	contains = "LinearAlgorithm",
	
	methods = list(
		predict_noNA = function(XY, x)
		{
			if (length(lambda) > 0 || nrow(XY) < 30) #TODO: magic number
			{
				#simple ridge regression with fixed lambda (not enough history for CV)
				X = matricize(XY[,names(XY) != "Measure"])
				Y = XY[,"Measure"]
				lambda_ = ifelse(length(lambda) > 0, lambda, LAMBDA)
				weight = ridgeSolve(X, Y, lambda_)
			}

			else
			{
				#enough data for cross-validations
				require(MASS, quietly=TRUE)
				gridLambda = seq(0.05,5.05,0.1)
				res_lmr = lm.ridge(Measure ~ . + 0, data=XY, lambda = gridLambda)
				lambda_ = res_lmr$lambda[which.min(res_lmr$GCV)]
				weight = as.matrix(coef(res_lmr))[which.min(res_lmr$GCV),]
			}

			lambdas <<- c(lambdas, lambda_)

			appendWeight(weight)
			return (matricize(x) %*% weight)
		}
	)
)
