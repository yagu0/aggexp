#' @include b_Algorithm.R

#' @title K Nearest Neighbors Algorithm
#'
#' @description K Nearest Neighbors Algorithm.
#' Inherits \code{\link{Algorithm}}
#'
#' @field k Number of neighbors to consider. Default: \code{n^(2/3)}
#'
KnearestNeighbors = setRefClass(
	Class = "KnearestNeighbors",

	fields = c(
		k = "numeric"
	),

	contains = "Algorithm",

	methods = list(
		predictOne = function(X, Y, x)
		{
			"Find the neighbors of one row, and solve a constrained linear system to obtain weights"

			distances = sqrt(apply(X, 1, function(z)(return (sum((z-x)^2)))))
			rankedHistory = sort(distances, index.return=TRUE)
			n = length(Y)
			k_ = ifelse(length(k) == 0 || k <= 0. || k > n, getKnn(n), as.integer(k))
			weight = ridgeSolve(matricize(X[rankedHistory$ix[1:k_],]), Y[rankedHistory$ix[1:k_]], LAMBDA)

			return (sum(x * weight))
		},
		predict_noNA = function(XY, x)
		{
			X = XY[,names(XY) != "Measure"]
			K = ncol(XY) - 1
			if (K == 1)
				X = as.matrix(X)
			else if (length(XY[["Measure"]]) == 1)
				X = t(as.matrix(X))
			Y = XY[,"Measure"]
			x = matricize(x)
			res = c()
			for (i in 1:nrow(x))
				res = c(res, predictOne(X, Y, x[i,]))
			return (res)
		}
	)
)
