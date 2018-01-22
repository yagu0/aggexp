#' @include b_Algorithm.R

#' @title Regression Tree
#'
#' @description Regression Tree using the \code{tree} package.
#' Inherits \code{\link{Algorithm}}
#'
#' @field nleaf Number of leaf nodes after pruning. Default: Inf (no pruning)
#'
RegressionTree = setRefClass(
	Class = "RegressionTree",

	fields = c(
		nleaf = "numeric"
	),

	contains = "Algorithm",

	methods = list(
		initialize = function(...)
		{
			callSuper(...)
			if (length(nleaf) == 0 || nleaf < 1)
				nleaf <<- Inf
		},
		predict_noNA = function(XY, x)
		{
			require(tree, quietly=TRUE)
			rt = tree(Measure ~ ., data=XY)
			treeSize = sum( rt$frame[["var"]] == "<leaf>" )
			if (treeSize > nleaf)
				rt = prune.tree(rt, best = nleaf)
			return (stats::predict(rt, as.data.frame(x)))
		}
	)
)
