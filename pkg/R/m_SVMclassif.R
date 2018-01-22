#' @include b_Algorithm.R

#' @title SVM Algorithm
#'
#' @description SVM classifier.
#' Inherits \code{\link{Algorithm}}
#'
#' @field kernel TODO
#' @field someParam TODO
#'
SVMclassif = setRefClass(
	Class = "SVMclassif",

	fields = c(
		kernel = "numeric",
		someParam = "logical"
	),

	contains = "Algorithm",

	methods = list(
		initialize = function(...)
		{
			callSuper(...)
			#TODO
		},
		predict_noNA = function(XY, x)
		{
			if (nrow(XY) <= 5)
				return (10) #TODO

			require(kernlab, quietly=TRUE)
			XY[,"alert"] = XY[,"Measure"] > 30
			alertsIndices = XY[,"alert"]
			XY[alertsIndices,"alert"] = "alert"
			XY[!alertsIndices,"alert"] = "noalert"
			XY[,"alert"] = as.factor(XY[,"alert"])
			XY[,"Measure"] = NULL

			ks = ksvm(alert ~ ., data=XY)
			pred = as.character(predict(ks, as.data.frame(x)))
			pred[pred == "alert"] = 70
			pred[pred == "noalert"] = 10
			return (as.numeric(pred))
		}
	)
)
