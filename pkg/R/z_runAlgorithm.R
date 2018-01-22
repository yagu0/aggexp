#' @include b_Algorithm.R

algoNameDictionary = list(
	ew = "ExponentialWeights",
	kn = "KnearestNeighbors",
	ga = "GeneralizedAdditive",
	ml = "MLpoly",
	rt = "RegressionTree",
	rr = "RidgeRegression",
	sv = "SVMclassif"
)

#' @title Simulate real-time predict
#'
#' @description Run the algorithm coded by \code{shortAlgoName} on data specified by the \code{stations} argument.
#'
#' @param shortAlgoName Short name of the algorithm.
#' \itemize{
#'   \item ew : Exponential Weights
#'   \item ga : Generalized Additive Model
#'   \item kn : K Nearest Neighbors
#'   \item ml : MLpoly
#'   \item rt : Regression Tree
#'   \item rr : Ridge Regression
#' }
#' @param stations List of stations dataframes to consider.
#' @param experts Vector of experts to consider (names).
#' @param ... Additional arguments to be passed to the Algorithm object.
#'
#' @return A list with the following slots
#' \itemize{
#'   \item{data : data frame of all forecasts + measures (may contain NAs) + predictions, with date and station indices.}
#'   \item{algo : object of class \code{Algorithm} (or sub-class).}
#'   \item{stations : list of dataframes of stations for this run.}
#'   \item{experts : character vector of experts for this run.}
#' }
#'
#' @examples
#' data(stations)
#' r = runAlgorithm("ew", list(st[[1]]), c("P","MA3"))
#' plotCurves(r)
#' r2 = runAlgorithm("ml", st[c(1,2)], c("MA3","MA10"))
#' plotError(r2)
#' @export
runAlgorithm = function(shortAlgoName, stations, experts, ...)
{
	#very basic input checks
	if (! shortAlgoName %in% names(algoNameDictionary))
		stop("Unknown algorithm:")
	experts = unique(experts)

	#get data == ordered date indices + forecasts + measures + stations indices (would be DB in prod)
	oracleData = getData(stations, experts)

	#simulate incremental forecasts acquisition + prediction + get measure
	algoData = as.data.frame(matrix(nrow=0, ncol=ncol(oracleData)))
	names(algoData) = names(oracleData)
	algorithm = new(algoNameDictionary[[shortAlgoName]], data=algoData, ...)
	predictions = c()
	T = oracleData[nrow(oracleData),"Date"]
	for (t in 1:T)
	{
		#NOTE: bet that subset extract rows in the order they appear
		tData = subset(oracleData, subset = (Date==t))
		algorithm$inputNextForecasts(tData[,names(tData) != "Measure"])
		predictions = c(predictions, algorithm$predict_withNA())
		algorithm$inputNextObservations(tData[,"Measure"])
	}

	oracleData = cbind(oracleData, Prediction = predictions)
	return (list(data = oracleData, algo = algorithm, experts = experts, stations = stations))
}
