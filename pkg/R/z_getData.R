#' @title Get forecasts + observations
#'
#' @description Get forecasts of all specified experts for all specified stations, also with (ordered) dates and (unordered) stations indices.
#'
#' @param station List of stations dataframes (as in the sample)
#' @param experts Names of the experts (as in dataframe header)
#'
#' @export
getData = function(stations, experts)
{
	data = as.data.frame(matrix(nrow=0, ncol=1 + length(experts) + 2))
	names(data) = c("Date", experts, "Measure", "Station")
	for (i in 1:length(stations))
	{
		#date index is sufficient; also add station index
		stationInfo = cbind(
			Date = 1:nrow(stations[[i]]),
			stations[[i]] [,names(stations[[i]]) %in% experts],
			Measure = stations[[i]][,"Measure"],
			Station = i)
		data = rbind(data, stationInfo)
	}

	#extra step: order by date (would be a DB request)
	data = data[order(data[,"Date"]),]

	return (data)
}
