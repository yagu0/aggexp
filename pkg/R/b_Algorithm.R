#' @include z_util.R

#' @title Algorithm
#'
#' @description Generic class to represent an algorithm
#'
#' @field H The window [t-H+1, t] considered for prediction at time step t+1
#' @field data Data frame of the last H experts forecasts + observations.
#'
Algorithm = setRefClass(
	Class = "Algorithm",

	fields = list(
		H = "numeric",
		data = "data.frame"
	),

	methods = list(
		initialize = function(...)
		{
			"Initialize (generic) Algorithm object"

			callSuper(...)
			if (length(H) == 0 || H < 1)
				H <<- Inf
		},
		inputNextForecasts = function(x)
		{
			"Obtain a new series of vectors of experts forecasts (1 to K)"

			nd = nrow(data)
			nx = nrow(x)
			indices = (nd+1):(nd+nx)

			appendedData = as.data.frame(matrix(nrow=nx, ncol=ncol(data), NA))
			names(appendedData) = names(data)
			data <<- rbind(data, appendedData)
			data[indices,names(x)] <<- x
		},
		inputNextObservations = function(y)
		{
			"Obtain the observations corresponding to last input forecasts"

			#if all experts made a large unilateral error and prediction is very bad, remove data
			n = nrow(data)
			lastTime = data[n,"Date"]
			xy = subset(data, subset=(Date == lastTime))
			xy[,"Measure"] = y
			x = xy[,names(xy) != "Measure"]
			y = xy[,"Measure"]
			ranges = apply(x-y, 1, range)
			predictableIndices = (ranges[2,] > -MAX_ERROR & ranges[1,] < MAX_ERROR)
#			predictableIndices = 1:length(y)
			data <<- data[1:(n-nrow(xy)),]
			data <<- rbind(data, xy[predictableIndices,])

			#oldest rows are removed to prevent infinitely growing memory usage,
			#or to allow a window effect (parameter H)
			delta = nrow(data) - min(H, MAX_HISTORY)
			if (delta > 0)
				data <<- data[-(1:delta),]
		},
		predict_withNA = function()
		{
			"Predict observations corresponding to the last input forecasts. Potential NAs"

			n = nrow(data)
			if (data[n,"Date"] == 1)
			{
				#no measures added so far
				return (rep(NA, n))
			}

			nx = n - nrow(subset(data, subset = (Date == data[n,"Date"])))
			x = data[(nx+1):n, !names(data) %in% c("Date","Measure","Station")]
			experts = names(x)
			prediction = c()

			#extract a maximal submatrix of data without NAs

			iy = getNoNAindices(x, 2)
			if (!any(iy))
			{
				#all columns of x have at least one NA
				return (rep(NA, n-nx))
			}

			data_noNA = data[1:nx,c(experts[iy], "Measure")]
			ix = getNoNAindices(data_noNA)
			if (!any(ix))
			{
				#no full line with NA-pattern similar to x[,iy]
				return (rep(NA, n-nx))
			}

			data_noNA = data_noNA[ix,]
			xiy = as.data.frame(x[,iy])
			names(xiy) = names(x)[iy]
			res = predict_noNA(data_noNA, xiy)
			#basic sanitization: force all values >=0
			res[res < 0.] = 0.
			return (res)
		},
		predict_noNA = function(XY, x)
		{
			"Predict observations corresponding to x. No NAs"

			#empty default implementation: to implement in inherited classes
		}
	)
)
