#' Sample data built from DataMarket Rhine River time-series
#'
#' 3 "stations": original serie, reversed series, average of both.\cr
#' "Experts": persistence (P), moving average with window==3 (MA3) and 10 (MA10).\cr
#' -----\cr
#' Generating R code:\cr
#' library(rdatamarket)\cr
#' serie = dmseries("https://datamarket.com/data/set/22wp/rhine-river-near-basle-switzerland-1807-1957")\cr
#' dates = seq(as.Date("1807-07-01"),as.Date("1956-07-01"),"years")\cr
#' serie = list(serie, rev(serie), (serie+rev(serie))/2)\cr
#' st = list()\cr
#' for (i in 1:3) {\cr
#'	st[[i]] = data.frame(\cr
#'		Date=dates,\cr
#'		P=c(NA,serie[[i]][1:149]),\cr
#'		MA3=c(rep(NA,3),sapply(4:150, function(j) mean(serie[[i]][(j-3):(j-1)]) )),\cr
#'		MA10=c(rep(NA,10),sapply(11:150, function(j) mean(serie[[i]][(j-10):(j-1)]) )),\cr
#'		Measure=as.double(serie[[i]])
#'	)\cr
#' }\cr
#' save(st, file="stations.RData")
#'
#' @name stations
#' @docType data
#' @usage data(stations)
#' @references \url{https://datamarket.com/data/set/22wp/rhine-river-near-basle-switzerland-1807-1957}
#' @format A list of 3 dataframes with 150 rows and 5 columns: Date,P,MA3,MA10,Measure
NULL
