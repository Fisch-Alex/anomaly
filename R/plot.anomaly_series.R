
#' Plots an anomaly_series object.
#'
#' Plot method for anomaly_series objects as returned by the \code{\link{anomaly_series}} method.
#'
#' @param x anomaly_series object
#' @param xlab Character string containing label for the x-axis.
#' @param ylab Character string containing label for the y-axis.
#' @param ... Other parameters to be passed to plotting methods.
#'
#' @examples 
#' library(anomaly)
#' set.seed(2018)
#' # Generate data typically followig a normal distribution with mean 0 and variance 1. 
#' # Then introduce 3 anomaly windows and 4 point outliers.
#' x  = rnorm(5000)
#' x[401:500]   = rnorm(100,4,1)
#' x[1601:1800] = rnorm(200,0,0.01)
#' x[3201:3500] = rnorm(300,0,10)
#' x[c(1000,2000,3000,4000)] = rnorm(4,0,100)
#' inferred_anomalies = anomaly_series(x)
#' plot(inferred_anomalies)
#'
#' @export
plot.anomaly_series = function(x,xlab="",ylab="",...){
  
  anomaly_object = x
  
  unexpectedarguments = names(list(...))
  
  if(length(unexpectedarguments)==1){warning(paste("The argument",unexpectedarguments,"has been ignored"))}
  if(length(unexpectedarguments)>1){warning(paste("The arguments",paste(unexpectedarguments,", "),"have been ignored"))}  
  
  plot(anomaly_object[["x"]],ylab=ylab,xlab=xlab)
  
  points(anomaly_object[["pointanomalies"]],anomaly_object[["x"]][anomaly_object[["pointanomalies"]]],col="red")
  
  abline(v = anomaly_object[["anomalywindows"]][,"start"],col="red")
  abline(v = anomaly_object[["anomalywindows"]][, "end" ],col="red")
  
}