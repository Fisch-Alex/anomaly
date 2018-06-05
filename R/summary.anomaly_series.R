#' Summary method for anomaly_series objects.
#'
#' A function summarising the content of an anomaly_object as produced by the \code{\link{anomaly_series}} method. It prints the number of detected anomalous windows and point anomalies.
#'
#' @param object anomaly_series object
#' @param ... Other parameters to be passed to summary methods.
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
#' summary(inferred_anomalies)
#'
#' @export
summary.anomaly_series = function(object,...){
  
  
  unexpectedarguments = names(list(...))
  
  if(length(unexpectedarguments)==1){warning(paste("The argument",unexpectedarguments,"has been ignored"))}
  if(length(unexpectedarguments)>1){warning(paste("The arguments",paste(unexpectedarguments,", "),"have been ignored"))}  
  
  anomaly_object = object
  
  cat("Point anomalies detected:\t")
  cat(length(anomaly_object[["pointanomalies"]]))
  cat("\n")
  cat("Anomalous segments detected:\t")
  cat(nrow(anomaly_object[["anomalywindows"]]))
  
}