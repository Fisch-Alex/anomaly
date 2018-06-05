#' Print function for anomaly_series objects.
#'
#' Prints the contents of an anomaly_series object as returned by the \code{\link{anomaly_series}} method.
#'
#'
#' @param x An anomaly_series object
#' @param ... Other parameters to be passed to print methods.
#'
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
#' print(inferred_anomalies)
#'
#' @export
print.anomaly_series = function(x,...){
  
  anomaly_object = x
  
  unexpectedarguments = names(list(...))
  
  if(length(unexpectedarguments)==1){warning(paste("The argument",paste(unexpectedarguments,", "),"has been ignored"))}
  if(length(unexpectedarguments)>1){warning(paste("The arguments",paste(unexpectedarguments,", "),"have been ignored"))}
  
  cat("Point anomalies detected:\t")
  cat(length(anomaly_object[["pointanomalies"]]))
  cat("\n")
  cat("location\t")
  cat("strength\t")
  cat("\n")
  if (length(anomaly_object[["pointanomalies"]]) > 0){
    for (ii in 1:length(anomaly_object[["pointanomalies"]]) ){
      cat(anomaly_object[["pointanomalies"]][ii])
      cat("\t")
      cat("\t")
      cat(anomaly_object[["pointanomalies_strength"]][ii])
      cat("\n")
    }
  }
  cat("\n")
  cat("Anomalous segments detected:\t")
  cat(nrow(anomaly_object[["anomalywindows"]]))
  cat("\n")
  cat("start\t")
  cat("end\t")
  cat("mean change\t")
  cat("variance change\t")
  cat("\n")
  if (nrow(anomaly_object[["anomalywindows"]]) > 0){
    for (ii in 1:nrow(anomaly_object[["anomalywindows"]]) ){
      cat(anomaly_object[["anomalywindows"]][ii,"start"])
      cat("\t")
      cat(anomaly_object[["anomalywindows"]][ii,"end"])
      cat("\t")
      cat(anomaly_object[["anomalies_strength"]][ii,"mean_change"])
      cat("\t")
      cat(anomaly_object[["anomalies_strength"]][ii,"variance_change"])
      cat("\n")
    }
  }
  
}