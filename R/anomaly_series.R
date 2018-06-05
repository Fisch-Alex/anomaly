
#' A technique for detecting anomalous segments based on CAPA (Collective And Point Anomaly) by Fisch et al.
#'
#' CAPA (Collective And Point Anomaly) by A. Fisch, I. A. Eckley, and P. N. Fearnhead assumes that the data has a certain mean and variance for most
#' timepoints and detects segments in which the mean and/or variance deviates from the typical mean and variance as collective anomalies. It also detects point
#' outliers and returns a measure of strength for the changes in mean and variance. At best, if the number of anomalous windows scales linearly with the number of
#' data points, CAPA scales linearly with the number of data points. At worst, if there are no anomalies at all, CAPA scales quadratically with the number of data points. 
#'
#' @param x A numeric vector containing the data which is to be inspected.
#' @param penaltywindow A numeric constant indicating the penalty for adding an additional epidemic changepoint. It defaults to a BIC style penalty if no argument is provided.
#' @param penaltyanomaly A numeric constant indicating the penalty for adding an additional point anomaly. It defaults to a BIC style penalty if no argument is provided.
#' @param minimumsegmentlength An integer indicating the minimum length of epidemic changes. It must be at least 2 and defaults to 10.
#' @param warnings A logical determining whether the warnings should be displayed. It defaults to TRUE.
#' 
#' @return An anomaly_series object. 
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
#' summary(inferred_anomalies)
#' print(inferred_anomalies)
#'
#' @references Fisch, A., Eckley, I. A. & Fearnhead, P. N. (2018) A linear time method for the detection of point and collective anomalies.
#'
#' @export
anomaly_series = function(x, penaltywindow = NULL, penaltyanomaly = NULL, minimumsegmentlength = 10, warnings = TRUE){
   
  ##### We do our preprocessing here
  
  if(!is.logical(warnings)){
    stop("warnings must be a logical.")
  }
  
  if(length(warnings)>1){
    warnings = warnings[1]
    if(warnings){warning("warnings should be a logical vector of length 1. Considered only the first entry.")}
  }

  tryCatch({x = as.vector(x)}, error = function(e){stop("The data x is not a vector and can not be converted to one.")}, 
             warning = function(w){if(warnings){warning("The data x is not a vector. We managed to convert it but it is probably not a good idea.")}} )
  
  if (sum(is.na(x)) > 0){
    if(warnings){warning("x contains NAs. We removed them and continued our analysis on the rest.")}
    x = x[which(!is.na(x))]
  }
  
  if (sum(is.nan(x)) > 0){
    if(warnings){warning("x contains NaNs. We removed them and continued our analysis on the rest.")}
    x = x[which(!is.nan(x))]
  }
  
  if (sum(is.infinite(x)) > 0){
    if(warnings){warning("x contains infinite values. We removed them and continued our analysis on the rest.")}
    x = x[which(!is.infinite(x))]
  }
  
  if(mad(x) == 0){
    stop("x has robust variance 0. We can not run our algorithm on such data")
  }
  
  defaultreturn_minimumsegmentlength = function(){
    if(warnings){warning("The input for minimumsegmentlength is not an integer and cannot be converted to an integer. We revert to default.")}
    return(10)
  }
  
  if (!(is.numeric(x) + is.integer(x))){
    stop("x must contain numbers.")
  }
  
  minimumsegmentlength = tryCatch({as.integer(minimumsegmentlength)},
           error   = function(e){defaultreturn_minimumsegmentlength()},
           warning = function(w){defaultreturn_minimumsegmentlength()})
  
  if (length(minimumsegmentlength)>1){
    if(warnings){warning("The input for minimumsegmentlength has multiple entries. Only the first one is kept")}
    minimumsegmentlength = minimumsegmentlength[1]
  }
  
  if(is.na(minimumsegmentlength)){
    minimumsegmentlength = defaultreturn_minimumsegmentlength()
  }
  
  if(length(x) <= minimumsegmentlength){
    stop("The length of x must be longer than the minimum segment length")
  }
  
  if(2 > minimumsegmentlength){
    if(warnings){warning("minimumsegmentlength must be at least 2. We reverted to default")}
    minimumsegmentlength = 10
  }
  
  if(length(x) <= 100){
    if(warnings){warning("The length of x is less than 100...")}
  }
  
  if(is.null(penaltywindow)){
    penaltywindow = 4*log(length(x))
  }
  
  if(is.null(penaltyanomaly)){
    penaltyanomaly = 3*log(length(x))
  }
  
  Defaultwindowpenalty = function(){
    if(warnings){warning("Non-numeric argument for penaltywindow. Default penalty used.")}
    penaltywindow = 4*log(length(x))
  }
  
  Defaultanomalypenalty = function(){
    if(warnings){warning("Non-numeric argument for penaltyanomaly. Default penalty used.")}
    penaltyanomaly = 3*log(length(x))
  }
  
  penaltywindow  = tryCatch({as.numeric(penaltywindow)} ,error = function(e){Defaultwindowpenalty()} , warning = function(w){Defaultwindowpenalty()} )
  penaltyanomaly = tryCatch({as.numeric(penaltyanomaly)},error = function(e){Defaultanomalypenalty()}, warning = function(w){Defaultanomalypenalty()})
  
  if(is.na(penaltywindow)){
    if(warnings){warning("penaltywindow is NA. Default penalty used.")}
    penaltywindow = 4*log(length(x))
  }
  
  if(is.nan(penaltywindow)){
    if(warnings){warning("penaltywindow is NaN. Default penalty used.")}
    penaltywindow = 4*log(length(x))
  }
  
  if(is.infinite(penaltywindow)){
    if(warnings){warning("penaltywindow is infinite. Default penalty used.")}
    penaltywindow = 4*log(length(x))
  }
  
  if(is.na(penaltyanomaly)){
    if(warnings){warning("penaltyanomaly is NA. Default penalty used.")}
    penaltyanomaly = 3*log(length(x))
  }
  
  if(is.nan(penaltyanomaly)){
    if(warnings){warning("penaltyanomaly is NaN. Default penalty used.")}
    penaltyanomaly = 3*log(length(x))
  }
  
  if(is.infinite(penaltyanomaly)){
    if(warnings){warning("penaltyanomaly is infinite. Default penalty used.")}
    penaltyanomaly = 3*log(length(x))
  }
  
  if(length(penaltyanomaly)>1){
    if(warnings){warning("penaltyanomaly has more than one entry. Only the first one is kept.")}
    penaltyanomaly = penaltyanomaly[1]
  }
  
  if(length(penaltywindow)>1){
    if(warnings){warning("penaltywindow has more than one entry. Only the first one is kept.")}
    penaltywindow = penaltywindow[1]
  }
  
  if((penaltyanomaly)<=0){
    if(warnings){warning("penaltyanomaly is less than 0!")}
  }
  
  if((penaltywindow)<=0){
    if(warnings){warning("penaltywindow is less than 0!")}
  }
  
  ##### Actual code happens below
  
  output        = list()
  output[["x"]] = x
  
  output[["anomalies_strength"]]      = data.frame(variance_change = numeric(0), mean_change = numeric(0))
  output[["pointanomalies_strength"]] = numeric(0)

  n = length(x)
  x = x - median(x)
  x = x/mad(x)
  
  Canomalyoutput = .Call("MeanVarAnomaly", PACKAGE = "anomaly", x, as.integer(n), as.integer(minimumsegmentlength), penaltywindow, penaltyanomaly)
  
  if(is.null(Canomalyoutput)){
    warning("User interrupt. NULL is returned.")
    return(NULL)
  }
  
  emptyoutput = matrix(nrow = 0,ncol = 2)
  colnames(emptyoutput) = c("start","end")
  
  
  if(length(Canomalyoutput) == 2){
      
      tmp = matrix(nrow=0,ncol=2)
      colnames(tmp) = c("start","end")
    
      output[["pointanomalies"]]          = integer(0)
      output[["anomalywindows"]]          = tmp
      return(structure(output,class="anomaly_series"))
    
  }
  
  Canomalyoutput = rev(Canomalyoutput[3:length(Canomalyoutput)])
  Canomalyoutput = as.data.frame(matrix(Canomalyoutput, ncol = 2, byrow = T))
  colnames(Canomalyoutput) = c("start","end")
  
  output[["pointanomalies"]] = Canomalyoutput[which(Canomalyoutput$start == Canomalyoutput$end),"start"]
  output[["anomalywindows"]] = Canomalyoutput[which(Canomalyoutput$start < Canomalyoutput$end),]
  
  if (length(which(Canomalyoutput$start == Canomalyoutput$end)) > 0){
    
      output[["pointanomalies_strength"]] = abs(x[output[["pointanomalies"]]])
    
  } 
  
  if (length(which(Canomalyoutput$start < Canomalyoutput$end)) > 0){
      
      meanchanges     = rep(NA,length(which(Canomalyoutput$start < Canomalyoutput$end)))
      variancechanges = rep(NA,length(which(Canomalyoutput$start < Canomalyoutput$end)))  
      
      for (ii in 1:length(which(Canomalyoutput$start < Canomalyoutput$end)) ){
        
          observation = which(Canomalyoutput$start < Canomalyoutput$end)[ii]
          
          variance = var(x[Canomalyoutput$start[observation]:Canomalyoutput$end[observation]])
          
          variancechanges[ii] = sqrt(variance) + 1/sqrt(variance) - 2
          
          meanchanges[ii]     = mean(x[Canomalyoutput$start[observation]:Canomalyoutput$end[observation]])^2/sqrt(variance)
          
      }
        
      output[["anomalies_strength"]] = data.frame(variance_change = variancechanges, mean_change = meanchanges)
      
  }
  
  return(structure(output,class="anomaly_series"))
  
}
