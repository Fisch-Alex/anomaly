\name{period_average}
\alias{period_average}
\title{A function to search the Kepler data for periodically recurring dips in luminosity}
\usage{
period_average(data,period)
}
\arguments{
\item{data}{A dataframe with one column named "Day" and the other "Brightness", such as Kepler10965588 (included in the package)}

\item{period}{A numeric which is larger than 0 representing the period (in days) which is to be examined}

}
\value{
A vector of numerics
}
\description{
The signal of transitting planets is very weak, especially if the planet is small. This function amplifies it by amplifying the periodicity of the signal. All observations times are taken modulo the period and then binned. An average is then taken within each bin. This is then returned. If the period (or an integer fraction of a period) is used then a transit signal should be visible. 
}
\examples{
library(anomaly)
data(Lightcurves)
### Plot the data for Kepler 10965588: No transit apparent
plot(Kepler10965588$Day,Kepler10965588$Brightness,xlab = "Day",pch=".")
### Examine a period of 62.9 days for Kepler 10965588
binned_data = period_average(Kepler10965588,62.9)
inferred_anomalies = anomaly_series(binned_data)
plot(inferred_anomalies,xlab="Bin")
# We found a planet! 
}
