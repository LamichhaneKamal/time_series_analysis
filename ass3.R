##Author: Pratik Bhandary
##10-10-2016
require(gdata)
require(graphics)
#require(deseasonalize)
library(forecast)
df = read.xls ("pearson.xls", header = TRUE, fileEncoding="UTF-8")
torontotimeseries <- ts(df[1:878,1], 1:12, frequency = 12, start=c(1940,1), end = c(2012,12) )
print(torontotimeseries)
torontotimeseries.stl = stl(torontotimeseries, s.window="periodic")
fit <- decompose(torontotimeseries, type="multiplicative")
print(torontotimeseries.stl)
print(stl(log(torontotimeseries), s.window = "per", t.window = 1000) )

#Just to see if tthere are any differences in the data 
#plot(fit) #using decompose
#seasonal adjustment i.e deseasonalized
torontotimeseries.sa <- seasadj(torontotimeseries.stl)
print('This is deseasonalized data')
print(torontotimeseries.sa)
differencedTS <- diff(torontotimeseries)
library(tseries)
pv <- adf.test(torontotimeseries)
print(pv)
print('Mean, variance, autocorrelation, etc. are all constant over time if p value is lesser than 0.05')
plot(torontotimeseries.stl,main = "Linear data plot") #using STL
plot(stl(log(torontotimeseries), s.window = "per", t.window = 1000) , main="Plot to get the linear trend")
plot(torontotimeseries.sa, main="Deseasonalized data",  xlab="Time", ylab="")
Colour_Plot_for_all_years <- torontotimeseries.sa
seasonplot(Colour_Plot_for_all_years ,12, col=rainbow(12),year.labels=TRUE) 
####End################

#READ THIS FROM THESE WEBSITE FOR EXPLANATION
#https://www.r-bloggers.com/time-series-decomposition/
#http://rstatistics.net/time-series-analysis/
#######CHANGE THE LANGUAGE COZ THIS IS COPIED FROM INTERNET
#The four graphs are the original data, seasonal component, trend component and
# the remainder and this shows the periodic seasonal pattern extracted out from the
# original data and the trend that moves around between 47 and 51 degrees
# Fahrenheit. There is a bar at the right hand side of each graph to allow a 
# relative comparison of the magnitudes of each component. For this data the 
#  change in trend is less than the variation doing to the monthly variation.

