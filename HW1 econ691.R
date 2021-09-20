
#This is a script is HW1.
#created by Hissah Altamimi on Sep, 09, 2021.


#Q1: 

setwd("~/Desktop/ECON691/DR.GROVES/DATA")
mydata<-read.csv("ILCovid19.csv")
attach(mydata)
library(tidyverse)
covid<-read.csv("ILCovid19.csv")
head(covid)
#        Date Tests Cases Deaths
# 1 3/10/2020   326    20      0
# 2 3/11/2020   367    25      0
# 3 3/12/2020   418    32      0
# 4 3/13/2020   444    46      0
# 5 3/14/2020   708    64      0
# 6 3/15/2020   932    93      0
dim(covid)# [1] 534   4
DIF<-function(x) {
  # x is a vector, not data.frame
  diff(x)
}

new.cases<-DIF(covid[,"Cases"])
new.deaths<-DIF(covid[,"Deaths"])
new.tests<-DIF(covid[,"Tests"])

PercentChange<-function(x) {
  N<-length(x)
  diff(x)/x[1:(N-1)]
}

percentchange.cases<-PercentChange(covid[,"Cases"])
percentchange.deaths<-PercentChange(covid[,"Deaths"])
percentchange.tests<-PercentChange(covid[,"Tests"])

par(mfrow=c(3,1))
data<-data.frame(percentchange.cases, 
                   percentchange.deaths, 
                   percentchange.tests)
names(data)<-c("Cases","Deaths","Tests")
plot(ts(data),main="%change",xaxt="n",xlab="")
ticks<-c(1, 200, 400, 533)
dates<-covid[,"Date"]
axis(1,at=ticks,labels=dates[c(2,199,399,533)])

