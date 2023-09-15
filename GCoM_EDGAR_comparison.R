## EDGAR-GCoM comparison

## Upload packages
library(readxl)
library(tidyr)
library(dplyr)
library(plyr)
library(readr)
library(data.table)
library(car)
library(MASS)

## 1.1 Import data
data0 = read.table("gcom_edgar_sample.txt", header=TRUE)

## 1.2 Declare functions for computing general statistics
func <- function(xx)
{
  return(data.frame(C=(length(unique(xx$organisation_name))), N = (length(xx$CoM_emis)), COR = cor(xx$CoM_emis, xx$EDGAR_emis) ))
}

func2 <- function(xx)
{
  return(data.frame(NRMSE = sqrt((sum(xx$CoM_emis-xx$EDGAR_emis)^2)/(length(xx$CoM_emis)-1))/(max(xx$CoM_emis, xx$EDGAR_emis)-min(xx$CoM_emis, xx$EDGAR_emis)), N = (length(xx$CoM_emis))))
}

func3 <- function(xx)
{
  return(data.frame(BIAS = sum(xx$CoM_emis-xx$EDGAR_emis)/sum(xx$EDGAR_emis), N = (length(xx$CoM_emis))))
}

func4 <- function(xx)
{
  return(data.frame(MAPE = mean(abs(xx$CoM_emis-xx$EDGAR_emis)/xx$EDGAR_emis), N = (length(xx$CoM_emis))))
}

## 2.1 Select Residential buildings sector
data = subset(data0,sector=="RCO")

## 2.1.1 Clean from missing observations
data = subset(data,!is.na(CoM_emis)&!is.na(EDGAR_emis))

## 2.1.2 Define variable "size"
data$size = ifelse(data$population3>500000,"Big",ifelse(data$population3<500000&data$population3>50000,"Medium","Small"))

## 2.1.3 Test on equal medians by size of the city
data1 = data[c("CoM_emis","size")]
data1$emis=data1$CoM_emis
data1$frame = "CoM"
data2 = data[c("EDGAR_emis","size")]
data2$emis = data2$EDGAR_emis
data2$frame = "EDGAR"
data1 = data1[c("emis","size","frame")]
data2 = data2[c("emis","size","frame")]
data12 = rbind(data1,data2)

## 2.1.3.0 Total
data10 = data1
data20 = data2
wilcox.test(data10$emis, data20$emis, paired = TRUE, alternative = "two.sided")
median(data10$emis)-median(data20$emis)
t.test(data10$emis, data20$emis, paired = TRUE, alternative = "two.sided")

## 2.1.3.1 Big cities
data10 = subset(data1,size=="Big")
data20 = subset(data2,size=="Big")
wilcox.test(data10$emis, data20$emis, paired = TRUE, alternative = "two.sided")
median(data10$emis)-median(data20$emis)

## 2.1.3.2 Medium cities
data10 = subset(data1,size=="Medium")
data20 = subset(data2,size=="Medium")
wilcox.test(data10$emis, data20$emis, paired = TRUE, alternative = "two.sided")
median(data10$emis)-median(data20$emis)

## 2.1.3.3 Small cities
data10 = subset(data1,size=="Small")
data20 = subset(data2,size=="Small")
wilcox.test(data10$emis, data20$emis, paired = TRUE, alternative = "two.sided")
median(data10$emis)-median(data20$emis)

## 2.1.4 Estimate correlation
(cor_tot = cor(data[c("CoM_emis","EDGAR_emis")]))

## 2.1.5 Estimate NRMSE
sse=sum(data[c("CoM_emis")]-data[c("EDGAR_emis")])^2
min=min(data[c("CoM_emis","EDGAR_emis")])
max=max(data[c("CoM_emis","EDGAR_emis")])
n = dim(data)[1]
(nrmse_tot = sqrt(sse/(n-1))/(max-min))

## 2.1.6 Estimate Bias
(bias=sum(data[c("CoM_emis")]-data[c("EDGAR_emis")])/sum(data[c("EDGAR_emis")]))

## 2.1.7 Estimate MAPE
(mape=sum(abs(data[c("CoM_emis")]-data[c("EDGAR_emis")])/(data[c("EDGAR_emis")]))/dim(data)[1])

## By country
xx = data[c("organisation_name","CoM_emis","EDGAR_emis","year","country","size")]
data_y = ddply(xx, .(country), func)
data_z = ddply(xx, .(country), func2)
data_b = ddply(xx, .(country), func3)
data_m = ddply(xx, .(country), func4)
stats=data_y
stats=cbind(stats,data_z[c("NRMSE")],data_b[c("BIAS")],data_m[c("MAPE")])

## By city size
data_y = ddply(xx, .(size), func)
data_z = ddply(xx, .(size), func2)
data_b = ddply(xx, .(size), func3)
data_m = ddply(xx, .(size), func4)
stats=data_y
stats=cbind(stats,data_z[c("NRMSE")],data_b[c("BIAS")],data_m[c("MAPE")])

## 2.1.8 Estimate the linear relation between EDGAR and CoM by robust linear regression
mod = rlm(EDGAR_emis ~  CoM_emis, data=data)
summary(mod) 


## 3.1 Select Road transport sector
data = subset(data0,sector=="TRO_noRES")

## 3.1.1 Clean from missing observations
data = subset(data,!is.na(CoM_emis)&!is.na(EDGAR_emis))

## 3.1.2 Define variable "size"
data$size = ifelse(data$population3>500000,"Big",ifelse(data$population3<500000&data$population3>50000,"Medium","Small"))

## 3.1.3 Test on equal medians by size of the city
data1 = data[c("CoM_emis","size")]
data1$emis=data1$CoM_emis
data1$frame = "CoM"
data2 = data[c("EDGAR_emis","size")]
data2$emis = data2$EDGAR_emis
data2$frame = "EDGAR"
data1 = data1[c("emis","size","frame")]
data2 = data2[c("emis","size","frame")]
data12 = rbind(data1,data2)

## 3.1.3.0 Total
data10 = data1
data20 = data2
wilcox.test(data10$emis, data20$emis, paired = TRUE, alternative = "two.sided")
median(data10$emis)-median(data20$emis)
t.test(data10$emis, data20$emis, paired = TRUE, alternative = "two.sided")

## 3.1.3.1 Big cities
data10 = subset(data1,size=="Big")
data20 = subset(data2,size=="Big")
wilcox.test(data10$emis, data20$emis, paired = TRUE, alternative = "two.sided")
median(data10$emis)-median(data20$emis)

## 3.1.3.2 Medium cities
data10 = subset(data1,size=="Medium")
data20 = subset(data2,size=="Medium")
wilcox.test(data10$emis, data20$emis, paired = TRUE, alternative = "two.sided")
median(data10$emis)-median(data20$emis)

## 3.1.3.3 Small cities
data10 = subset(data1,size=="Small")
data20 = subset(data2,size=="Small")
wilcox.test(data10$emis, data20$emis, paired = TRUE, alternative = "two.sided")
median(data10$emis)-median(data20$emis)

## 3.1.4 Estimate correlation
(cor_tot = cor(data[c("CoM_emis","EDGAR_emis")]))

## 3.1.5 Estimate NRMSE
sse=sum(data[c("CoM_emis")]-data[c("EDGAR_emis")])^2
min=min(data[c("CoM_emis","EDGAR_emis")])
max=max(data[c("CoM_emis","EDGAR_emis")])
n = dim(data)[1]
(nrmse_tot = sqrt(sse/(n-1))/(max-min))

## 3.1.6 Estimate Bias
(bias=sum(data[c("CoM_emis")]-data[c("EDGAR_emis")])/sum(data[c("EDGAR_emis")]))

## 3.1.7 Estimate MAPE
(mape=sum(abs(data[c("CoM_emis")]-data[c("EDGAR_emis")])/(data[c("EDGAR_emis")]))/dim(data)[1])

## By country
xx = data[c("organisation_name","CoM_emis","EDGAR_emis","year","country","size")]
data_y = ddply(xx, .(country), func)
data_z = ddply(xx, .(country), func2)
data_b = ddply(xx, .(country), func3)
data_m = ddply(xx, .(country), func4)
stats=data_y
stats=cbind(stats,data_z[c("NRMSE")],data_b[c("BIAS")],data_m[c("MAPE")])

## By city size
data_y = ddply(xx, .(size), func)
data_z = ddply(xx, .(size), func2)
data_b = ddply(xx, .(size), func3)
data_m = ddply(xx, .(size), func4)
stats=data_y
stats=cbind(stats,data_z[c("NRMSE")],data_b[c("BIAS")],data_m[c("MAPE")])

## 3.1.8 Estimate the linear relation between EDGAR and CoM by robust linear regression
mod = rlm(EDGAR_emis ~  CoM_emis, data=data)
summary(mod)

## end ##
