library(tseries)
library(timeSeries)
library(lmtest)
library(forecast)
library(nortest)
library(qcc)

EWMAwithSlidingWindow <- function(s,window.size=52,parlambda=0.8) {
  #s: time series data
  #window.size: sliding window size
  #parlambda: smoothing parameter
  ct1<-NULL 
  ys1<-NULL 
  sigma1<-NULL 

  data1<-s[1:window.size]
  x<-ewma(data1,lambda = parlambda, nsigmas = 2,plot = F)
  ct1[1:(window.size+1)]<-x$center
  #ys1 from final model
  sigma1[1:(window.size+1)]<-x$std.dev
  for (i in 2:(length(s)-window.size)){
    data1<-s[i:(i+window.size-1)]
    data2<-w[i:(i+window.size-1)]
    x<-ewma(data1,lambda = parlambda, nsigmas = 2,plot = F)
    ct1[window.size+i]<-x$center
    sigma1[window.size+i]<-x$std.dev
    }
  ys1<-ewma(s,lambda = parlambda, nsigmas = 2,plot = F)$y
  nsig<-1.2
  slimitsL<-ct1-nsig*sigma1
  slimitsU<-ct1+nsig*sigma1
  return_list <- list("smoothed line" = ys1, "upperBound" = slimitsU, "lowerBound" = slimitsL)
  return(return_list) 
}



