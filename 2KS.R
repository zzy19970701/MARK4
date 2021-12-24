#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(nortest)
library(psych)
library(pastecs)
#x<-c(9.967,10.001,9.994,10.023,9.969,9.965,10.013,9.992,9.954,9.934)
#  describe(x)
#* @serializer unboxedJSON
#* @post /summary
function(x) {
  x<-as.numeric(x)
  x1<-summary(x)
  x1<-as.matrix(x1)
  x2<-describe(x)
  x2<-as.matrix(x2)
  x3<-stat.desc(x,norm = T)
  x3<-as.matrix(x3)
  list(paste0("样本数量(n):",x2[,2]),
       paste0("最小值(min):",x1[1,]),
       paste0("最大值(max):",x1[6,]),
       paste0("极差(range):",signif(x3[6,],5) ),
       paste0("均值(mean):",signif(x1[4,],5)),
       paste0("方差(var):",signif(x3[12,],5)),
       paste0("标准差(sd):",signif(x2[,4],5)),
       paste0("截尾均值(trimmed):",signif(x2[,6],5)),
       paste0("绝对中位差(mad):",signif(x2[,7],5)),
       paste0("第一四分位数(1st Qu.):",x1[2,]),
       paste0("中位数(median):",x1[3,]),
       paste0("第三四分位数(3rd Qu.):",x1[5,]),
       paste0("峰度(kurtosis):",signif(x2[,12],5)),
       paste0("峰度系数的显著判据(kurt.2SE):",signif(x3[18,],5)),
       paste0("偏度(skewness):",signif(x2[,11],5)),
       paste0("偏度系数的显著判据(skew.2SE):",signif(x3[16,],5)),
       paste0("平均数的标准误(se):",signif(x2[,13],5)),
       paste0("平均置信度为95%的置信区间(CI.mean.0.95):",signif(x3[11,],5)),
       paste0("normtest.W:",signif(x3[19,],5)),
       paste0("normtest.p:",signif(x3[20,],5))
      )
}

#* @serializer png
#* @post /boxplot
function(x) {
  x<-as.numeric(x)
  boxplot(x)
}

#* @serializer png
#* @post /qqnorm
function(x) {
 
  z<-scale(x)
  qqnorm(z)
  qqline(z)
}

#* @serializer png
#* @post /hist
function(x) {
  
  hist(x,col="lightblue")
}


#* @serializer png
#* @post /ker-den
function(x) {
 
  plot(density(x))
}


#* @serializer unboxedJSON
#* @post /test
function(x) {
 
  list(paste0("ks-statistic:",ks.test(x,"pnorm",mean(x),sd(x))$statistic),
       paste0("ks-p.value:",ks.test(x,"pnorm",mean(x),sd(x))$p.value),
       paste0("ks-alternative:",ks.test(x,"pnorm",mean(x),sd(x))$alternative),
       paste0("ad-statistic:",ad.test(x)$statistic),
       paste0("ad-p.value:",ad.test(x)$p.value),
       paste0("cvm-statistic:",cvm.test(x)$statistic),
       paste0("cvm-p.value:",cvm.test(x)$p.value),
       paste0("pearson-statistic:",pearson.test(x)$statistic),
       paste0("pearson-p.value:",pearson.test(x)$p.value),
       paste0("pearson-n.classes:",pearson.test(x)$n.classes),
       paste0("pearson-df:", pearson.test(x)$df)
       )   
}


#* @serializer png
#* @post /nor-dis
function(x) {
  
  set.seed(1)
  yc <- seq(min(x),max(x),0.001)
  y <- pnorm(yc,mean(x),sd(x))
  plot(yc,y,xlab = "data",ylab = "normal ditribution",type="l")
  plot(ecdf(x),  add = T, col="red")
}


#* @serializer png
#* @post /nor-den
function(x) {
  
  set.seed(1)
  yc <- seq(min(x),max(x),0.001)
  y <- dnorm(yc,mean(x),sd(x))
  plot(yc,y,xlab = "data",ylab = "normal density function",type="l")
}


#* @serializer png
#* @post /lnor-dis
function(x) {
  
  set.seed(1)
  yc <- seq(min(x),max(x),0.001)
  y <- plnorm(yc,meanlog = mean(yc),sdlog = sd(yc))
  plot(yc,y,xlab = "data",ylab = "lognormal distribution",type = "l")
  plot(ecdf(x),add = T, col="red")
}
#* @serializer png
#* @post /lnor-den
function(x) {
  
  set.seed(1)
  yc <- seq(min(x),max(x),0.001)
  y <- dlnorm(yc,meanlog = mean(yc),sdlog = sd(yc))
  plot(yc,y,xlab = "data",ylab = "lognormal density function",type = "l")
  
}
#* @serializer png
#* @post /logi-dis
function(x) {
  
  set.seed(1)
  yc <- seq(min(x),max(x),0.001)
  y <- plogis(yc,mean(x),sd(x))
  plot(yc,y,xlab = "data",ylab = "logistic ditribution",type="l")
  plot(ecdf(x),  add = T, col="red")
}
#* @serializer png
#* @post /logi-den
function(x) {
  
  set.seed(1)
  yc <- seq(min(x),max(x),0.001)
  y <- dlogis(yc,mean(x),sd(x))
  plot(yc,y,xlab = "data",ylab = "logistic density function",type="l")
}

#* @serializer png
#* @post /uni-dis
function(x) {
  
  set.seed(1)
  yc <- seq(min(x),max(x),0.001)
  y <- punif(yc,min = min(yc),max = max(yc))
  plot(yc,y,xlab = "data",ylab = "uniform ditribution",type="l")
  plot(ecdf(x),  add = T, col="red")
}

#* @serializer png
#* @post /uni-den
function(x) {
  
  set.seed(1)
  yc <- seq(min(x),max(x),0.001)
  y <- dunif(yc,min = min(yc),max = max(yc))
  plot(yc,y,xlab = "data",ylab = "uniform density function",type="l")
 
}


#* @serializer png
#* @post /exp-dis
function(x,lamda) {
  
  set.seed(1)
  yc <- seq(min(x),max(x),0.001)
  y <- pexp(yc,lamda)
  plot(yc,y,xlab = "data",ylab = "exponential ditribution",type="l")
  plot(ecdf(x),  add = T, col="red")
}

#* @serializer png
#* @post /exp-den
function(x,lamda) {
  
  set.seed(1)
  yc <- seq(min(x),max(x),0.001)
  y <- dexp(yc,lamda)
  plot(yc,y,xlab = "data",ylab = "exponential density function",type="l")
  
}



#* @serializer png
#* @post /poi-dis
function(x,lamda) {
  
  set.seed(1)
  yc <- seq(min(x),max(x),0.001)
  y <- ppois(yc,lamda)
  plot(yc,y,xlab = "data",ylab = "poisson ditribution",type="l")
  plot(ecdf(x),  add = T, col="red")
}

#* @serializer png
#* @post /poi-den
function(x,lamda) {
  
  set.seed(1)
  yc <- seq(min(x),max(x),0.001)
  y <- dpois(yc,lamda)
  plot(yc,y,xlab = "data",ylab = "poisson density function",type="l")
  
}