# WLS
WLS Regression

n<-500
b0<-1
b1<-1
set.seed(123)
x<-runif(n,1,10)
s2<-0.1*x^2

set.seed(123)
e<-rnorm(n,0,s2^0.5)
y<-1+1*x+e

plot(y~x,lwd=1,pch=19,col="black",main="Contoh Heteroskedastisitas")
abline(1,1,col="red")

fit1<-lm(y~x)
a0<-fit1$coefficient[1]
a1<-fit1$coefficient[2]

abline(a0,a1,col="blue")

summary(fit1)

#WLS

fitWLS<-lm(y~x,weight=1/s2)
summary(fitWLS)

c0<-fitWLS$coefficient[1]
c1<-fitWLS$coefficient[2]

abline(c0,c1,col="green")

legend("topleft",
c("TRUE","OLS","WLS"),
fill=c("red","blue","green"))

###############
#uji heteros (breusch pagan test)
#install.packages("lmtest")
library(lmtest)
bptest(fit1)

residual=fit1$residuals
absresidual=abs(residual)
cor(x,absresidual,method="spearman")
r=cor(x,absresidual,method="spearman")
#uji rank spearman
t1=r*(n-2)^0.5/(1-r^2)^0.5
t1


#heteros tidak bermsalah di ketakbiasan tetapi bermasalah di standard error
