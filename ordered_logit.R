require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

#Age function
age<-function(from, to) {
  from_lt<-as.POSIXlt(from)
  to_lt<-as.POSIXlt(to)
  
  age<-to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

#SYNTHETIC PATIENT DATA (random asthma)
patients10k<-read.csv("patients_10k.csv",header=TRUE)
smoke<-rbinom(10000,1,0.3)
poverty<-rbinom(10000,1,0.2)
ageyears<-rep(0,10000)
location<-sample(1:9,10000,replace=TRUE)
PM2.5<-rnorm(10000,12.5,2)
NO2<-rnorm(10000,23,5)
pollen<-rbinom(10000,3,0.5)

patients10k<-cbind(patients10k,smoke,poverty,ageyears,location,PM2.5,NO2,pollen)

patients10k$ageyears<-age(as.character(patients10k[,3]),Sys.Date())
patients10k$pollen<-factor(patients10k$pollen) #categorical variable

Asthma<-rbinom(10000,3,0.3)
patients10k<-cbind(patients10k,Asthma)
patients10k$Asthma<-factor(patients10k$Asthma) #categorical variable
patients10k$Sex<-as.numeric(patients10k$Sex)

pollen1<-rep(0,10000)
pollen2<-rep(0,10000)
pollen3<-rep(0,10000)
patients10k<-cbind(patients10k,pollen1,pollen2,pollen3)

for(i in 1:10000){
  if(patients10k[i,]$pollen==1){patients10k[i,]$pollen1=1}
  else{patients10k[i,]$pollen1=0}
}

for(i in 1:10000){
  if(patients10k[i,]$pollen==2){patients10k[i,]$pollen2=1}
  else{patients10k[i,]$pollen2=0}
}

for(i in 1:10000){
  if(patients10k[i,]$pollen==3){patients10k[i,]$pollen3=1}
  else{patients10k[i,]$pollen3=0}
}

#FIT MODEL
ologit <- polr(Asthma ~ Sex + smoke + poverty + ageyears + PM2.5 + NO2 + pollen, data = patients10k, Hess=TRUE)
summary(ologit)

#Change coefficients so not random
patients10k$p0<-1/(1+exp((-(3-0.01*patients10k$Sex-0.4055*patients10k$smoke-0.0953*patients10k$poverty-0.01*patients10k$ageyears-0.0953*patients10k$PM2.5-0.0953*patients10k$NO2-0.0488*patients10k$pollen2-0.0953*patients10k$pollen3))))
patients10k$cp1<-1/(1+exp((-(4.5-0.01*patients10k$Sex-0.4055*patients10k$smoke-0.0953*patients10k$poverty-0.01*patients10k$ageyears-0.0953*patients10k$PM2.5-0.0953*patients10k$NO2-0.0488*patients10k$pollen2-0.0953*patients10k$pollen3))))
patients10k$cp2<-1/(1+exp((-(6-0.01*patients10k$Sex-0.4055*patients10k$smoke-0.0953*patients10k$poverty-0.01*patients10k$ageyears-0.0953*patients10k$PM2.5-0.0953*patients10k$NO2-0.0488*patients10k$pollen2-0.0953*patients10k$pollen3))))

patients10k$p1<-patients10k$cp1-patients10k$p0
patients10k$p2<-patients10k$cp2-patients10k$cp1
patients10k$p3<-1-patients10k$cp2

for(i in 1:10000){
  patients10k[i,]$Asthma<-sample(c(0,1,2,3), size=1, prob=c(patients10k[i,]$p0,patients10k[i,]$p1,patients10k[i,]$p2,patients10k[i,]$p3))
  
}

#Fit model to non-random data
ologit <- polr(Asthma ~ Sex + smoke + poverty + ageyears + PM2.5 + NO2 + pollen, data = patients10k, Hess=TRUE)
summary(ologit)

