rm(list= ls()[!(ls() %in% c('ologit'))])

#READ PATIENT DATA
#Variables: Sex, DOB, Race, Ethnicity, First Name, Last Name
patients10k<-read.csv("patients_10k.csv",header=TRUE)

#Add health variables: smoker/non smoker, poverty yes/no, age in years using a function to calculate using DOB
patients10k$smoke<-rbinom(10000,1,0.3)
patients10k$poverty<-rbinom(10000,1,0.2)

#Age function
age<-function(from, to) {
  from_lt<-as.POSIXlt(from)
  to_lt<-as.POSIXlt(to)
  
  age<-to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

#Add ages
patients10k$ageyears<-age(as.character(patients10k[,3]),Sys.Date())

#Randomly allocate each patient to a grid square (using 3 x 3 grid, labelled 1 to 9)
patients10k$location<-sample(1:9,10000,replace=TRUE)

#Make Sex variable binary (M=1, F=0)
patients10k$Sex<-factor(patients10k$Sex,levels=c("M","F"),labels = c(1,0))
patients10k$Sex<-as.numeric(patients10k$Sex) #convert from factor to numeric so can run regression
patients10k$Sex<-patients10k$Sex%%2 #numeric makes 0 into 2, don't know why? mod2 of column to make 0 again



#POLLUTION

#input grid dimensions (model only works for 3 x 3 now, but have used n=3 and m=3 where possible to make it easier to change to other dimensions later)
n<-3
m<-3

#Pollution
x<-seq(0,730*pi,pi/12) #x is time, 2pi is one day, 730pi is a year, pi/12 is one hour
hour<-x/(pi/12) #add hourly scale
q<-runif(length(x),0.75,1.25) #random variation scalar
PM2.5<-q*(1.5*sin(x)+12.5) #synthetic PM2.5 data cycles over a day
NO2<-q*(3*sin(x/365)+23) #synthetic NO2 data cycles over a year
Pollen<-sample(c(0,1,2,3), size=length(x), replace=TRUE, prob=c(0.2,0.4,0.3,0.1)) #synthetic pollen counts recorded as very low, low, medium, high
PollutionMatrix<-cbind(x,hour,PM2.5,NO2,Pollen) #make table of all pollution factors

#Plot pollution graph
plot(x,NO2,type='l',xlim=c(0,240),ylim = c(0,35))
lines(x,PM2.5)
points(x,Pollen)

#Make function Pol(t) using variables set above. Will predict some pollution level given a time (hour) input
Pol<-function(t){
  P<-subset(PollutionMatrix,hour==t,select = c(PM2.5,NO2,Pollen))
  P<-as.vector(P)
  return(P)
}

#Create empty table of local pollution at time (to be given in Pol function later)
PM2.5L<-rep(0,n*m)
NO2L<-rep(0,n*m)
PollenL<-rep(0,n*m)
LocalPollution<-cbind(PM2.5L,NO2L,PollenL)

#Random scalars for each grid square to give local variation
r<-runif(n*m,0.75,1.25)

#Add values using Pol function and local variation scalars
for (i in 1:(n*m)){
  LocalPollution[i,1]<-r[i]*Pol(5)[1]
  LocalPollution[i,2]<-r[i]*Pol(5)[2]
  LocalPollution[i,3]<-Pol(5)[3]
}
####################################################################
#BACK TO PATIENT DATA TABLE

#Fill in pollution values from LocalPollution table using location variable to match
patients10k$PM2.5<-LocalPollution[patients10k$location,1]
patients10k$NO2<-LocalPollution[patients10k$location,2]
patients10k$pollen<-LocalPollution[patients10k$location,3]

#Make pollen into categorical variable so can predict using ordered logit model
patients10k$pollen<-factor(patients10k$pollen)




#PREDICT NEW ASTHMA


patients10k<-cbind(patients10k,predict(ologit, newdata = patients10k, type="probs"))

patients10k$Asthma<-rep(0,10000)

for(i in 1:10000){
  patients10k[i,]$Asthma<-sample(c(0,1,2,3), size=1, prob=c(patients10k[i,17],patients10k[i,18],patients10k[i,19],patients10k[i,20]))
  
}

AsthmaEx<-table(patients10k$location,patients10k$Asthma)

#add n and m grid square 'coordinates'
patients10k$n<-ceiling(patients10k$location/3)
patients10k$m<-patients10k$location%%3
for(i in 1:10000){
  if(patients10k[i,]$m==0){patients10k[i,]$m=3}
}

#separate variables for severity of asthma
patients10k$Mild<-rep(0,10000)
for(i in 1:10000){
  if(patients10k[i,]$Asthma==1){patients10k[i,]$Mild=1}
  else{patients10k[i,]$Mild=0}
}

patients10k$Mod<-rep(0,10000)
for(i in 1:10000){
  if(patients10k[i,]$Asthma==2){patients10k[i,]$Mod=1}
  else{patients10k[i,]$Mod=0}
}

patients10k$Sev<-rep(0,10000)
for(i in 1:10000){
  if(patients10k[i,]$Asthma==3){patients10k[i,]$Sev=1}
  else{patients10k[i,]$Sev=0}
}

MildMat<-xtabs(Mild~n+m,patients10k)
ModMat<-xtabs(Mod~n+m,patients10k)
SevMat<-xtabs(Sev~n+m,patients10k)

#plot
#mild
MildMatMelt<-melt(MildMat)
names(MildMatMelt)<-c("n","m","people")

MildMatMelt$people=factor(MildMatMelt$people>400)
levels(MildMatMelt$people)=c("less than 400","more than 400")

qplot(m,n, fill=people, data=MildMatMelt,geom='tile',asp=1,xlim = c(0,(m+1)),ylim = c((n+1),0),main="Mild exacerbations")

#moderate
ModMatMelt<-melt(ModMat)
names(ModMatMelt)<-c("n","m","people")

ModMatMelt$people=factor(ModMatMelt$people>300)
levels(ModMatMelt$people)=c("less than 300","more than 300")

qplot(m,n, fill=people, data=ModMatMelt,geom='tile',asp=1,xlim = c(0,(m+1)),ylim = c((n+1),0),main="Moderate exacerbations")

#severe
SevMatMelt<-melt(SevMat)
names(SevMatMelt)<-c("n","m","people")

SevMatMelt$people=factor(SevMatMelt$people>200)
levels(SevMatMelt$people)=c("less than 200","more than 200")

qplot(m,n, fill=people, data=SevMatMelt,geom='tile',asp=1,xlim = c(0,(m+1)),ylim = c((n+1),0),main="Severe exacerbations")
