#Kendyl Barron
#Biostats F15
#Assignment 8

#chapter15question30

#get data
getwd()
ClawData<-read.csv("ABD_all_data/chapter15/chap15q30FiddlerCrabFans.csv")
ClawData

#seperate female subset from dataframe
female<-(subset(ClawData,crabType=="female"))
#create numerical vector of female data points
Female<-female$bodyTemperature
Female
#repeat for other three categories of treatment
maleIntact<-(subset(ClawData,crabType=="intact male"))
MaleIntact<-maleIntact$bodyTemperature
maleMinor<-(subset(ClawData,crabType=="male minor removed"))
MaleMinor<-maleMinor$bodyTemperature
maleMajor<-(subset(ClawData,crabType=="male major removed"))
MaleMajor<-maleMajor$bodyTemperature

#plot four groups' numeric data, add labels, color, title
boxplot(Female,MaleIntact,MaleMinor,MaleMajor, names=c("Female","Male Intact",
        "Male Minor","Male Major"), xlab=c("Treatment Group"),
        ylab=c("Body Temperature (degrees C/log minute)"),
        col=c("red","blue","green","orange"),
        main=c("Rates of Heat Gain vs Claw Removal Among Fiddler Crabs"))

#seperate vectors from list
ClawData$bodyTemperature
ClawData$crabType
#calculate ANOVA where treatment is the categorical explanatory variable 
#and body temperature is the numerical response
aov(bodyTemperature~crabType)
#add anova command to take the output and create an anova table
anova(aov(bodyTemperature~crabType))

attach(ClawData)
ClawData[[85]]<- NULL
ClawData
NewClawData<-paste(Female, MaleIntact,MaleMinor,MaleMajor)
stripchart(bodyTemperature~crabType,vertical=TRUE, method="jitter")


avg<-c(0.4667,0.3905,0.2619,0.2048,0.1286,0.0762)
SE<-c(0.0642,0.0642,0.0642,0.0642,0.0642,0.0642)
plot(avg)


f<-mean(Female)
mi<-mean(MaleIntact)
mj<-mean(MaleMinor)
mmn<-mean(MaleMajor)
y<-c(f,mi,mj,mmn)
x<-c(1,2,3,4)
data<-data.frame(x,y)
data
stripchart(x~y, ylim=c(1,2), ylab=c("Rate Heat Gain (C/logmin)"), 
     xlab=c("Treatment Group"), main=c("Rate Heat Gain vs Claw Treatment"))

sd<-c(sd(Female),sd(MaleIntact),sd(MaleMinor),sd(MaleMajor))
sd

for(i in 1:5)
  up = y[i] + sd[i]
  low = y[i] - sd[i]
  segments(x[i],low , x[i], up)
  segments(x[i]-epsilon, up , x[i]+epsilon, up)
  segments(x[i]-epsilon, low , x[i]+epsilon, low)
