# Introduction to the Normal Distribution 
# Mary Killilea
# Fall 2015

#Check working directory:

getwd()

#British spies example
#MI5: Spies can be no taller than 180.3cm
#General population: mean 177.0cm with standard deviation 7.1cm, 
#Normally distributed
#Question - What proportion of British men are excluded?

#Remember properties of the normal distribution:
#It is a continuous distribution so probability is measured 
#by the area under the curve 
##rather than the height
#It is symmetric around the mean
#It has a single mode
#Probability density is highest exactly at the mean
#Mean, median and mode are all equal

#the pnorm example below gives you the upper.tail or probability 
#of a 180.3 or greater given the mean and standard deviation

pnorm(180.3,mean=177,sd=7.1,lower.tail=F)

help(pnorm)

#using Z:
z = (180.3 - 177)/(7.1)
z
#z how many sd from mean
pnorm(z, lower.tail = F)

#z value tells you how many standard deviations a particular value 
#is from the mean
z*7.1


# Central Limit Theorm
# Spanish Flu in Switzerland

#Read the flu data
flu <- read.csv("ABD_all_data/chapter10/chap10e6AgesAtDeathSpanishFlu1918.csv")
head(flu)
flu

#Histogram of the flu data
#Figure 10.6-1
hist(flu$age, right = FALSE)
#not at all normally dist.


# Demonstration of the central limit theorem. 
# Treat the age at death measurements from Switzerland in 1918 as the population.
# Take a large number of random samples, each of size n, from the population
# of age at death measurements and plot the sample means. 
# Note: your results won't be the identical to the one in Figure 10.6-2, 
# because 10,000 random samples is not large enough for extreme accuracy. 
# After you run the code a couple of times and see the changes.
# Change n below to another number and rerun to see the effects of sample 
# size on the shape of the distribution of sample means.

n <- 4
results <- vector()
for(i in 1:10000){
  AgeSample <- sample(flu$age, size = n, replace = FALSE)
  results[i] <- mean(AgeSample)
}

# plot the data generated above
hist(results, right = FALSE, breaks = 50, col = "firebrick", las = 1, 
     xlab = "Mean age at death (yrs)", ylab = "Frequency", main = "")

#probability of 128 or greater given the mean and standard deviation
pnorm(128,mean=123.8,sd=3.1,lower.tail=F)

#different method to get the same value using Z the standard normal deviate
z = (128 - 123.8)/(3.1)
pnorm(z, lower.tail = F)

#probability of 120 or less given the mean and standard deviation
#lower.tail=T becuase now looking for less than rather than greater than
pnorm(120, mean=123.8, sd=3.1, lower.tail=T)
z=(120-123.8)/(3.1)
pnorm(z, lower.tail=T)


chp11q21

#set working directory
getwd()
#get data
SoilData<-read.csv("ABD_all_data/chapter11/chap11q21SoilLeadAndHurricanes.csv")

#select log ratio vector from dataframe
LogRatio<-SoilData$LogRatio
LogRatio
mean(LogRatio)

#graph with number of bin breaks to visualize normality, edit color and labels
hist(LogRatio, breaks=10, col=c("red"), xlab=c("LogRatio of Change in Soil Lead(mg/kg)"),
     main = c("Soil Lead After Hurricanes Katrina and Rita"))

#find the mean of the data set
MeanChange<-mean(LogRatio)
MeanChange
#find the standard deviation of the data set
ChangeSD<-sd(LogRatio)
ChangeSD
#use the sd to find the standard error of the data set
SEChange<-ChangeSD/sqrt(46)
SEChange

#find the t-statistic which equals (Y-bar)-(mu assumed in null hypothesis)/(SE)
t=(MeanChange-0)/SEChange
t
#t=-3.22, but t of 0.05(2) and 45 degrees of freedom =2.01. our calculated t-statistic
#does not fall within the range of -2.01 and 2.01 so reject the null hypothesis

#find the p value given the t-statistic. degrees of freedom= n-1= 46-1=45
#multiply by two because the test is two-tailed
2*pt(t,45)
#p=0.00236 therefore <0.05 so reject null.
