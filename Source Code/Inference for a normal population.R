# Inference for a normal population
# Mary Killilea
# Fall 2015

# Student's t is the result of substituting the standard error of the mean
# that we estimate from our data
# for the population parameter standard error of the mean

# t = the sample mean - population mean / standard error of the mean 


#The undulation rate (Hz) of Paradise snake:
#Ybar = 1.375 Hz, s = 0.324, n = 8

help(pt)

#to calculate t for 95% confidence intervals you just need to use .975 
#for the probability because it uses the lower.tail of the 
#distribution and the df (n-1)


#t for the paradise snake example can be calculated
#recall our df was 7 (8 snakes total):
#qt will give you the t value - you give it the probability, 
#it gives you the number
#whose cumulative probability matches that number

#t for the paradise snake example can be calculated:e
help(qt)
t = qt(.975,7)
t



#confidence interval: Ybar +- s/sqrt(8) * t
1.375 + .324/sqrt(8) * t
1.375 - .324/sqrt(8) * t

#pt - give a number - it computes the probability that a norm distrb
#random number will be less than that number

pt(2.365,7)



##Example 11.3
#One-sample t-test example
#Human body temperature.  
# How well does the data below support the null hypothesis that the human 
# body temperature is 98.6

# Read the temperature data into R
HumanTemp<-read.csv("ABD_all_data/chapter11/chap11e3Temperature.csv")
head(HumanTemp)
HumanTemp

# Create a numerical vector called temperature
temperature<-HumanTemp$temperature
temperature


# You can manually calculuate t using the formula below and then calculate a p-value with the pt command
#remember pt calculates the area under the curve to the left of t (if your value is negative this is correct if it is positive you use 1-t)

MeanTemp<-mean(temperature)
MeanTemp

sdTemp<-sd(temperature)
SETemp<-sdTemp/sqrt(25)
SETemp

t=(MeanTemp-98.6)/SETemp
t

2*pt(t,24)

# the t.test command below is comparing the mean value of the daily 
# intake data with a mu of 7725

t.test(temperature,mu=98.6)
