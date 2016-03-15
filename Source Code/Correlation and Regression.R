#Author:  Mary Killilea
#Last Updated:   November 2015
#Filename:  Correlation and Regression.R


#####################################
# Calculating and Testing Correlation

#Example 16.2

#read data into R

wolf <- read.csv("ABD_all_data/chapter16/chap16e2InbreedingWolves.csv")
head(wolf)
attach(wolf)

#plot data
plot(nPups,inbreedCoef)

#coefficent continuous variable
#test for a correlation
cor.test(nPups,inbreedCoef)
#inbreed oeff is causing the infant mortality or at least predicting.
#next step fitting a linear model that would make sense??
#correlation etst save a new variable.

#The cor.test function computes a number of useful quantities, 
#which we save in the object boobyCor. 
#The quantities can be extracted one at a time or shown all at once.

wolf.cor<-cor.test(nPups,inbreedCoef)

r <- wolf.cor$estimate
r
#calc standard error use formula to calculate yourself. manually
#should be a 2 not 3??? maybe??

SE <- sqrt( (1 - r^2)/(nrow(wolf) - 2))
SE
#SE to get CI can directly use R output.
t<-r/SE
t

#direct CI for values???
wolf.cor$conf.int

##Example 16.5 Miracles of memory Spearman Rank
#parametric methods can handel gaussian data. nonparametric do not rely on
#relationship of normality but ranks instead. ex ordinal data 
#(ex a lot not much a little)
#read in the data
trick <- read.csv("ABD_all_data/chapter16/chap16e5IndianRopeTrick.csv")
head(trick)
str(trick)
#impressiveness score marked as an integer could change to factor (ordinal type in R
#but ignore that). going to tell cor test what value to use, so doesnt matter
#score not cont num variable just arbitrary scales.

#plot the data
plot(impressivenessScore ~ years, data = trick)
#pos correlation btw years and impressiveness score
#years predictor for impressiveness score? doesnt make sense but corr coeff.


#Test of zero Spearman rank correlation. 
#In this example, the variable "impressivenessScore" is a number score 
#with lots of tied observations. Because of the ties, R will warn you 
#that the P-value in the output is not exact.
cor.test(trick$years, trick$impressivenessScore, method = "spearman")
#correlation -.28 for pearson's prdocut while spearman rank corr coeff 
#is more robust before 0.78 not its 0.49 more comparable??
#spearman correlation best? in case of massive sensitivity of outliers??

###Regression The Lion Noses

#fitting outliers. linear assoc or general assoc (w ranks)
# read data into R
LionNoses <- read.csv("ABD_all_data/chapter17/chap17e1LionNoses.csv")
head(LionNoses)
attach(LionNoses)


# get an intuition about any trends in the data by visualizing it
# initially look to see if regression seems appropriate
# i.e. any obvious clues we should STOP!
plot( proportionBlack , ageInYears )


# calculate the slope of the line
# y = m*x + b
# which variable is the independent variable (x)?
# 
# slope = covariance(x,y)/variance(x)
slope = cov( proportionBlack , ageInYears )/var(proportionBlack  )
slope

# if we know the slope, this is easy to calculate
y_intercept = mean( ageInYears ) - slope*mean( proportionBlack )
y_intercept

# linear model can also be calculated using the lm command
lm( ageInYears~proportionBlack)
#want to know whetehr slope... have positive trend but is this noise??
#if fit line to random noise can have a slope. is this slope due to data or chance

#calc p value for that. null hypoth slope 0??
# we can test the null hypothesis that the slope=0 using one of the following commands
summary( lm( ageInYears~proportionBlack ) )
#slope highly statistically signif. so sure this slope is not 0 and not by chance?
#actually above 0? also looks r^2 gives frac variance explained by model fit
#residuals from teh line outliers?? r^2 tells us 62% var explained by one feat only
#pretty good r^2 for biological data? important feature. 0.8 good fit! (for bio)

anova( lm( ageInYears~proportionBlack ) )
#summary gives actual coeff? above.. anova gives us the general analysis
#of the various kinds of variance. fitting the line means a lot of var explained by line
#nice fit then residuals very small because var explained...? 
#r^2 how good a fit it is. pvalue how likely to be just by chance.
#interpret linear regression if you get big exp nice line pos corr good r^2=0.7
#and a very small p value. if just a few points might get a very good r^2=0.7
#but not a signifcant be value 0.07. effect size good fit but not enough data
#bio interesting should do a bigger experiment. might get p value 0.001 but not
#a good r^2 missing some feature. this feature not whole story. may not be linear 
#his ex data looks exponential. conc: look at both! anova func gives var. calc r^2
#var in residual 1-83.543/(138.544+83.543) total var?? 0.62 r^2?

# we can calculate the residuals
#first calculate y-hat
#fitted values for each of the actual values that we have. yhat?
yhat <- proportionBlack*slope + y_intercept
yhat
#yhat predicted value right on the line
residuals <- ageInYears - yhat
residuals
#error? actual value minus fitted values?

#Plot the residuals to evaluate assumption
plot(proportionBlack,residuals)	
#homosedastic? var doesnt not change? not quiet. var increasing a fit
#not ideal?? there are transformation could do like sqrt but its not too bad.


#Plots with linear model can be created using the abline command
plot( proportionBlack , ageInYears )
abline( lm( ageInYears ~ proportionBlack ) )
#draw lines. fit linear model? a and b stand for intercept and slope. y=bx+a.




