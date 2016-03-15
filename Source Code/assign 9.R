#chp16q15

#set directory and read in data
getwd()
languageData<-read.csv("ABD_all_data/chapter16/chap16q15LanguageGreyMatter.csv")
#plot data into scatterplot with labeled axis and title
plot(languageData, xlab="Proficieny Score for 2nd Language", 
     ylab="Gray-Matter Density (mm^3/voxel)", main=c("2nd Language vs Gray Matter Density"))

#view dataframe and attach data so that columns within it
#can be referenced seperately. then calculate correlation w cor function
head(languageData)
attach(languageData)

#can extract calculations from cor function
#to use r value to calculate SE and t manually
lang.cor<-cor.test(proficiency,greymatter)
r <- lang.cor$estimate
r
SE <- sqrt( (1 - r^2)/(nrow(languageData) - 2))
SE
#SE to get CI can directly use R output.
t<-r/SE
t
#compare test statistic to tcrit (0.05,df=20):2.09. test stat>tcrit
#reject null hypothesis that true population correlation (p) is 0.

#read in data and attach so to refer to vectors seperately
portionData<-read.csv("ABD_all_data/chapter17/chap17q31LastSupperPortionSize.csv")
head(portionData)
attach(portionData)

#calculate the slope of the line y=bx+a
#slope=covariance(x,y)/variance(x) to find change in Y per unit X
slope = cov(year,portionSize)/var(year)
slope
#returns 0.0033

#calculate a intercept when x is 0. a=-1.1689
y_intercept = mean(portionSize) - slope*mean(year)
y_intercept

##99CONFIDENCE INTERVALS

#plot residuals, first calculate yhat
yhat <- year*slope + y_intercept
yhat
#yhat predicted value right on the line
residuals <- portionSize - yhat
residuals

#Plot the residuals to evaluate assumption
plot(year,residuals, main=c("Residual Plot of Portion Size vs Year Data"))	

#apply lm function to formula describing variable portion size to variable years
#save linear regression model in new vector portion.lm
portion.lm <- lm(portionSize ~ year)
#create new dataframe setting year value to sample size n=33
newdata = data.frame(year=33)
#predict interval type as confidence and use default 0.95
predict(portion.lm,newdata, interval="confidence") 

#test the null hypothesis that the slope=0 using linear model command
#can calculate p-value for null hypothesis. p=0.001727 <0.05 reject null.
summary(lm(portionSize~year))

