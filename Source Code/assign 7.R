#Assignment 7

#chapter13question21

#get data
getwd()
MatingData<-read.csv("ABD_all_data/chapter13/chap13q21StressAndIncompatibleMates.csv")
head(MatingData)

#attach to reference vectors of dataframe indepdently
attach(MatingData)
#get differences in corticosterone concentration for each female for different matings
diff<- corticosterone_concentration_compatible-corticosterone_concentration_incompatible
diff

#internally order and rank measurments
i<-rank(diff)
i
#calculate proportion of distribution below an observation as i/n+1
proportion<-i/(43+1)
proportion

#use cumulative  distribution function
qnorm(proportion)

#view actual data vs normally distributed data, not diagonal. data isnt normally distributed
plot(diff,qnorm(proportion))
#another method. generation of Normal Q-Q Plot to detect deviation from normality in data.
qqnorm(diff,datax=TRUE, xlab=c("Normal Quantiles"),
       ylab=c("Differences in Stress Levels with Compatibility of Mates (ng/ml)"))

shapiro.test(biomassRatio)

#number of negative values in list (43 entries long)
x<-sum(diff<0)
#returns 42. so positive values must be 1.

#use the cumulative probability function for binomial distribution given null assumption p=0.5
pbinom(1,size=43,prob=0.5)
#p=5.00*10^12 much less than 0.05 standard. so reject null. the stress levels are not the same
#between females with corresponding

