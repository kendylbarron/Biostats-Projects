## Can we meet the assumptions
#  Mary Killilea
#  Updated Fall 2015

getwd()

##Calculating the standard deviates for normal quantile plots

biomass.ratio<-read.csv("ABD_all_data/chapter13/chap13e1MarineReserve.csv")
head(biomass.ratio)
#null not 0 but 1 for the ratio???? check if normally distributed.

attach(biomass.ratio)
#puts columns directly into work space, dont have to specify 
#refer to columns of dataframe without haveing to specify

#interally order them and rank them. first element 13th biggest element.. etc?
i<-rank(biomassRatio)
i

#know the rank. know 32 data points. know quartile and proporiton data it covers.
#from franks to proportions ex 39 elements smaller than it?? quantiles. relative ranking.
#of 100 percentile of 4 quartile. of n quantile.
proportion<-i/(32+1)
proportion

#cumulative distribution fucntion. culmination of distribution?? area under curve?
#percentage lower than that...? number true normal give us same quantiles as actual data
#these numbers should be the same? should plot along a diagonal line?
qnorm(proportion)

plot(biomassRatio,qnorm(proportion))
#actual data (x) vs normal dist. not diagonal. therefore data is not normally distributed.
#if diverge a little its ok but this is curved. not normal data.

###normal quantile plot command
#qqnorm (makes qqnrom plot) comparing against the normal distribution.
#lets you compare anything? here plot against normal distribution.
#first element smallest in actual data and smallest in theoretical data??
#axis not measured in quantiles but units of data... dont let label confuse you??
qqnorm(biomassRatio,datax=TRUE)
help(qqnorm)

#what does datax do? look at help. just flips the axis?? usually theoretic x empirical y.

#Shapiro-Wilk test for normality (null hypothesis is population is normal)

shapiro.test(biomassRatio)
#test for normality. p value is very smalle <0.05 so reject (not normal)
#for t test all theory based on data being normal but robust if divergence still works fine
#as long as data is symmetricalish... ok even if dont exactly fit assumptions. parametric
#better when data small?? book specifies in more detail. large data set small divergence
#will reject normality? small test will fit when it shouldnt?? 


#transform biomass.ratio

hist(biomassRatio)
#not symmetrical

hist(log(biomassRatio))
#still not symmmetrical but helping
qqnorm(log(biomassRatio),datax=TRUE)
#better fit but still departing too much from diagonal to be data. there must be better
#transformation of the data available to make it fit normal distribution better.

#3rd option is to perform:
##Non-parametric tests (or nonpermutation test with computer???)
##Sign test on page 99 of Dalgaard


Sexual.Selection<-read.csv("ABD_all_data/chapter13/chap13e4SexualConflict.csv")
head(Sexual.Selection)

attach(Sexual.Selection)

#Create a dataframe that only includes the data for taxonpairs with negative differences
#subset to extract data from dataframe. select certain rows and columns.
negative<-subset(Sexual.Selection,difference<0)
negative

#x number of rows when just negative. n number of all rows total
x<-nrow(negative)
n<-nrow(Sexual.Selection)
#if null is true expect 50 50 negative and positive. simple sign test. binomial test.
#is the sign equally distributed.
2*pbinom(7,25,.5)

2*pbinom(x,n,.5)
#p<0.05n sign column not equally distributed +/- which indicates some difference present??


#wilcox.test is can be used instead Mann Whitney 

cannibalism <- read.csv("ABD_all_data/chapter13/chap13e5SagebrushCrickets.csv")
head(cannibalism)
attach(cannibalism)


#fancy code to create multiple histograms
library(lattice)
#plotting package, built in. for plotting graphs next to eachother.
#two historgraphs starved and fed. (dont look normally distribtued..)
histogram( ~ timeToMating | feedingStatus, data = cannibalism,
           layout = c(1,2), col = "firebrick", breaks = seq(0, 100, by = 20),
           type = "count", xlab = "Time to mating (hours)", ylab = "Frequency")

#same as two sample t test? numerical ~ categorical seperating pieces of data.
#Wilcox test on time to making based on the feeding status
wilcox.test(timeToMating ~ feedingStatus, data = cannibalism)
#dataframe with two vectors (wide format) or stacked format:
#columns which syas which group each are in. like the data for cannibalism. more convenient. 

#Permutation test of the difference between mean time to mating of starved 
#and fed crickets.

#Begin by calculating the observed difference between means 
#(starved minus fed). The difference is -18.25734 in this data set.
cricketMeans <- tapply(cannibalism$timeToMating, cannibalism$feedingStatus, mean)
cricketMeans
diffMeans <- cricketMeans[2] - cricketMeans[1]
diffMeans

#permutation test.. response time of mating and feeding status explanatory??
#first calculate mean of subgroups which is done above. better is long format.
#column with category specify which group data goes into. use tapply??
#Decide on the number of permutations.
nPerm <- 10000
#calculating test statistic?? in our actual data. how big is the difference of the means?
#18 units (hours?) difference between these two groups.. p value evidence against null.
#if null true how unlikely see such a big value. this is what we are directly simulating
#in a permutation test. easy two sample cases.

#Create a loop to permute the data many times (determined by nperm). 
#In the loop, i is just a counter that goes from 1 to nPerm by 1; 
#each permuted difference is saved in the permResult.

permResult <- vector() # initializes
for(i in 1:nPerm){
  # step 1: permute the times to mating
  permSample <- sample(cannibalism$timeToMating, replace = FALSE)
  # step 2: calculate difference betweeen means
  permMeans <- tapply(permSample, cannibalism$feedingStatus, mean)
  permResult[i] <- permMeans[2] - permMeans[1]
}
#shuffle data null distribution??
#permutation tests very few assumptions...

#Plot null distribution based on the permuted differences (Figure 13.8-1).
hist(permResult, right = FALSE, breaks = 100) 
#100 means.

#Use the null distribution to calculate an approximate P-value. 
#This is the twice the proportion of the permuted means that fall 
#below the observed difference in means, diffMeans (-18.25734 in this example). The following code calculates the number of permuted means falling below diffMeans.
sum(as.numeric(permResult <= diffMeans))
#how likely get -18 or bigger?? or n value or bigger/lesser?
#These commands obtain the fraction of permuted means falling below 
#diffMeans.
sum(as.numeric(permResult <= diffMeans)) / nPerm


#Finally, multiply by 2 to get the P-value for a two-sided test.
2 * ( sum(as.numeric(permResult <= diffMeans)) / nPerm )


#t test very robust to differences in variants. subtle differences ok??
# Example of F test of equal variance on the lizard data from Chapter 12

# Read the lizard data from Chapter 12
lizard.data<-read.csv("ABD_all_data/chapter12/chap12e3HornedLizards.csv")
head(lizard.data)

#subset the data into 2 vectors based on living or dead lizards

living<-subset(lizard.data,Survival=="living")
living

killed<-subset(lizard.data,Survival=="killed")
killed

#perform an F test on the lizard data
help(var.test)
var.test(living$squamosalHornLength,killed$squamosalHornLength)
