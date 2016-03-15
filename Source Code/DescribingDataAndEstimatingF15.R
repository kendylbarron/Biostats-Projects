## Describing Data
#  Mary Killilea
#  Updated Fall 2015


#######

# Check that your working directory is correct
getwd()


# follow along as we go through this script
# Ctrl Enter will run any individual line of code
# don't just trust that the script we provided works, check it yourself!


############
# Example 3.1 Gliding Snakes

# Read the snake undulation data from the chapter 3 folder
SnakeData<- read.csv("ABD_all_data/chapter03/chap03e1GlidingSnakeUndulations.csv")

# Make sure the data read into R properly and look at headers
SnakeData

# what is the mean?
# why is it useful?

# the mean function will calculate the mean of a vector.  
# mean will not work on the data frame so you need 
# to tell it what vector to use with the "$"

mean(SnakeData$undulationRate ) 

# if you are going to use the vector several times it might be worth 
# creating a new vector

undulation<-SnakeData$undulationRate
undulation

mean(undulation)

#take a look at the frequency distribution.  The default looks close to the book
hist(undulation) 

help(hist)

hist(undulation, breaks=3)
  
# the median -- the middle observation in a set of data (splits the data in two)

median( undulation )

# means that 50% of the counts are above 1.35 and 50 are below


# the quantile - partitions the data based on given probabilities   
?quantile

quantile( undulation )

# maybe we want all these descriptive statistics
summary( undulation )


sd(undulation)
var(undulation)


a#########################
# more summary statistics

# data from Table3.1-2
# calculating descriptive statistics from a frequency table
# Read in the conviction frequency data.  This is not the raw data

ConvictionData <- read.csv("ABD_all_data/chapter03/chap03t1_2ConvictionsFreq.csv")
ConvictionData

#Create vectors of number of convictions and frequencyy
No.Convictions <- ConvictionData$convictions
Frequency <- ConvictionData$frequency
Frequency   
# lets work backwards - we don't want the frequency, we want to split the data
# into each individual person and how many times they've been convicted

help( rep.int )

# what are we doing?

rawdata <- rep.int( No.Convictions,Frequency )
ConvictionData
rawdata
median( rawdata )
# sum( rawdata <= 0 )
# sum( rawdata > 0 )
# length( rawdata )

quantile( rawdata )

# IQR is InterQuartile Range - shows span of data
# (difference of the third and first quantiles)
IQR( rawdata )
mean( rawdata )

# how is this calculated?
summary( rawdata )
summary( ConvictionData )

sd(rawdata)


###################################
# Example 4.1 The lenght of human genes
# Describe the parameters of a known population of human gene lengths. 
# Then, take a random sample from the known population to estimate the 
# population mean.

getwd()
# Read the human gene length data, which we will use as our known population of measurements.

humanGeneLengths <- read.csv("ABD_all_data/chapter04/chap04e1HumanGeneLengths.csv")
head(humanGeneLengths)


# Draw a histogram of the length of genes in the human genome (Figure 4.1-1). 
# We first use subset to grab the large majority of genes that are 15,000 nucleotides 
# or less in length and put into a second data frame. 

geneLengthsUpTo15K <- subset(humanGeneLengths, geneLength <= 15000)
hist(geneLengthsUpTo15K$geneLength, right = FALSE)

# A fancier histogram that makes use of additional options can be produced 
# with the commands here.
hist(geneLengthsUpTo15K$geneLength, breaks = seq(0,15000,500), 
     xlab = "Gene length (Number of nucleotides)", ylab = "Frequency", 
     col = "firebrick", las = 1, main = "", right = FALSE)
??hist
# Calculate the population mean and standard deviation (Table 4.1-1). 
# Just this once we must use the total number of genes N instead 
# of N - 1 in the denominator when calculating the variance and standard deviation, 
# because we treat all the genes of the human genome as a population for this exercise,
#not as a sample. For this reason we can't use the built-in commands to calculate 
#variance and standard deviation, because they divide by N - 1.

meanGeneLength <- mean(humanGeneLengths$geneLength)
meanGeneLength

N <- nrow(humanGeneLengths)
N
help(nrow)
varGeneLength <- sum( (humanGeneLengths$geneLength - meanGeneLength)^2 ) / N
sdGeneLength <- sqrt(varGeneLength)
sdGeneLength


# Take a single random sample of 100 genes from the population of genes. 
# The argument replace = FALSE ensures that the same gene is not sampled 
# twice. Save your random sample to a vector. 
# Note: your sample won't be the identical to the one in the book, 
# because each random sample is subject to sampling error.

geneSample100 <- sample(humanGeneLengths$geneLength, size = 100, replace = FALSE)

# Draw a histogram of the unique random sample.

hist(geneSample100, right = FALSE)

mean(geneSample100)
sd(geneSample100)


# Calculate the standard error of the mean gene length for the unique sample 
# of 100 genes. The length command indicates the number of elements in a vector 
# variable, which is the sample size if there are no missing (NA) elements in the vector. 
# You won't get the same value for the standard error as we obtained (146.3, p 102) because your unique random sample will not be the same as ours.

n <- length(geneSample100)
SE<-sd(geneSample100) / sqrt(n)
SE

# Create a loop to take repeated random samples from the population and 
# calculate the mean on each sample. This generates the sampling distribution 
# of the mean. Take a large number (10,000) of random samples, each of size 100. 
# On each iteration, the sample mean is calculated and saved in a vector named results100 
# (the samples themselves are not saved). The results vector is initialized before the loop. 
# The term results100[i] refers to the ith element of results100, where i is a counter. 
# This many iterations might take a few minutes to run on your computer. 

results100 <- vector() 
for(i in 1:10000){
  temporarySample <- sample(humanGeneLengths$geneLength, size = 100, 
                            replace = FALSE)
  results100[i] <- mean(temporarySample)
}

results100

#plot the histogram of the results
samp.dist<-hist(results100, breaks = 50, right = FALSE)

#convert the frequency to relative frequency.  
#There are several ways to do this but I found the histogram tools
# to be the easiest

install.packages("HistogramTools")
library(HistogramTools)
PlotRelativeFrequency(samp.dist,ylab="RelativeFrequency")
##read in and download library of sorts??


# Figure 4.4-1. Locust serotonin
# Draw a strip chart with standard error bars for the locust serotonin data. The data are from Chapter 2 (Figure 2.1-2).
# Read and inspect the data.

locustData <- read.csv("ABD_all_data/chapter02/chap02f1_2locustSerotonin.csv")


# Begin by drawing the strip chart. Keep the graphic window open because we'll be adding the error bars to this plot.

stripchart(serotoninLevel ~ treatmentTime, data = locustData, 
           method = "jitter", vertical = TRUE)
#plot missing labels and legend describing statistical symbols... 
#ex SEM can be confused w 95%confidenceinterval.


# Now calculate the statistics by group needed for the error bars: 
# the mean and standard error. 
# Here, tapply is used to obtain each quantity by treatment group. 

meanSerotonin <- tapply(locustData$serotoninLevel, locustData$treatmentTime, mean)
meanSerotonin
#apply mean to column above. break up according to treament time factor.
#categorical column treatment time.

sdSerotonin <- tapply(locustData$serotoninLevel, locustData$treatmentTime, sd)
nSerotonin <- tapply(locustData$serotoninLevel, locustData$treatmentTime, length)
seSerotonin <- sdSerotonin / sqrt(nSerotonin)
#calculate statistics seperate for each of these things. formulas w plug ins.
#can covert between them but list on the plot what it is.

# Finally, add the error bars to the strip chart. Offset their position (a constant offsetAmount is used below) so that they are drawn to the right of the data points. Somewhat confusingly, the treatments are "0", "1", and "2", but the positions of points for the three treatments along the x-axis are 1, 2, and 3. This is because the treatment variable is a factor, whose first level is always at position 1, the second is at 2, and so on.

offsetAmount <- 0.2
segments( c(c(1,2,3) + offsetAmount), meanSerotonin - seSerotonin, 
          c(c(1,2,3) + offsetAmount), meanSerotonin + seSerotonin)
points(meanSerotonin ~ c(c(1,2,3) + offsetAmount), pch = 16, cex = 1.2)



###########################Practice####################
#Use the data for chap04q09NumberGenesRegulated.csv to practice what you learned
# read the data in
# plot the data and calculate the summary statistics

getwd()
NumberGenesRegulated <- read.csv("ABD_all_data/chapter04/chap04q09NumberGenesRegulated.csv")
NumberGenesRegulated

data<-c(0.86,1.02,1.02,1.01,1.02,1,0.99,1.01,0.91,0.83,1.01 )
meandata <- mean(data)
median(data)
var(data)
sd(data)
sort(data)
len(data)

vardata <- sum( (data - meandata)^2 ) / 11