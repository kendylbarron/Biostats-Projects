# Fitting Probability Models
# Mary Killilea
# Fall 2015


#############
# Example 8.1

# using chi squared test (chisq.test) in R
# application of chi squared to the data is obvious

# read the birth data
birthdata <- read.csv("ABD_all_data/chapter08/chap08e1DayOfBirth.csv" )
birthdata
number <- birthdata$day
number
table<-table(number)
# chisq.test default will create a simple proportional model
# remember that it will take a really large chisq to reject H_0
chisq.test(table)

# is this what we got in class? X2 = 15.05
help( chisq.test )

# create a a proportional model that works for our data
# why is it 52?
# why is it over 365?
# why is there a 53?

expected.prop = c( 52 , 52 , 52 , 52 , 52 , 53 , 52 )/365
expected.prop

chisq.test( birthdata$Number.of.births , p = expected.prop )
# are births equitably distributed over days of the week?
# data sqaured 0.01847. from chi test xsquared 14.742 palue 0.02236
# unlikely to get particular unequal distribution in data given null true. at this level
# alpha 0.05 it is significant so ?? 


###################
# Chapter 8 Example 

# using chisq.test to consider probability distributions
# evaluate the "deviation" of the observations
# trying to see how the data behaves, what distributions it "looks like"

# H_0 - the number of boys in 2 child families has a binomial distribution
# testing the fit of a distribution of data on multiple families
# NOT testing a hypothesis about a mean proportion of boys (!!!)

# testing the fit to the binomial distribution - fitting results of 
# multiple sets of trials, matching a set of frequencies to the expectation
# binomial test from last week - testing proportion (one set)
Boy.data <- read.csv("ABD_all_data/chapter08/chap08e5NumberOfBoys.csv" )
ObservedBoys <- Boy.data$Frequency
ObservedBoys
Boy.data
table <-table(Boy.data)
##sort into frequency table???
# don't know the expected probability of having a boy...
# can calculate that from data, total number sons/total number kids
obs_boys = (2*582) + 1332    # from class
p_boy = 2496/4888         # from class, chance of getting a boy
p_boy

# lets generate expected probabilities under the null hypothesis
num_children = 2    # number of events, in this case, a birth (or 2)
ExpectedProb <- c( dbinom( 0:num_children , num_children , p_boy ) )
ExpectedProb
# and translate that into expected boy numbers
ExpectedBoys <- ExpectedProb*2444

chisq.test( table, p = ExpectedProb )


#############
# Example 8.6
# Poisson distribution - describes the number of successes in blocks of time
# or space, when successes happen independently (!) of each other and occur with
# equal probability (!) at every point in time and space (spiders, MSH)

# life gets interesting when not poisson - some individuals mutation prone?
# some fishers better catchers? some plants produce better seeds?

# if occurence of extinctions is random in time, then number of extinctions 
# per block should follow poisson
# lets use chi sq GOF test
# H_0 - number of extinctions per time interval has a Poisson distribution
Extinction.Data <- read.csv( "ABD_all_data/chapter08/chap08e6MassExtinctions.csv" )

# need to estimate mu, the mean number of extinctions/time interval
# get this from the sample mean
MeanExtinctions = sum( Extinction.Data$Number.of.extinctions
                       *Extinction.Data$Frequency/76 )
MeanExtinctions


help( dpois )
# could use the data that we have to generate observed and expected values 
# (see book)
# what is the expected probability of three extinctions in a time interval?
dpois( 3 , MeanExtinctions )
#poisson distrbution. exactly analogous. expect possion

chisq.test( Extinction.Data$Frequency , p = dpois( 0:20 , MeanExtinctions ) )

# why might the chisq.test be incorrect?
# what are assumptions of chi square?
# ?
Frequency2 <- c( 13 , 15 , 16 , 7 , 10 , 4 , 2 , 9 )
sum( Frequency2 )

# combining the data from 7 - 10 to elimiate those low numbers... 
Prob <- c( sum( dpois( 0:1 , MeanExtinctions ) ) ,
           dpois( 2:7 , MeanExtinctions ) ,
           1 - ( sum( dpois( 0:7 , MeanExtinctions ) ) ) )
Prob

sum( Prob )
Prob*76

chisq.test( Frequency2 , p = Prob )
# R is giving this error because there is still one Expected Value less than 5
# would also have to account for df difference



