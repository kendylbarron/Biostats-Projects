# Analyzing proportions
# Mary Killilea
# Fall 2015



###########################
# right-handed toad example

# gives the probability for each value from 0-18
# from a binomial distribution with a probability of .5 (for success)
# vector of quantities = 0 to 18, size is 18, prob is .5 (they are equal)
dbinom<- dbinom( 0:18 , 18 , .5 )
barplot(dbinom)
# notice that 0:18 produces a range
0:18

# how could this be useful?
# 

# pbinom will give you the probability for all values less than or 
# equal to 13 with 18 trials and a probability of .5
pbinom( 13 , 18 ,.5 )
pbinom(16,18,.5)
#equivalent of going to table and looking up

# adjust the code below to get the same value as above
sum( dbinom( 14:18 , 18 , .5 ) )
sum( dbinom(0:13, 18, .5))

# if you make "lower.tail=FALSE" then its the values greater than 13
pbinom( 13 , 18 , .5 , lower.tail = FALSE )

pbinom( 13 , 18 , .5 , lower.tail = TRUE )
p<-2*(1-pbinom(16,18,.5))
p
# how could you check this?
# P(x <= 13) + P(x > 13) = 1
# 
1 - pbinom(13,18,.5,lower.tail = TRUE)

# why does this work?
# 


######################
# calculating p-values

# calculating the p-value for a two-sided test is acheived by multiplying
# the probability by 2, representing both sides of the distribution

# in lecture, we wanted to know the prob of greater than 13, but we had a 
# two sided test (must add up probabilities above and below 13, or mult by 2)
pvalue = 2*( 1 - pbinom( 13 , 18 , .5 ) )
pvalue

# similarly, you could sum the values from 14-18
pvalue2= 2*sum(dbinom(14:18,18,.5))
pvalue2

# what does multiplying by 2 really represent?
# P(x > 13) = P(x < ?)
# 
# is the distribution symmetric?
# 
pbinom( ? , 18 , .5 )
#prob that 4 or less put ?=4

pbinom( ? , 18 , .5 ) + 1 - pbinom( 13 , 18 , .5 )
#lower tail= ?=4 and uppertail/second part 14-18, so it is symmetric???
#normal distributions are symmetric. one sided test just looks at tail end
#two sided test looks at both tails, end and beginning.
# what does this p value tell us?
# 


#########################
# plotting a distribution

help( barplot )
# R understands multi-line input (in several contexts, such as searching a ")"
# matching a ")")
# from now on, we will try to represent plots in this multi-line format to
# emphasize the options
barplot( dbinom( 0:18 , 18 , .5 ) ,
    space = 0 ,
    names.arg = 0:18 ,
    cex.names = 0.6 ,
    ylim = c( 0 , 0.20 )
    )
#histogram equivalent if space=0

# you can also have R test the null hypothesis for you
help( binom.test )
binom.test( 14 , 18 , .5 , alternative = "two.sided" )
#enough evidence to reject the null? 0.05 alpha. so statistical sign. reject null.
#also calculate confidence intervals for you
2*(sum(dbinom(14:18,18,.5)))

# how do we interpret this result?
# reject null. statistical significance. compare to alpha 0.05
# do we accept the null hypothesis?
# reject null


