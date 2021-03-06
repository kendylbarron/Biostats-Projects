# Mary Killilea 
# Probability and Hypothesis Testing
# Sept 2015

# please add notes and comments during recitation########
# don't forget to set your working directory


##################################################
#Probability


###For the purpose of this course you will be doing most of your probability
###work by hand, but here are a few functions to play with.
###There is also a Chapter on Probability in the book:
###Introduction to Probability and Statistics Using R (availble on NYU Classes)

#Simulating Data

#Code to similate flipping a coin 10 times assuming an equal probability of H or T
sample(c("H","T"), 10, replace=T)

#sample fucntion take random sample from data, specifiy number of samples.
#ex 10 coin flips. replace=T because independent events. defaults to replace=T

#If probability of 2 events is not even (10% and 90%)
sample(c("success","failure"), 10, replace=T, prob=c(.9,.1))
#weighted coin. success or heads, 90% of time.

#Calculating probabilities
#Calculating the probability of rolling a 4 on a six sided die
1/6

#The probability or rolling 3 on the first and second roll (multiplication rule)
1/6^2
1/36


# Example 6.2. The right hand of toad
# Obtain the null distribution for the test statistic 
# (the number of right-handed toads out of 18).
# Take a very large number of samples of 18 toads from a theoretical 
# population in which left- and right- handed individuals occur with 
# equal probability (as stipulated by the null hypothesis). 
# For each sample, the number of right-handed toads is calculated and 
# saved in a vector named results18 (the samples themselves are not saved). 
# The results vector is initialized before the loop. The term results18[i] 
# refers to the ith element of results18, where i is a counter. 
# This many iterations might take a few minutes to run on your computer. 

results18 <- vector()
for(i in 1:10000){
  tempSample <- sample(c("L", "R"), size = 18, prob = c(0.5, 0.5), replace = TRUE)
  results18[i] <- sum(tempSample == "R")
}
#test statistic for sample size 18. UNDERSTAND LOOPS. for i new variable, 1 to 10000
#1:10000 vector 10000 long. i as index element last line. empty vector???
results18

# Save results18 also as a factor with 19 levels, representing each 
# possible integer outcome between 0 and 18. 
# This step is for tables and graphs only -- it makes sure that all 19 categories are included 
# in tables and graphs, even categories with no occurrences.

factor18 <- factor(results18, levels = 0:18)
factor18
#factor18 with 19 levels. results 18 discrete numerical values convert into factor.
#R table command wants a factor. assign variables factors or numerical so R knows what to do.
# Tabulate the relative frequency distribution of outcomes in the random sampling process (Table 6.2-1). This is the null distribution.
#Your results will be similar but not identical to the numbers in Table 6.2-1 because 10,000 times is not large enough for extreme accuracy.
#The table command calculates the frequencies, and dividing by the length of results18 (i.e., the number of elements in the vector, equal to the number of iterations in the loop) yields the proportions (relative frequencies). 

nullTable <- table(factor18, dnn = "nRightToads")/length(factor18)
data.frame(nullTable)

# Draw a histogram of the null distribution: the number of right-handed toads out of 18.
#Setting freq = FALSE causes hist to graph density of observations rather than frequency. In this case, height of bars indicates relative frequency if bars are one unit wide (as in the present case).  

hist(results18, right = FALSE, freq = FALSE,
     xlab = "Number of right-handed toads")

#centered at 9. simulating the null.
# Commands for a fancier histogram are shown here.

# We can make a histogram using "barplot" with the height of the bars
# as proportions, as given in "nullTable"
barplot(height = nullTable, space = 0, las = 1, cex.names = 0.8, col = "white",
        xlab = "Number of right-handed toads", ylab = "Relative frequency")

# Calculate the fraction of samples in the null distribution having 14 or more right-handed toads. 
#This won't be exact because 10,000 times is not large enough for extreme accuracy.

frac14orMore <- sum(results18 >= 14)/length(results18)
frac14orMore

# Calculate the approximate P-value. This won't be exact because 10,000 times is not large enough for extreme accuracy.

2 * frac14orMore
#p value objective measure. 0.05 traditional but weak.
# ------------------------------------------------------------

# Example 6.4. The genetics of mirror-image flowers
# Determine the null distribution for the test statistic, the number of left-handed flowers out of 27.
# Obtain a very large number of samples of 27 individuals from a theoretical population in which L and R occur in the ratio 1:3, as specified by the null hypothesis.
#For each sample, the number of "L" is counted and saved in a vector named results27 (the samples themselves are not saved).

results27 <- vector(mode = "numeric")
for(i in 1:10000){
  tempSample <- sample(c("L", "R"), size = 27, prob = c(0.25, 0.75), 
                       replace = TRUE)
  results27[i] <- sum(tempSample == "L")
}

# Save the results also as a factor. This is to ensure that all categories between 0 and 27 are included in tables and graphs, even those that did not occur in the sampling process.

results27fac <- factor(results27, levels = 0:27)

# Tabulate the relative frequency distribution of outcomes. To get relative frequencies, use table to get frequencies and then divide by the total number of replicates. 
#Your results will be similar but not identical to the numbers in Table 6.2-1 because 10,000 iterations is not large enough for extreme accuracy. 

nullTable <- table(results27fac, dnn = "nRightFlowers")/length(results27fac)
data.frame(nullTable)

# Draw a histogram of the null distribution for the the number of left-handed flowers out of 27. Setting freq = FALSE causes hist to graph density of observations rather than frequency. 
#In this case, height of bars indicates relative frequency if bars are one unit wide (as in the present case).

hist(results27, right = FALSE, freq = FALSE,
     xlab = "Number of left-handed flowers")

# Commands for a fancier histogram are included here.

barplot(height = nullTable, space = 0, las = 1, cex.names = 0.8, col = "white",
        xlab = "Number of left-handed flowers", ylab = "Relative frequency")

# Calculate the fraction of results in the null distribution having 6 or fewer left-handed flowers.

frac6orLess <- sum(results27 <= 6)/length(results27)
frac6orLess

# Calculate the P-value. This won't be exact because 10,000 times is not large enough for extreme accuracy

2 * frac6orLess

#pvalue doesnt reach statistical signifcant. null value could just not be true or may
#have too small a sample size.
