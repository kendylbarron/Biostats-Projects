# Contingency Analysis 
# Mary Killilea
# Fall 2015


getwd()
########### Example 9.2 Aspirin and Cancer
# odds ratios

#read the cancer data into R
cancer <- read.csv("ABD_all_data/chapter09/chap09e2AspirinCancer.csv")
head(cancer)

# create a table from the data
cancerTable <- table(cancer$cancer, cancer$aspirinTreatment)
cancerTable

# create a mosaic plot
mosaicplot( t(cancerTable), col = c("firebrick", "goldenrod1"), cex.axis = 1, 
            sub = "Aspirin treatment", ylab = "Relative frequency", main = "")
??mosaicplot
# Odds ratios with the 95% confidence interval can be calculated
# using a library called epitools
install.packages("epitools", dependencies = TRUE)
library(epitools)

# The odds ration command only works if the library is installed
oddsratio(cancerTable, method = "wald")

# $measure[-1] gets rid of extra information. remove row 1.
oddsratio(cancerTable, method = "wald")$measure[-1,]

# how could you calculate the odds ratio without the above library? from contingency
# table??

# Calculate relative risk using the epitools package. 
# The layout expected by the riskratio function is the complete opposite of
# the layout used in the book. 
# To use the command with a contingency table in book style (such as cancerTable), 
# we need to flip (transpose) the table and reverse the order of the rows. 
# We can do this all at once with the following arguments to the riskratio function.
riskratio(t(cancerTable), rev = "both", method = "wald")

riskratio(t(cancerTable), method = "wald", rev = "both")$measure[-1,]

# Notice that the result differs slightly from the relative risk value 
# given in the book for these same data (1.007) because rounding error 
# here is reduced.

########### Example 9.4  
# 
# contingency tests - when both variables are categorical,
# used to test whether one variable is "contingent" on the other
# chi square contingency test is most commonly used test of assocation
# between 2 categorical variables

# tests "Goodness of Fit" (GOF) of the data to the null model
# (null model is: variables are independent)

# Chapter 9 Example 3
worm <- read.csv( "ABD_all_data/chapter09/chap09e3ParasiteBrainWarp.csv" )
head(worm)

# what is wrong with this data? didnt' have column title of fate. mislabeled.

# We have made a correction to the data set.  

worm <- read.csv( "ABD_all_data/chapter09/chap09e3ParasiteBrainWarp_corrected.csv" )
head(worm)

#Set the preferred order of infection categories in tables and graphs.

worm$infection <- factor(worm$infection, levels = c("uninfected", "light", "high"))

  
# Create a table
wormTable <- table(worm$fate, worm$infection)
wormTable
addmargins(wormTable)


# transpose data and use mosaic plot
mosaicplot( t(wormTable), col = c("firebrick", "goldenrod1"), cex.axis = 1, 
            sub = "Infection status", ylab = "Relative frequency")
??t()
# want to test whether the probability of being eaten by birds differs,
# according to infection status
# are categorical variables "infection level" and "being eaten" independent?
# ?

# H_0 = the data are independent!
# have to calculate the expected frequencies for each cell under the null model

test_results = chisq.test( worm$fate, worm$infection,correct = FALSE )
test_results

test_results2 = chisq.test( wormTable )
test_results2


E <- test_results$expected
E
O <- test_results$observed
O
# compare these to table 9.3-2

# what is this calculation?
# ?
(O - E)^2/E
sum((O - E)^2/E)


# chi squared contingency test is a special case of the
# chi squared Goodness of Fit test
# here the probability model is that the variables are independent

# if the table is 2x2, Fishers exact test should be used. like GOF test? 2 cases 
# do binomal because exact. exact test for fischer test. 2x2 or 3x3. counts not 
# too large. is exact.
# for in cases where expected cell frequencies are too low to meet rules
fisher.test( wormTable )

#Try using fisher test on the following data
vampire <- read.csv("ABD_all_data/chapter09/chap09e5VampireBites.csv")
vampire
vampTable<-table(vampire$estrous, vampire$bitten)
vampTable
fisher.test(vampTable)
