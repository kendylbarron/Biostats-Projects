##Mary Killilea
## Introduction to R
# Sept 2015

# please add comments to the lines below to help you understand
# the code during recitations

# a comment line always starts with a "#" character (in R)
# this tells R to ignore everything on the line
# this lets you leave notes and clues about what things do

# ALWAYS COMMENT!!!

###############
# where is your data?  where do you want to save things?

# Set your working directory using "setwd" command so that R can find files and
#    save output to the correct location (remember to use forward slashes!)
#
# 

setwd( "C:/Users/Mary Killilea/Documents/BiostasticsFall2015/R Code" ) 

# In R Studio you can set the working directory without the "setwd" command
#  just select "Session" on the tool bar and select "Set Working Directory"
#  If you select "Choose Directory" you can find the folder where you want to 
#  read and write files
 
# check that your working directory is correctly set

getwd()

# why do we do this?


##############
# loading data

# read data from a file into R
# will this work for all files?

read.csv( "03e2SpiderRunningAmputation.csv" )  

# note that you don't need directory information about where the spider data is 
# located on the computer because it is in the working directory

# comma-separated values is a file type saved in excel that stores data
#    in plain-text form
# information is separated by commas or tabs

# what if I want to load in a "table" or data in another format?
# how do I search for these options?

help( read.table )

# are there other ways to get help?

# you may want to read in the data in different types of file formats
# here we can read in the csv as a table
# note that we are changing some of the defaults (commas separate the input)

read.table( "03e2SpiderRunningAmputation.csv" , header = T , sep = "," )


#################
# using data in R

# assign data to a data frame

# what is a "data frame"?
# what does this mean for R?

SpiderData <- read.csv( "03e2SpiderRunningAmputation.csv" )

# now we can refer to this big dataset as only SpiderData
# a line with just the variable name will print a representation of the data
#    (alternatively "print()")
SpiderData

#R is cap sensitive
spiderdata

#In the following sections of code you will learn:

# how do I extract data from tables?
# how can I learn what data is in a table?
# how do I create variables (called vectors in R)?
# how do I assign values to vectors?

head(SpiderData)

Before <- SpiderData$speed.before

Before

After <- SpiderData$speed.after

After

# what is a "function"?
# R can work as a calculator but it is also a vector calculator
# remember case sensitive

Before - After
Difference <- Before - After

Difference

# how do I manually enter data into R?
# this should NOT be done for large datasets!!!

RandomData <- c( 1.25 , 2.94 , 2.38 , 3.09 , 4.22 , 5.62 , 6 , 7 , 8 , 9 , 10 , 11 , 12 , 13 , 14 , 15 )
RandomData

# how do I enter categorical data?
Labels <- c( "low" , "med" , "high" )
Labels

# if you don't know what command you want to use you can use ??
# in this example I want to generate random numbers but I don't know the command
# if I use ?random I get an error because it isn't a R function but 
# ??random does a search for everywhere random occurs in the R help document


?random
??random

?sample

#this creates a vector of numbers from 1 to 10
x<-1:10
x
#sample will randomly select the 10 numbers (this is like picking 10 numbers our of hat)

sample(x)

sample(x,20)

# if you want the numbers put back in the hat then you do this:

sample(x, replace=T)

sample(x,100,replace=T)

