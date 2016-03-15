## Displaying Data
#  Kendyl Barron
#  Updated Fall 2015


######## Recitation Setup###############

# 1. go to NYU Classes and download the ABD_all_data.zip file
#
# 2. unzip the data into a working directory or folder (can be a usb directory)
#        -
# 3. Set your working directory in R studio.  
      #Select "Session" from the toolbar
      # "Set Working Directory" -> "Choose Directory"
      # Select the directory where you have downloaded the data

# 4. Check your working directory with the following command:

getwd()


# Now follow along as we go through this scriptline by line
# Ctrl Enter will run any individual line of code 
# You can also highlight a section of code and type Ctrl Enter
# Don't just trust that the script we provided works, check it yourself!
# This is now your code so feel free to add notes.  Just remember to use"#"
# Ask lots of questions


###########Figure 2.1-2 of Serotonin levels in 3 treatments - strip charts #############

# Read the data into R.  
# The following code assumes the ABD_all_data is a folder in your working directory
LocustData <- read.csv("ABD_all_data/chapter02/chap02f1_2locustSerotonin.csv")


# If you want to see the data you can just type the data frame name
LocustData

# What are the vectors in LocustData?


# Create a stripchart of the LocustData. 

stripchart(serotoninLevel ~ treatmentTime, data = LocustData, method = "jitter", 
           vertical = TRUE)


# Use the help command below to learn:
  #1.Why you are plotting "serotoninLevel ~ treatmentTime"
  #2. What is he purpose of method = "jitter"
  #3. Why does vertical = TRUE


help(stripchart)

# Create a strip chart closer to that shown in Figure 2.1-2, by including more options.
# We will not go through this line by line but you can play with it later
help(par)

par(bty = "n") # plot x and y axes only, not a complete box

stripchart(serotoninLevel ~ treatmentTime, data = LocustData, vertical = TRUE, 
           method = "jitter", pch = 16, col = "firebrick", cex = 1.5, las = 1,
           ylab = "Serotonin (pmoles)", xlab = "Treatment time (hours)",
           ylim = c(0, max(LocustData$serotoninLevel)))

# The following command calculates the means in each treatment group (time)
meanSerotonin = tapply(LocustData$serotoninLevel, LocustData$treatmentTime, mean)

# "segments" draws draws lines to indicate the means
segments(x0 = c(1,2,3) - 0.1, y0 = meanSerotonin, x1 = c(1,2,3) + 0.1, 
         y1 = meanSerotonin, lwd = 2)



############# Example 2.2A.Deaths from Tigers - frequency table and barplot ########

# Read the data into data frame named tigerData
tigerData <- read.csv("ABD_all_data/chapter02/chap02e2aDeathsFromTigers.csv")
head(tigerData)


# Generate a frequency table. The sort function is included to sort the categories by their frequencies.
tigerTable <- sort(table(tigerData$activity), decreasing = TRUE)
tigerTable

# You can arrange the frequency table vertically.
data.frame(Frequency = tigerTable)

# Use the addmargins command to include sums in your frequency table.
data.frame(Frequency = addmargins(tigerTable))

# Draw a bar graph. 
#The additional arguments cex.names = 0.5 shrinks the axis labels by 50%, 
#and las = 2 flips the labels, so that they all fit in the window.
barplot(tigerTable, ylab = "Frequency", cex.names = 0.5, las = 2)

# A slightly fancier bar graph of the data with more options, like that shown in Figure 2.2-1, is shown here.

oldpar = par(no.readonly = TRUE) # stores a backup copy of current graph settings in "oldpar"
par(mar = c(8, 4, 4, 2) + 0.1)   # creates more room for labels below the x-axis
barplot(tigerTable, las = 2, col = "firebrick", cex.names = 0.8, ylim = c(0,50), 
        xlab = "", ylab = "Frequency (number of people)")
mtext("Activity", side = 1, line = 7, cex = 0.9) # adds the text under the x-axis
par(oldpar)  # reverts graph settings back to default






############ Example 2.2B Abundance of bird species - histograms#########################

# chapter 2 example 2.1B
# read in Bird Data
BirdData <- read.csv( "ABD_all_data/chapter02/chap02e2bDesertBirdAbundance.csv" )

# look at the data fram
BirdData

# we often really only need to see the first few lines of data 
# with the vector names. The can be done with the head command
head(BirdData)

# what type of data do we have here?
# how would we want to display this?

# create a histogram of bird data
# how do we make a histogram?
??histogram

# try researching "graphics" and then go deeper
?graphics
library( help = "graphics" )

# what are our options for a histogram?
?hist
help( hist ) 

hist(BirdData$abundance)

hist( BirdData$abundance, breaks = 10 , main = "Histogram of Bird Abundance" , 
      xlab = "Abundance" , srt = 0 , srt = 45 , col = "purple" , 
      xlim = c( 0 , 650 ) , ylim = c( 0 , 25 ) )

# what do each of these options do?
# can you explain each argument and choice?


# what colors can we use?
colors()

# what is the histogram telling us?

# let's adjust some of the parameters - title, breaks, y axis




########## Example 2.3 reproductive effort and malaria############### 
##########contigency table and mosaic plot#####

# what is a "matrix"?

# how do we make one in R?
?matrix
data <- matrix( c( 7 , 15 , 28 , 15 ) , ncol = 2 , byrow = T )
# what does "byrow" do?

data

# add some names
colnames( data ) <- c( "control" , "egg removal" )
rownames( data ) <- c( "malaria" , "no malaria" )
data


#Create a grouped bar graph
barplot(data, beside = TRUE)

help(barplot)

barplot(data, beside =T, space = c(.2,1) , legend = rownames( data ) , 
        ylab = "frequency", xlab = "" , col = c( "red" , "gold" ),
        main = "barplot" , args.legend = list( x = "bottom" , inset = -.5 ) )



#
# Create a mosaic plot

mosaicplot(data)

# What is wrong with this plot?

# mosaicplot and barplot read in data differently
# we need to transpose the matrix (switch rows and columns) for the mosaic 
# how do we calculate the transpose?

??transpose

transposed.data<-t(data)
transposed.data


mosaicplot(transposed.data, main = "Mosaic Plot" , color = TRUE )

mosaicplot(transposed.data, main = "Mosaic Plot" , col = c( "red" , "gold" ), off=0 )



#########Example 2.3B Guppy attractivness - scatter plot#########

# What type of data do we display with a scatter plot?

# Read in the data

GuppyData <- read.csv("ABD_all_data/chapter02/chap02e3bGuppyFatherSonAttractiveness.csv")

head(GuppyData)

#Create individual vectors from the data frame
Father <- GuppyData$fatherOrnamentation
Son <- GuppyData$sonAttractiveness
Father
Son
   
help( plot )

plot(Father,Son)

plot( Father , Son , xlab = "Father's ornamentation" , 
      ylab = "Son's attractiveness" , pch = 16 , col = "purple" )


############Challenge###########
#Create a line plot of the chap02e4aMeaslesOutbreaks data





