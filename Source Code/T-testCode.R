# Comparing two means
# Mary Killilea
# Fall 2015



##Example 12.2 So macho it makes you sick
##Two-sample paired t-test (don't forget paired=TRUE)

# Read the bird data into R

    bird.data<-read.csv("ABD_all_data/chapter12/chap12e2BlackbirdTestosterone.csv")
    head(bird.data)
    
# Notice there is a beforeImplant and logBeforeImplant.  This is because
# the implant data is not normal so we use a log transformation
# we will cover this next week so you don't need to worry about it now
# but you should use the log data. ((ratio on logarithic scale become additions.))
# take log of data now normally distributed
    

# Attaching the data allows you to refer to the variables in the data fram    
    attach(bird.data)
    help(attach)
    

#If you are interested.
#You can see the effect of the log transformation in the histograms below.
    
dif=(beforeImplant-afterImplant)
hist(dif) 
    
logdif=(logBeforeImplant-logAfterImplant)
hist(logdif)    

#The textbook just pretends to use the original data, 
#so this analysis will look the same.
   
      
t.test(logBeforeImplant,logAfterImplant,paired=TRUE)


## Example 12.3 Spike or be spiked
# Two-sample t-test

# Read the data
lizard.data<-read.csv("ABD_all_data/chapter12/chap12e3HornedLizards.csv")
head(lizard.data)

# Attach the data frame

attach(lizard.data)
    
# Calculate the t-test by separating the data by "Survival"

t.test(squamosalHornLength ~ Survival, data = lizard.data, var.equal = TRUE)



      

