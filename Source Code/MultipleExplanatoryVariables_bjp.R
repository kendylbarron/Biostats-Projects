#Author:  Mary Killilea
#Last Updated:   December 2, 2015
#Filename:  MultipleExplanatoryVariables


#Example 18.2
#Read data into R

	ZooplanktonData<-read.csv("ABD_all_data/chapter18/chap18e2ZooplanktonDepredation.csv")
	ZooplanktonData
	
  head(ZooplanktonData)
	attach(ZooplanktonData)

#Run a two-way anova with a block design

	anova(lm(diversity~block+treatment))  #this won't work if you don't make block a factor

	#not interger, cont num variable, but back block a factor
  class(block)
  
  block.f<-as.factor(block)
  
  class(block.f)
  
	#anova with factor. df 2 for treatment (if read in as cont num df 1 so know wrong)
  #treatment really does somehow affect the diversity
  #planned comparison would be to do a summary lm
	anova(lm(diversity~block.f+treatment))

	anova(lm(diversity~treatment+block.f))  #results will not change if you change the order


	#Example 18.3
	#Read data into R
  #Run the a two-way ANOVA with interaction on the same data using the following formula

	AlgeaData<-read.csv("ABD_all_data/chapter18/chap18e3IntertidalAlgae.csv")
	AlgeaData
	head(AlgeaData)
	attach(AlgeaData)
str(AlgeaData)
  #height levels low high mid. herbiro minus or plus
  #code to create the interaction plot

  interaction.plot(herbivores,height,sqrtArea)
  #variable height vs minus or plus. variables are interacting.
  #low tide makes a big differenece between allowing herbs or not
  #mid tide level smaller affect. notice affect difference. lines not parallel.
  
  
  #In this case order does not matter.  
  #There is some rounding in Sum of squares
  
	anova(lm(sqrtArea~height*herbivores))
	anova(lm(sqrtArea~herbivores*height))
  
	#Example 18.4
	#Read data into R 
  
	MoleRat<-read.csv("ABD_all_data/chapter18/chap18e4MoleRatLayabouts.csv")
	head(MoleRat)
	
  #Assign variables
  lnmass<-MoleRat$lnMass
	lnenergy<-MoleRat$lnEnergy
	caste<-MoleRat$caste
	
	

	#Create data separate data frames for each caste *this is used for ploting abline  
  infrequent<-MoleRat[caste=="lazy",]
  infrequent
  
  frequent<-MoleRat[caste=="worker",]
  
  #These are the linear models used to create regression lines for the data (figure 18.4-1 right side)
 
  lm.infrequent<-lm(lnEnergy~lnMass,data=infrequent) #, subset=caste=="lazy")
  summary(lm.infrequent)
  
  lm.frequent<-lm(lnEnergy~lnMass,data=frequent)
  summary(lm.frequent)
  
  #create a scatter-plot of the data with different symbols for the two castes 
  #then add the regression lines based on the linear models above
  
	plot(lnmass,lnenergy,pch=as.numeric(caste))
	
	abline(lm.infrequent)
  abline(lm.frequent)
  #plotting mass vs energy. mass really does effect energy level so important
  #as a covariate.
	

#Run and ANOVA of the full model with the interaction.
#The order will change the sum of squares of the main effect and the covariate 
# it will not change the sum of squares residual or interaction
  #interactoin term not significant at all in this example
	anova(lm(lnenergy~caste*lnmass))
	anova(lm(lnenergy~lnmass*caste))
	#the final value here does not change the p value but other values do
	#why is that?
  
# Since the interaction is not significant you can test the effect of caste with
# the mass.  Order will change sum of squares but not the model parameters (slope or intercept)
  
	anova(lm(lnenergy~lnmass + caste))
  anova (lm(lnenergy~caste + lnmass))
  
# The covariate lnmass should go first because you want to test caste with mass
  #already in the model not the other way around.

  