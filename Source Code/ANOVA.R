#Author:  Mary Killilea
#Last Updated:   November 2015
#Filename:  ANOVA

#Make sure you set your working directory 
#so that R can find files and saves output to the correct location.
#anove measuring diff of means just like a t-test
    
# Example 15.1 The knees who say night
#Assign the file a dataframe name and the data variable names

    	Circadian<-read.csv("ABD_all_data/chapter15/chap15e1KneesWhoSayNight.csv")

	    head(Circadian)

	    Treatment<-Circadian$treatment
      Treatment
      
	    Shift<-Circadian$shift
      Shift


#  Calculate ANOVA where treatment is the categorical explanatory variable 
#  and shift is the numerical response
    
    	
      aov(Shift~Treatment)

# Adding the "anova" command will take the output and create an anova table      
      anova(aov(Shift~Treatment))
#residuals between means to each individual sample. and differences of the means themselves.
#bon feroni correction? also home correction? 
    	
    	 
# Planned comparison
# interested 
    	CircadianModel<-aov(Shift~Treatment)
    	summary.lm(CircadianModel)
      
      help(summary.lm)
      
    	
# treatmenteyes show mean shift under estimate. how much level treatment eyes is shifted the overall mean difference
# compared with the overall lowest level of the factor (control) (have 3 levels). below mean shift knee treatment.
# big shift for the eyes and tiny shift of the knees. what is the signifanct of each shift? Pr significance. 
# one or two very small number? debate about corretion for those numbers.
# Calculating means raw data - Planned comparison the long way

	help(subset)
              
      knee<-(subset(Circadian,treatment=="knee"))
      knee
    	mean_knee<-mean(knee$shift)
      mean_knee
      length_knee<-length(knee$shift)
      
      mean_control=mean(subset(Circadian$shift,Circadian$treatment=="control"))
      mean_control
    	length_control=length(subset(Circadian$shift,Circadian$treatment=="control"))
    	length_control
      

      mean_diff = mean_knee - mean_control
      mean_diff
# mean diff knee and control very similar to difference. to knee estimate in anova. same but calculated
# directly rather than w anova function
      
# p values more powerful than if broke data up in three subsets. so use this.
# Mean square error from ANOVA table
      MSE = 0.4955
      

#SE calculation for Planned comparison

      SE=sqrt(MSE/length_knee + MSE/length_control)
      SE

# Calculating a t for planned comparison

      t=mean_diff/SE
      t

# Calculate the p-value
      
      2*pt(t,19)
      
      help (pt)

# two level anova mathematically equilavent to t test. anova uses f statistic?
      
      
#How you would conduct a Tukey-Kramer unplanned comparison.  
#Compare this output to the planned comparison
# unplanned all pair wise comparisons. built linear model aov and shif treatment.
      TukeyHSD(aov(Shift~Treatment))
# tell us the p adjusted. corrected for multi testing look through for significant.
# eye control signifcant. knee to eyes significant (knees nothing). unplanned comp. issue multi testing. 
    
# Random effects ANOVA example

  	Femurs<-read.csv("ABD_all_data/chapter15/chap15e6WalkingStickFemurs.csv")

  	head(Femurs)

   	individual<-(Femurs$specimen)
	  individual
# technical replicate. repeat same measurement on same individual. not bio variabiltiy. but estimate
# technical variance in measuring ability. measure length femur on insect. est varaince.random effect factor.
# random effects level taht come out of a population. vs fied effects. levels 1 2 3 of specimens represent
# insects and if replciated would not use those exact levels. but if replciate eye knee exp would use those levels.
	  
#because the individuals are numbered R needs to be told that 
#each individual walking stick is a category 
# you can't do anova if the explanatory variable is a number       

      ind<-as.character(individual)
      ind<-as.factor(indvidiual)
      str(ind)

	    length<-Femurs$femurLength
	    length

 	    anova(lm(length~ind))
# not interest in p value particular just want variance estimates. p 481 in book. est of within group variance.
# specimen set and an integer. anova want it to be a factor. problem in R. read in data. integer is discrete
# check data types. convert specimen to a factor in this case string or character. dont want a regression.

