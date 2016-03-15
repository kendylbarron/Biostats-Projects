#Kendyl Barron
#Biostats F15
#Assignment 3
#Chapter6Question27

# The test statistic of this experiement is the number of patients who have improved 
# adrostenone detection. Out of a much larger population, a sample of 12 patients, assume 
# the proportion of patients who increase accuracy and decrease in accuracy is 50:50 
# (null hypothesis). For each 12 patient sample, the number of patients who have improved
# in detection are saved in the vector results12.
results12 <- vector()
#intially an empty vector for which the results of the following loop will be stored.

for(i in 1:10000){
  tempSample <- sample(c("Less Accurate", "More Accurate"), size = 12, prob = c(0.5, 0.5), replace = TRUE)
  results12[i] <- sum(tempSample == "More Accurate")}
#loop takes 10,000 13-patient samples with 50:50 probability and counts how many 
#of the sample have improved accuracy and stores the value in the results vector.
results12
#groups the results into categories of patients improved accuracy values 0-12 out of the
#total sample (12)
factor12 <- factor(results12, levels = 0:12)

#calculate the frequency of each result. This gives the probability
#of each result given the null hypothesis (50:50 proportion) is true.
nullTable <- table(factor12, dnn = "nPatientsImproving")/length(factor12)
data.frame(nullTable)

#graph the null distribution: the probability of the number of patients
#improving out of a 12 person sample given the null hypothesis is true. 
#label x,y axis and title graph. add labels to top of bins. set x and y limits. 
hist(results12, right = FALSE,freq = FALSE,
     xlab = "Number of Patients Improving",
     ylab = "Probability",
     ylim = c(0,0.250),
     xlim = c(0,12),
     labels = T,
     main = c("Null Distribution of Sensitivity to Androstenone"))
