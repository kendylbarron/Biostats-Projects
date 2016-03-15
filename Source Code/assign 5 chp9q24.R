#Kendyl Barron
#Biostats F15
#Assignment 5
#Chapter9Question24

#part b. create matrix for table of data.
DataMatrix<-matrix(c(32,20,18,30),ncol=2, byrow=T)
DataMatrix

#label columns of matrix.
colnames(DataMatrix)<- c("France (cool)", "India (warm)")
rownames(DataMatrix)<- c("Sterile", "Fertile")
DataMatrix

#plot data into mosaic, add color, title, axis labels.
mosaicplot(DataMatrix, col=c("blue", "red"), 
           main = c("Fertility Among Male Drosophilia vs Climate Source"),
           ylab= c("Source Location"),
           xlab= c("Fertility"))

#conduct appropriate hypothesis test. h0=categories are independent
test_results<-chisq.test(DataMatrix)
test_results

#calculate expected values
E <- test_results$expected
E
#calculated observed values
O <- test_results$observed
O

#calculate the test statistic comparing observed to expected frequencies
(O - E)^2/E
sum((O - E)^2/E)
#test statistic will be compared to critical value for df=1 at alpha=0.05
#if test statistic is larger h0 will be rejected.
