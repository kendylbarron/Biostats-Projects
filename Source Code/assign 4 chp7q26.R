#Kendyl Barron
#Biostats F15
#Assignment 4
#Chapter7Question26

#dbinom gives values of the binomal distribution for event x out of sample
#size n given propability p such that (x,n,p). 16:18 specifies probabilities
#for 16, 17, and 18, while the sum fucntion adds these values
#this test is assumed to be two sided so the value is then multiplied by 2
#this value is the assumed p-value given the null (p=0.5) is true.

dbinom<-sum(dbinom(16:18,18,0.5))
dbinom
2*dbinom

