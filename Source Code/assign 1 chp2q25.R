#Kendyl Barron
#Biostats F15
#Assignment 1
#Chapter2Question35

#set working directory
getwd()
#read data into R and check table display
FoodReductionLifespan <-read.csv("ABD_all_data/chapter02/chap02q35FoodReductionLifespan.csv", sep=",")
FoodReductionLifespan

#seperate sex and foodTreatment vectors and combine with paste to great 4 groups
sex <- FoodReductionLifespan$sex
foodTreatment <- FoodReductionLifespan$foodTreatment
both <-paste(sex, foodTreatment)

#create stripchart with lifespan data points of each of 4 groups represented
stripchart(lifespan ~ both, FoodReductionLifespan, method = "jitter", 
           ylab = c("lifespan in years"),
           vertical = TRUE,
           main = c("Effect of Reduced Diet on Lifespan of Rhesus Monkeys"),
           col=c("red","blue"))

