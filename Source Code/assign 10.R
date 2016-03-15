
#read in data
OpsinData<-read.csv("ABD_all_data/chapter18/chap18q18OpsinExpression.csv")
OpsinData

#seperate vectors
OpsinData$population
OpsinData$relativeExpressionOfSWS1
OpsinData$waterClarity

#plotxfactor vs trace factor vs response
#how to plot actual datapoints like a strip chart over interaction lines?
interaction.plot(population,waterClarity, relativeExpressionOfSWS1, 
                 main=c("Interaction Plot"))


