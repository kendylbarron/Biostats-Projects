#Kendyl Barron
#Biostats F15
#Assignment 1
#Chapter2Question32

#create a datamatrix so data can be organized into a barplot
DataMatrix <-matrix(c(21,38,38,211), ncol=2, byrow=T)
DataMatrix

#label rows and columns of the matrix
colnames(DataMatrix) <- c( "Infected" , "Uninfected" )
rownames(DataMatrix) <- c( "Drivers with accidents" , "Control" )
DataMatrix

#plot data with bars besides one another, add color, label axis,
#adjust range of y axis, add title and legend
barplot(DataMatrix, beside = TRUE,
        col=c("red", "blue"),
        xlab = "Toxoplasma gondii Exposure",
        ylab = "Number of Drivers",
        ylim = c(0,250),
        main = c("Car Accidents Among Drivers with Toxoplasma gondii"))
legend("top",                                
       legend=c("Drivers w/ Accidents", "Drivers w/o Accidents"),
       fill=c("red","blue")) 

#add actual values to plotted data
text(1.5, 35, "21")
text(2.5, 50, "38")
text(4.5, 50, "38")
text(5.5, 225, "211")


