#chp12q31

getwd()
#get data
RatData<-read.csv("ABD_all_data/chapter12/chap12q31RatReciprocity.csv")
RatData

RatData$focalRat<-NULL
RatData

boxplot(RatData, main=c("Generalized Reciprocity Among Rattus norvegicus"), ylab=c("Tendency to Help (Pulls/Min)"),
        xlab=c("Treatment"), col=c("orange"))

HelpData<-RatData$AfterHelp
HelpData
mean(HelpData)

NoHelpData<-RatData$AfterNoHelp
NoHelpData
mean(NoHelpData)

mean(HelpData)-mean(NoHelpData)

#attach data so to refer to vectors within dataframe
attach(RatData)
#perform paired t-test given data.
t.test(AfterHelp,AfterNoHelp,paired=TRUE)
