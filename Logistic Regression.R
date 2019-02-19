#####Logistic Regression Graded assignments #####

library(gains)
library(dplyr)
library(irr)
library(caret)
library(usmap)
install.packages("usmap")
setwd("D:\\Jigsaw\\3.R")
getwd()

goodforu<-read.csv("goodforu-class12.csv")



library(dataQualityR)
install.packages("dataQualityR")

goodforu(crx)


brandA<-goodforu%>%select(X1,X2,X8,X9,X15,X16,X22,X23,X29,X30,X38,X39,X40,X41,X42,X43,X44,X45,X46,X47,X48,X49,X50,X51,X52,X53,X54,X55,X56,X57,X58,X59,X60,X61,X62)

brandA$target<-ifelse(brandA$X23>5,1,0)

#let's check the data for any anamoly and fix it if any
colSums(is.na(brandA))
str(brandA)
#we dont need X23 now for analysis ,so we can remove it 
brandA<-brandA%>%select(-X23)


#let's sample the data

set.seed(200)
str(brandA)
index<-sample(nrow(brandA),nrow(brandA)*0.70,replace = F)
#let's create train and validation data sets
train<-brandA[index,]
test<-brandA[-index,]
#lets's check the dataset
dim(train);dim(test)
#let's build the model taking target as the dependent variable and all others as independent variable
model<-glm(target~.,data = train,family = "binomial")
summary(model)

step(model,direction = 'both')

model2<-glm(formula = target ~ X1 + X2 + X8 + X9 + X15 + X16 + X22 + 
              X29 + X30 + X38 + X40 + X43 + X47 + X51 + 
              X57, family = "binomial", data = train)

step(model2,direction = 'both')
 
summary(model2)


#validation of the model

pred<-predict(model2,type = "response",newdata = test)

#let's check the rate of 1,according to that we will set a cutoff value
table(train$target)/nrow(train)
#so we can assume anything with probability of 1 greater than 0.2527401 will be 1 else 0,
#where 1 is good and 0 is bad
pred<-ifelse(pred>0.5,1,0)
#now run kappa matrix 
kappa2(data.frame(test$target,pred))


#confusion matrix
test$result<-ifelse(pred>0.5,1,0)

as.factor(test$target)->test$target
as.factor(test$result)->test$result

confusionMatrix(test$result,test$target,positive = "1")

#Accuracy is coming to be 84.28% and my kappa is coming to be 0.544

#let's check wiht the confidence interval 
confint(model)

#seems fine to me it's not that wide
View(test)

#now Preaparing the Gain_Table

tab<-data.frame(test$target,Probability=predict(model,type="response",newdata = test))
View(tab)



tab<-tab[order(-tab$Probability),]
View(tab)
tab$pred<-ifelse(tab$Probability>0.5,1,0)
View(tab)
tab<-tab%>%mutate(quant=ntile(tab$Probability,10))
#tab%>%select(-quantile)->tab
View(tab)

tab%>%filter(test.target==pred)%>%group_by(quant)%>%summarise(response=n())->response

tab%>%group_by(quant)%>%summarise(numebr_of_observatio=n())->number_of_cases

gain_table<-cbind(number_of_cases,response)
View(gain_table)

#remove duplicate column
gain_table <- gain_table[, !duplicated(colnames(gain_table))]
View(gain_table)

gain_table%>%mutate(cumlateive_response=cumsum(response))->gain_table

gain_table%>%mutate(Pecentage_of_events=(response/sum(response))*100,gain=cumsum(Pecentage_of_events))->gain_table
View(gain_table)

gain_table%>%mutate(cumulative_lift=gain/(quant*10))->gain_table

#gain_chart

library(ggplot2)

View(gain_table)
a<-ggplot(gain_table,aes(x=quant*10))+geom_line(aes(y=gain,linetype="% of cumulative events(model)",colour="% of cumulative events(model)"))+geom_line(aes(y=quant*10,linetype="% of cumulative events(random)",colour="% of cumulative events(random)"))
b<-a+scale_x_continuous(breaks=seq(0,100,10),expand = c(0,0),limits = c(0,100))+scale_y_continuous(breaks=seq(0,100,10),expand = c(0,0),limits = c(0,100))
c<-b+xlab("%of data sets")+ylab("% of events")+guides(linetype=F)+geom_point(aes(y=gain),colour="red")+geom_point(aes(y=quant*10),color="blue")
c+ggtitle("Gains Chart")

#lift Chart
p<-ggplot(gain_table,aes(x=quant*10))+geom_line(aes(y=cumulative_lift,linetype="Lift(Model)", colour="Lift(Model)"))+geom_line(aes(y=rep(1,10),linetype="lift(random)", colour="lift(random)"))
q<-p+scale_x_continuous(breaks=seq(0,100,10),expand = c(0,0),limits = c(0,100))+scale_y_continuous(breaks = seq(0,1.5,0.3),expand = c(0,0),limits = c(0,1.5))
r<-q+xlab("%of data sets")+ylab("lift")+theme_bw()+geom_point(aes(y=cumulative_lift),colour="red")+geom_point(aes(y=rep(1,10)),colour="green")
s<-r+scale_colour_manual(name="linetype", values = c("Lift(Model)" = "red", "lift(random)" = "green"))+guides(linetype=F)
s+ggtitle("Lift Chart")


#Equation For the model is log(p/1-p)=-1.43972 +( -0.38741)*X2+(-0.34811)*X9 +(-0.43805 )*X16+ 0.42569*X30
#From the model we can infer that except from the processing level all other have -ve impact 
# farm grown ingredients has a -ve impact of 0.38741 on the Brand value
# Have zero grams trans fat has -ve impact of 0.34811 on the brand value
# Are made with natural oils has -ve impact of 0.43805 on the brand value
#Processing level has a positve impact of 0.42569 on the brand value