#  problem
#  """several cars have rolled into an independent audit unit for crash test
# they are being evaluated on a defined scale{poor(-10) to excellent(10)} on:"""
# 1.manikin head impact
# 2.manikin body impact
# 3.inerior impact 
# 4.HVAC impact
# 5.safety alarm()
# each crash test is very expensive
# the crash test was performed for only 100 cars
# type of car- Hatchback/SUV,was noted
# however with this data in future they should be able to predict the type of cars
# part of data reserved for analysis
library(caret)# for confusionMatrix
#importing files #reading data
crashTest_1<-read.csv("C:/Users/abppa/OneDrive/Desktop/portfolio/programming/r_programming/sample data files/crashTest_1.csv",row.names = 1)

crashTest_1_t<-read.csv("C:/Users/abppa/OneDrive/Desktop/portfolio/programming/r_programming/sample data files/crashTest_1_test.csv",row.name=1)
#viewing data
View(crashTest_1)
View(crashTest_1_t)
#details about structure of data frame
str(crashTest_1)
crashTest_1$CarType<-as.factor(crashTest_1$CarType)
str(crashTest_1_t)
#statiscal deatils
summary(crashTest_1)
summary(crashTest_1_t)
#glm generalised linear model
logisfit<-glm(formula=crashTest_1$CarType ~ .,family = "binomial",data = crashTest_1)
logisfit
summary(logisfit)

#predicting out
logistrain<-predict(logisfit,type='response')
plot(logistrain)
tapply(logistrain, crashTest_1$CarType, mean)

logispred<-predict(logisfit,newdata = crashTest_1_t,type='response')
plot(logispred)
crashTest_1_t[logispred<=0.5,"logispred"]<-"Hatchback"
crashTest_1_t[logispred>0.5,"logispred"]<-"SUV"
confusionMatrix(table(crashTest_1_t[,7],crashTest_1_t[,6]),positive='Hatchback')
