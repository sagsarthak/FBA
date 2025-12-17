#14/11/2025
library(readxl)
college_new <- read_excel("C:/Users/Salil Modak/Desktop/Term 2/Fundamental of Business Analytics/predictive.xlsx", sheet = "College")
options(scipen=999)
college_new$City <- ifelse(college_new$Location == "City", 1, 0)

Model <- lm(Earnings ~ Cost + Grad + Debt + City, data = college_new)
summary(Model)
predict(Model, data.frame(Cost=25000, Grad=60, Debt = 80, City=1))

predict(Model, data.frame(Cost=25000, Grad=60, Debt = 80, City=1), level=0.95, interval="confidence")



predict(Model, data.frame(Cost=25000, Grad=60, Debt = 80, City=1), level=0.95, interval="prediction")



library(readxl)
retail_new <- read_excel("C:/Users/Salil Modak/Desktop/Term 2/Fundamental of Business Analytics/predictive.xlsx", sheet = "Retail")
options(scipen=999)
retail_new$d1 <- ifelse(retail_new$Quarter == 1,1,0)
retail_new$d2 <- ifelse(retail_new$Quarter == 2,1,0)
retail_new$d3 <- ifelse(retail_new$Quarter == 3,1,0)
Model <- lm(Sales ~ GNP + d1 + d2 + d3, data = retail_new)
summary(model)

predict(Model, data.frame(GNP=25000, d1=0, d2=1, d3=0))





#27/11/2025
#Logistic Regression
library(readxl)
mortgage <- read_excel("C:/Users/Salil Modak/Desktop/Term 2/Fundamental of Business Analytics/LogisticRegression.xlsx", sheet = "Mortgage")

Linear_model <- lm(y~x1+x2, data = mortgage)
summary(Linear_model)
predict(Linear_model, data.frame(x1=c(5,60), x2=10))


Logistic_Model <- glm(y~x1+x2, family = binomial, data = mortgage)
summary(Logistic_Model)
predict(Logistic_Model, data.frame(x1=c(20,30), x2=30), type = "response")

pollution <- read_excel("C:/Users/Salil Modak/Desktop/Term 2/Fundamental of Business Analytics/LogisticRegression.xlsx", sheet = "Pollution")
linear_m <- lm(Illness~Minutes, data = pollution)
summary(linear_m)
predict(linear_m, data.frame(Minutes = 1280))

logistic_m <- glm(Illness~Minutes, family = binomial, data = pollution)
summary(logistic_m)
predict(logistic_m, data.frame(Minutes = 1280))


library(readxl)
spam <- read_excel("C:/Users/Salil Modak/Desktop/Term 2/Fundamental of Business Analytics/LogisticRegression.xlsx", sheet = "Spam")
linearmodel <- lm(Spam~Recipients+Hyperlinks+Characters, data = spam)
summary(linearmodel)
logisticalmodel <- glm(Spam~Recipients+Hyperlinks+Characters, family = binomial, data = spam)
summary(logisticalmodel)

pHatLin <- predict(linearmodel)
pHatLog <- predict(logisticalmodel, type='response')
yHatLin <- ifelse(pHatLin>=0.5,1,0)
yHatLog <- ifelse(pHatLog>=0.5,1,0)

100*mean(spam$Spam==yHatLin)
100*mean(spam$Spam==yHatLog)



myData <- read_excel("C:/Users/Salil Modak/Desktop/Term 2/Fundamental of Business Analytics/LogisticRegression.xlsx", sheet = "Spam")
TData <- myData[1:375,]
VData <- myData[376:500,]


Model1 <- glm(Spam~Recipients+Hyperlinks+Characters, family=binomial, data=TData)
summary(Model1)

pHat1 <-predict(Model1,VData, type='response')
yHat1 <- ifelse(pHat1>=0.5,1,0)
100*mean(VData$Spam==yHat1)

yTP1 <- ifelse(yHat1 == 1 & VData$Spam == 1, 1, 0)
yTN1 <- ifelse(yHat1 == 0 & VData$Spam == 0, 1, 0)
100*mean(VData$Spam == yHat1)
100*(sum(yTP1)/sum(VData$Spam==1))
100*(sum(yTN1)/sum(VData$Spam==0))



Model2 <- glm(Spam~Recipients+Hyperlinks, family=binomial, data=TData)
summary(Model2)

pHat2 <-predict(Model2,VData, type='response')
yHat2 <- ifelse(pHat2>=0.5,1,0)
100*mean(VData$Spam==yHat2)
yTP2 <- ifelse(yHat2 == 1 & VData$Spam == 1, 1, 0)
yTN2 <- ifelse(yHat2 == 0 & VData$Spam == 0, 1, 0)
100*mean(VData$Spam == yHat1)
100*(sum(yTP2)/sum(VData$Spam==1))
100*(sum(yTN2)/sum(VData$Spam==0))


# New Training and Validation Data
TData2 <- myData[c(1:250, 371:500), ]
VData2 <- myData[251:375,]
Model1 <- glm(Spam~Recipients+Hyperlinks+Characters, family=binomial, data=TData2)
pHat1 <-predict(Model1,VData2, type='response')
yHat1 <- ifelse(pHat1>=0.5,1,0)
yTP1 <- ifelse(yHat1==1 & VData2$Spam ==1, 1 , 0)
yTN1 <- ifelse(yHat1==0 & VData2$Spam ==0, 1 , 0)
100*mean(VData2$Spam==yHat1)

100*(sum(yTP1)/sum(VData2$Spam==1))
100*(sum(yTN1)/sum(VData2$Spam==0))

Model2 <- glm(Spam~Recipients+Hyperlinks, family=binomial, data=TData2)
pHat1 <-predict(Model2,VData2, type='response')
yHat1 <- ifelse(pHat1>=0.5,1,0)
yTP1 <- ifelse(yHat1==1 & VData2$Spam ==1, 1 , 0)
yTN1 <- ifelse(yHat1==0 & VData2$Spam ==0, 1 , 0)
100*mean(VData2$Spam==yHat1)

100*(sum(yTP1)/sum(VData2$Spam==1))
100*(sum(yTN1)/sum(VData2$Spam==0))



# New Training and Validation Data
TData4 <- myData[c(126:250, 251:500), ]
VData4 <- myData[1:125,]
Model1 <- glm(Spam~Recipients+Hyperlinks+Characters, family=binomial, data=TData4)
pHat1 <-predict(Model1,VData4, type='response')
yHat1 <- ifelse(pHat1>=0.5,1,0)
yTP1 <- ifelse(yHat1==1 & VData4$Spam ==1, 1 , 0)
yTN1 <- ifelse(yHat1==0 & VData4$Spam ==0, 1 , 0)
100*mean(VData4$Spam==yHat1)

100*(sum(yTP1)/sum(VData4$Spam==1))
100*(sum(yTN1)/sum(VData4$Spam==0))

Model2 <- glm(Spam~Recipients+Hyperlinks, family=binomial, data=TData4)
pHat1 <-predict(Model2,VData4, type='response')
yHat1 <- ifelse(pHat1>=0.5,1,0)
yTP1 <- ifelse(yHat1==1 & VData4$Spam ==1, 1 , 0)
yTN1 <- ifelse(yHat1==0 & VData4$Spam ==0, 1 , 0)
100*mean(VData4$Spam==yHat1)

100*(sum(yTP1)/sum(VData4$Spam==1))
100*(sum(yTN1)/sum(VData4$Spam==0))


# DEPRESSION DATA 
library(readxl)
mortgage <- read_excel("C:/Users/Salil Modak/Desktop/Term 2/Fundamental of Business Analytics/Depression.xlsx", sheet = "Depression")




# CLASSIFICATION AND REGRESSION TREES
library(readxl)
alcohol <- read_excel("C:/Users/Salil Modak/Desktop/Term 2/Fundamental of Business Analytics/beeranalytics classification tree_blank.xlsx", sheet = "Data")
View(alcohol)
plot(alcohol$Flavonoids~alcohol$Alcohol, main = "Scatter plot of rating", xlab = "Alcohol", ylab = "Flavonoids")
legend("Topright", legend=c("A Rating", "B Rating"), pch=16, col = c(20,26))

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

str(alcohol)
table(alcohol$Rating)
tree <- rpart(Rating ~. ,data = alcohol, method = "class") #Classification
tree

rpart.plot(tree)

#Classification Trees
#02/12/2025
library(readxl)
myData <- read_excel("C:/Users/Salil Modak/Desktop/Term 2/Fundamental of Business Analytics/RLL regression trees - blank.xlsx", sheet = "training data")
str(myData) # type of column variables
table(myData$Demand)

tree <- rpart(Demand ~ Price + Relative_Price_of_Competing_Styles, data=myData, method = "anova")
tree
rpart.plot(tree)
prp(tree, type = 1, extra = 1, under = TRUE)


tree2 <- rpart(Demand ~ Price + Discount + Relative_Price_of_Competing_Styles, data=myData, method = "anova")
tree2
rpart.plot(tree2)


#HELOC data new 
suppress
install.packages("caret")
install.packages("gains")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("pROC")
library(caret)
library(gains)
library(rpart.plot)
library(rpart)
library(pROC)
library(readxl)
helok <- 
  helok$HELOC <- as.factor(helok$HELOC)
set.seed(1)
myIndex <- createDataPartition(helok$HELOC, p=0.7, list = FALSE)
trainset <- helok[myIndex,]
validationset <- helok[-myIndex, ]
set.seed(1)
default_tree <- rpart(helok ~., data = trainset, method = "class")
summary(default_tree)
prp(default_tree, type = 1, extra = 1, under = TRUE)
set.seed(1)
full_tree <- rpart(helok ~., data = trainset, method = "class", cp = 0, minsplit = 2, minbucket = 1)







#Import the data from the HELOC_Data worksheet of the HELOC data file into a data frame (table) and label it myData.
#If you are using a newer version of R than version 3.5.3, execute the following line of code:
suppressWarnings(RNGversion("3.5.3"))
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("gains")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("pROC")
library(caret)
library(gains)
library(rpart)
library(rpart.plot)
library(pROC)
myData <- read_excel("C:/Users/Salil Modak/Desktop/Term 2/Fundamental of Business Analytics/cart data.xlsx", sheet = "HELOC_Data")
myData$HELOC <- as.factor(myData$HELOC)
set.seed(1)
myIndex <- createDataPartition(myData$HELOC, p=0.7, list=FALSE)
trainSet <- myData[myIndex,]
validationSet <- myData[-myIndex,]
set.seed(1)
default_tree <- rpart(HELOC ~., data = trainSet, method = "class")
summary(default_tree)
prp(default_tree, type = 1, extra = 1, under = TRUE)
printcp(default_tree)
set.seed(1)
full_tree <- rpart(HELOC ~ ., data = trainSet, method = "class", cp = 0, minsplit = 2, minbucket = 1)
prp(full_tree, type = 1, extra = 1, under = TRUE)
printcp(full_tree)

pruned_tree <- prune(full_tree, cp = 0.0164836)  
#0.0164835 value is little larger than CP of minimum error tree
prp(pruned_tree, type = 1, extra = 1, under = TRUE)
predicted_class <- predict(pruned_tree, validationSet, type = "class")
confusionMatrix(predicted_class, validationSet$HELOC, positive = "1")
predicted_prob <- predict(pruned_tree, validationSet, type= 'prob')
head(predicted_prob)
confusionMatrix(as.factor(ifelse(predicted_prob[,2]>0.26, '1', '0')), validationSet$HELOC, positive = '1')
# in above line we use 0.26 as cutoff value
validationSet$HELOC <- as.numeric(as.character(validationSet$HELOC))
gains_table <- gains(validationSet$HELOC, predicted_prob[,2])
gains_table
plot(c(0, gains_table$cume.pct.of.total*sum(validationSet$HELOC)) ~ c(0, gains_table$cume.obs), xlab = '# of cases', ylab = "Cumulative", type = "l")
lines(c(0, sum(validationSet$HELOC))~c(0, dim(validationSet)[1]), col="red", lty=2)
barplot(gains_table$mean.resp/mean(validationSet$HELOC), names.arg=gains_table$depth, xlab="Percentile", ylab="Lift", ylim=c(0, 3.0), main="Decile-Wise Lift Chart")
roc_object <- roc(validationSet$HELOC, predicted_prob[,2])
plot.roc(roc_object)
auc(roc_object)
# till here PYQ
myScoreData <- read_excel("C:/Users/Prof.Rohit Sindhwani/Downloads/cart data.xlsx", sheet = "HELOC_Score")
predicted_class_score <- predict(pruned_tree, myScoreData, type = "class")
predicted_class_score
predicted_class_prob <- predict(pruned_tree, myScoreData, type = "prob")
predicted_class_prob






#04/12/2025
install.packages('lpSolve')
library(lpSolve)

#capital budgeting 
lp.objective <- c(12.5,13.5,13,14.5,15,15.5)
lp.constraints <- matrix(c(2,2,1,4,2,4,
                           1,2,2,3,0,1,
                           1,1,1,2,4,1,
                           1,1,2,1,1,1,
                           0,1,0,0,0,1,
                           0,1,1,0,0,0),nrow=6, byrow=TRUE)
lp.directions <- c("<=","<=","<=","<=","<=","<=")
lp.rhs <- c(10,8,7,5,3,1)

lp.output <- lp("max", lp.objective, lp.constraints, lp.directions, lp.rhs, all.bin = TRUE)
lp.output
lp.output$solution

#tea problem
ip.objective <- c(190, 260, 150)
ip.constraints <- matrix(c(295, 385, 350,
                           260, 375, 0,
                           1,0,0), nrow = 3, byrow = TRUE)
ip.directions <- c(">=",">=", "<=")
ip.rhs <- c(5500,4000,8)
ip.output <- lp("min",ip.objective, ip.constraints, ip.directions, ip.rhs, all.int = TRUE)
ip.output
ip.output$solution

#transportation
ip.objective <- c(4.15, 5.95, 6.25, 3.75, 4.25, 8.25)
ip.constraints <- matrix(c(1,1,1,0,0,0,
                           0,0,0,1,1,1,
                           1,0,0,1,0,0,
                           0,1,0,0,1,0,
                           0,0,1,0,0,1), nrow = 5, byrow = TRUE)
ip.directions <- c("<=", "<=", "=", "=", "=")
ip.rhs <- c(160, 155, 85, 125, 100)
ip.output <- lp("min", ip.objective, ip.constraints, ip.directions, ip.rhs, all.int = TRUE)
ip.output
ip.output$solution


evals <- matrix(c(8.46, 7.61, 8.07, 7.68,
                  7.95, 8.37, 7.92, 8.38,
                  8.74, 7.56, 8.40, 7.86,
                  8.10, 8.09, 7.83, 8.02), nrow=4, byrow = TRUE)
class.assign <- lp.assign(evals, 'max')
class.assign$solution
class.assign
class.assign$objval/4

#Scheduling workforce problem
# a matrix of weekdays vs no of employees needed is given]
# all employees work consec for 5 days and then 2 days off
employees.start <- c(1,1,1,1,1,1,1)
workdays <- matrix(c(1,0,0,1,1,1,1,
                     1,1,0,0,1,1,1,
                     1,1,1,0,0,1,1,
                     1,1,1,1,0,0,1,
                     1,1,1,1,1,0,0,
                     0,1,1,1,1,1,0,
                     0,0,1,1,1,1,1), nrow = 7, byrow = TRUE)
employees.need <- c(9, 13, 21, 20, 10, 9, 6)
signs <- c(">=",">=",">=",">=",">=",">=",">=")
schedule.output <- lp("min", employees.start, workdays, signs, employees.need,
                      all.int = TRUE)
schedule.output$solution
schedule.output



#knn
suppressWarnings(RNGversion("3.5.3"))
library(readxl)
myData <- read_excel("C:/Users/Salil Modak/Desktop/Term 2/Fundamental of Business Analytics/supervised.xlsx", sheet = "Gym_Data")
View(myData)
library(caret)
library(gains)
library(pROC)
myData1 <- scale(myData[2:4])
View(myData1)
myData1 <- data.frame(myData1, myData$Enroll) #Enroll is the response and classifier
colnames(myData1)[4] <- 'Enroll'
myData1$Enroll <- as.factor(myData1$Enroll) # data was 0,1 which it sees as number but now it will make as factor Yes/No type

set.seed #to set randomness of the choosing training data
myIndex <- createDataPartition(myData1$Enroll, p=0.6, list=FALSE) #Creating partition p=0.6 is 60% will be training rest 40 validation, list=false
trainset <- myData1[myIndex,]
validationset <- myData1[-myIndex,]
View(trainset)
myCtrl <- trainControl(method = "cv", number = 10) # method- cross validation and k = 10
myGrid <- expand.grid(.k=c(1:10))
set.seed(1)

KNN_fit <- train(Enroll ~ ., data=trainset, method = "knn", trControl=myCtrl, tuneGrid = myGrid)  #`. is all three variable trcontol = 10 cross fole and tune grid for 1-10 getting accuracy
KNN_fit 

KNN_Class <- predict(KNN_fit, newdata = validationset)
confusionMatrix(KNN_Class, validationset$Enroll, positive = '1')
KNN_Class_prob <- predict(KNN_fit, newdata = validationset, type = 'prob')
KNN_Class_prob
confusionMatrix(as.factor(ifelse(KNN_Class_prob[,2]>0.403, '1', '0')),) 


validationset$Enroll <- as.numeric(as.character(validationset$Enroll))
gains_table <- gains(validationset$Enroll, KNN_Class_prob[,2])
gains_table

plot(c(0, gains_table$cume.pct.of.total*sum(validationset$Enroll)) ~ c(0, gains_table$cume.obs), 
     xlab = "#Cases",
     ylab = "cumalative",
     type = "1")
lines(c(0, sum(validationset$Enroll)) ~ c(0, dim(validationset$Enroll)[1]), col='red',lty=2)

#decile wise lift chart - bar plot
barplot(gains_table$mean.resp / mean(validationset$Enroll),
        names.arg = gains_table$depth, xlab = "Perc", ylab="lift",
        ylim = c(0,3), main="Decile wise")

#ROC fucntion and AUC value etc 
roc_object <- roc(validationset$Enroll, KNN_Class_prob[,2])
plot.roc(roc_object)
auc(roc_object)


Processing <- preProcess(myData[, 2:4], method = c("center","scale"))
myScoreData1 <- predict(PreProcessing, myScoreData)
KNN_Score <- predict(KNN_fit, newdata = myScoreData1)
myScoreData <- data.frame(myScoreData, KNN_Score)
View(myScoreData)


client_id <- c(1, 2, 3, 4, 5, 6)
before_weight <- c(90, 105, 88, 95, 110, 75)
after_weight  <- c(82, 99, 81, 91, 102, 70)

# Create the data frame
diet_data <- data.frame(
  Client = client_id,
  Before = before_weight,
  After = after_weight
)
View(diet_data)
diet_data$Weight_Loss <- diet_data$Before - diet_data$After
test_result <- t.test(diet_data$Weight_Loss, mu = 5, alternative = "greater", conf.level = 0.95)
test_result
if (p_val < alpha) {
  conclusion <- "Since the p-value is less than 0.05, we REJECT the Null Hypothesis."
  interpretation <- "The data SUPPORT the diet center's claim that participants lose more than 5 kg."
} else {
  conclusion <- "Since the p-value is greater than 0.05, we FAIL TO REJECT the Null Hypothesis."
  interpretation <- "The data DO NOT support the claim; there is not enough evidence to say weight loss is > 5 kg."
}