#Q1/24
# --------------------------------------------------
#A diet center claims that it has the most effective weight loss program in the region, Its advertisements say,Participants in our program lose nore than 5 kg within a month." The
#accompanying data file, "Diet_Center", shows the weights on the first day of the diet and then
#one month later, for six clients (before and after, respectively, measured in kgs).
#a) Specify and null and alternative hypothesis that tests the diet center's claim
#b) Calculate the value of the test statistic and the p-value. Assume that weight loss is normally
#distributed.
#c) At the 5% significance level, do the data support the diet center's claim?
# --------------------------------------------------
#Solution
# Import data
library(readxl)
myData <- read_excel("Diet_Center.xlsx")

# Calculate weight loss
weight_loss <- myData$Before - myData$After

# Sample statistics
x_bar <- mean(weight_loss)
s <- sd(weight_loss)
n <- length(weight_loss)

# Standard Error
SE <- s / sqrt(n)
#extra (t test)
diet <- read.csv("Diet_Center.csv")
diff <- diet$Before - diet$After
t.test(diff, mu = 5, alternative = "greater", paired = TRUE)
# p-value 
p_value <- pnorm(x_bar, mean = 5, sd = SE, lower.tail = FALSE)
p_value


#Q/2 24 - Healthy Living

library(readxl)
Endterm_FBA_2024_excel_datafile <- read_excel("Downloads/Endterm_FBA_2024 excel datafile.xlsx", 
                                              sheet = "Healthy_Living")

# 2. Run Regression
model <- lm(Healthy ~ FV + Exercise + Smoke, data = Endterm_FBA_2024_excel_datafile)

# 3. View Equation Estimates
summary(model)
library(car) # You need this library for 'vif' function

# Calculate VIF
vif(model)
library(lmtest) # Library for Breusch-Pagan test

# Method 1: Visual Plot
plot(model$fitted.values, resid(model), 
     main="Residuals vs Fitted", 
     xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red")

# Method 2: Statistical Test
bptest(model)


#Q3/24-Amanda Chen

library(readxl)
my_Data <- read_excel("C:/Users/GUNIKA/Downloads/Subscription_KNN.xlsx")
View(my_Data)

myData_scaled <- scale(my_Data[, c("Discount", "Age", "Sex")])
myData_knn <- data.frame(myData_scaled, Subscribe = my_Data$Subscribe)
myData_knn$Subscribe <- as.factor(myData_knn$Subscribe)
library(caret)
set.seed(1)
library(ggplot2)
library(lattice)

index <- createDataPartition(myData_knn$Subscribe, p = 0.6, list = FALSE)
trainSet <- myData_knn[index, ]
validationSet <- myData_knn[-index, ]

ctrl <- trainControl(method = "cv", number = 10)
grid <- expand.grid(.k = c(3,4,5))
knn_fit <- train(Subscribe ~ ., 
                 data = trainSet, 
                 method = "knn",
                 trControl = ctrl,
                 tuneGrid = grid)
knn_fit

knn_fit$results

knn_prob <- predict(knn_fit, validationSet, type = "prob")
knn_class <- ifelse(knn_prob[,2] > 0.65, "1", "0")
knn_class <- as.factor(knn_class)
confusionMatrix(knn_class, validationSet$Subscribe, positive = "1")

library(pROC)
roc_obj <- roc(validationSet$Subscribe, knn_prob[,2])
auc(roc_obj)


#Q4-Subscription 

library(readxl)
myData <- read_excel("Subscriprio.xlsx")
View(myData)

#Step 2: Create training and validation sets
trainData <- myData[1:200, ]
validationData <- myData[201:300, ]

#Step 3: Estimate LPM
lpm_model <- lm(Subscribe ~ Discount + Age + Sex, data = trainData)
summary(lpm_model)

#Step 4: Predict & classify (cutoff = 0.5)
lpm_prob <- predict(lpm_model, validationData)
lpm_class <- ifelse(lpm_prob > 0.5, 1, 0)

#Step 5: Accuracy of LPM
mean(lpm_class == validationData$Subscribe)
# This value is the holdout accuracy of Model 1


#Step 6: Estimate Logistic Model
logit_model <- glm(Subscribe ~ Discount + Age + Sex, 
                   data = trainData, 
                   family = binomial)
summary(logit_model)

#Step 7: Predict & classify
logit_prob <- predict(logit_model, validationData, type = "response")
logit_class <- ifelse(logit_prob > 0.5, 1, 0)

#Step 8: Accuracy of Logistic Model
mean(logit_class == validationData$Subscribe)

Step 9: Convert Subscribe to factor
library(caret)
myData$Subscribe <- as.factor(myData$Subscribe)

# LPM using 3-fold CV
ctrl <- trainControl(method = "cv", number = 3)

set.seed(1)
lpm_cv <- train(Subscribe ~ Discount + Age + Sex,
                data = myData,
                method = "lm",
                trControl = ctrl)

lpm_cv
# Accuracy reported is cross-validated accuracy for Model 1

# Logistic Regression using 3-fold CV
set.seed(1)
logit_cv <- train(Subscribe ~ Discount + Age + Sex,
                  data = myData,
                  method = "glm",
                  family = binomial,
                  trControl = ctrl)

logit_cv

#Q5/24-Charity

#STEP 1: Load required libraries
library(readxl)
library(rpart)
library(rpart.plot)
library(ROCR)

myData <- read_excel("Charity.xlsx")
View(myData)

#STEP 3: Convert variables to factor
myData$Charity <- as.factor(myData$Charity)
myData$Gender <- as.factor(myData$Gender)
myData$Married <- as.factor(myData$Married)

#STEP 4: Split into training and test data (Holdout)
set.seed(1)
trainIndex <- sample(1:nrow(myData), 0.7*nrow(myData))

trainData <- myData[trainIndex, ]
testData <- myData[-trainIndex, ]

# STEP 5: Grow the FULL TREE
full_tree <- rpart(Charity ~ Educ + Income + Age + Gender + Married,
                   data = trainData,
                   method = "class",
                   cp = 0)

rpart.plot(full_tree)

#STEP 6: Display CP table
printcp(full_tree)
plotcp(full_tree)

# STEP 7: Minimum Error Tree
min_cp <- full_tree$cptable[which.min(full_tree$cptable[,"xerror"]), "CP"]

min_error_tree <- prune(full_tree, cp = min_cp)
rpart.plot(min_error_tree)

# STEP 8: Best Pruned Tree (1-SE Rule)
xerror_min <- min(full_tree$cptable[,"xerror"])
xerror_se <- full_tree$cptable[which.min(full_tree$cptable[,"xerror"]), "xstd"]

best_cp <- max(full_tree$cptable[
  full_tree$cptable[,"xerror"] < xerror_min + xerror_se, "CP"])

best_pruned_tree <- prune(full_tree, cp = best_cp)
rpart.plot(best_pruned_tree)

#(a) Number of Leaf Nodes
length(unique(best_pruned_tree$where))
length(unique(min_error_tree$where))

# STEP 9: Prediction on Test Data
tree_prob <- predict(best_pruned_tree, testData, type = "prob")[,2]
tree_class <- ifelse(tree_prob > 0.5, 1, 0)

# STEP 10: Confusion Matrix
actual <- testData$Charity
conf_matrix <- table(Predicted = tree_class, Actual = actual)
conf_matrix


#(b) Performance Measures
#Misclassification Rate
misclass_rate <- mean(tree_class != actual)
misclass_rate

#Sensitivity (Recall)
sensitivity <- conf_matrix["1","1"] /
  (conf_matrix["1","1"] + conf_matrix["0","1"])
sensitivity

#Specificity
specificity <- conf_matrix["0","0"] /
  (conf_matrix["0","0"] + conf_matrix["1","0"])
specificity

#Precision
precision <- conf_matrix["1","1"] /
  (conf_matrix["1","1"] + conf_matrix["1","0"])
precision

#️ Theory line to write:
  #The best pruned tree demonstrates balanced classification performance as reflected by sensitivity, specificity, precision, and a low misclassification rate.

#(c) ROC Curve & AUC
#STEP 11: ROC Curve
pred <- prediction(tree_prob, actual)
perf <- performance(pred, "tpr", "fpr")

plot(perf, col = "blue", main = "ROC Curve for Classification Tree")
abline(0,1)

#STEP 12: AUC Value
auc <- performance(pred, "auc")
auc_value <- auc@y.values[[1]]
auc_value

#Q6-24/Apple

#STEP 1: Load required libraries
library(readxl)
library(arules)
library(arulesViz)

#STEP 2: Import data
appleDesc <- read_excel("Apple.xlsx", sheet = "AppleDescription")
appleCart <- read_excel("Apple.xlsx", sheet = "AppleCart")

View(appleDesc)
View(appleCart)


#STEP 3: Convert shopping cart data into transactions
#(Assuming AppleCart has TransactionID and Item)
trans <- as(split(appleCart$Item, appleCart$TransactionID), "transactions")
summary(trans)

#STEP 4: Generate association rules
rules <- apriori(trans,
                 parameter = list(supp = 0.10, conf = 0.50))

summary(rules)

#STEP 5: Sort rules by Lift
rules_lift <- sort(rules, by = "lift", decreasing = TRUE)
inspect(rules_lift)

#(a) Rule with the Largest Lift Ratio
top_rule <- rules_lift[1]
inspect(top_rule)

#(c) Review Top 10 Rules
top10_rules <- rules_lift[1:10]
inspect(top10_rules)

#Q1/23


library(lpSolve)

# Objective: Minimize sum of nurses (x1 + x2 + x3 + x4 + x5)
obj.fun <- c(1, 1, 1, 1, 1)

# Constraints Matrix (Rows = Time Slots, Cols = Shifts)
# 1 if the shift covers that time slot, 0 otherwise
constr <- matrix(c(
  1, 0, 0, 0, 0,  # 12am-4am (Shift 1 only)
  1, 1, 0, 0, 0,  # 4am-8am (Shift 1 + Shift 2)
  0, 1, 1, 0, 0,  # 8am-12pm (Shift 2 + Shift 3)
  0, 0, 1, 1, 0,  # 12pm-4pm (Shift 3 + Shift 4)
  0, 0, 0, 1, 1,  # 4pm-8pm (Shift 4 + Shift 5)
  0, 0, 0, 0, 1   # 8pm-12am (Shift 5 only)
), ncol = 5, byrow = TRUE)

# Requirements (RHS)
rhs <- c(10, 24, 18, 10, 23, 17)
direction <- rep(">=", 6)

# Solve
sol <- lp("min", obj.fun, constr, direction, rhs, all.int = TRUE)
print(sol$solution) # Output: Number of nurses needed per shift

#Q3/23 Republican

library(readxl)
library(caret)
library(class)

myData <- read_excel("bluered.xlsx")

# PART (a): Continuous Variables Only
X <- myData[, c("Age", "HouseholdSize", "Income", "Education")]
y <- as.factor(myData$Undecided)

X_scaled <- scale(X)

set.seed(1)
index <- createDataPartition(y, p = 0.7, list = FALSE)
trainX <- X_scaled[index, ]
testX  <- X_scaled[-index, ]
trainY <- y[index]
testY  <- y[-index]

accuracy <- c()
for(k in 1:20){
  pred <- knn(trainX, testX, trainY, k = k)
  accuracy[k] <- mean(pred == testY)
}

accuracy
which.max(accuracy)

# PART (b): All Variables
myData$Gender    <- ifelse(myData$Gender == "M", 1, 0)
myData$Married   <- ifelse(myData$Married == "Y", 1, 0)
myData$HomeOwner <- ifelse(myData$HomeOwner == "Y", 1, 0)
myData$Church    <- ifelse(myData$Church == "Y", 1, 0)

X_all <- myData[, c("Age","HouseholdSize","Income","Education",
                    "Gender","Married","HomeOwner","Church")]

y <- as.factor(myData$Undecided)
X_all_scaled <- scale(X_all)

set.seed(1)
index <- createDataPartition(y, p = 0.7, list = FALSE)
trainX <- X_all_scaled[index, ]
testX  <- X_all_scaled[-index, ]
trainY <- y[index]
testY  <- y[-index]

accuracy_all <- c()
for(k in 1:20){
  pred <- knn(trainX, testX, trainY, k = k)
  accuracy_all[k] <- mean(pred == testY)
}

accuracy_all
which.max(accuracy_all)


#Q-4/23

library(readxl)
library(caret)

data <- read_excel("bluered.xlsx")

data$Female    <- ifelse(data$Gender == "F", 1, 0)
data$HomeOwner <- ifelse(data$HomeOwner == "Y", 1, 0)
data$Married   <- ifelse(data$Married == "Y", 1, 0)
data$Church    <- ifelse(data$Church == "Y", 1, 0)

data$Undecided <- as.factor(data$Undecided)

set.seed(1)
index <- createDataPartition(data$Undecided, p = 0.7, list = FALSE)
train <- data[index, ]
test  <- data[-index, ]

full_model <- glm(
  Undecided ~ Age + HomeOwner + Female + Married +
    HouseholdSize + Income + Education + Church,
  data = train,
  family = binomial
)

summary(full_model)

reduced_model <- glm(
  Undecided ~ Age + HomeOwner + Married +
    Income + Education + Church,
  data = train,
  family = binomial
)

summary(reduced_model)

prob_full <- predict(full_model, test, type = "response")
prob_red  <- predict(reduced_model, test, type = "response")

pred_full <- ifelse(prob_full > 0.5, 1, 0)
pred_red  <- ifelse(prob_red > 0.5, 1, 0)

mean(pred_full == test$Undecided)
mean(pred_red == test$Undecided)

exp(coef(reduced_model))

#Q-5/23

library(readxl)
library(rpart)
library(rpart.plot)
library(caret)

data <- read_excel("bluered.xlsx")

data$Female    <- ifelse(data$Gender == "F", 1, 0)
data$HomeOwner <- ifelse(data$HomeOwner == "Y", 1, 0)
data$Married   <- ifelse(data$Married == "Y", 1, 0)
data$Church    <- ifelse(data$Church == "Y", 1, 0)
data$Undecided <- as.factor(data$Undecided)

set.seed(1)
index <- createDataPartition(data$Undecided, p = 0.7, list = FALSE)
train <- data[index, ]
test  <- data[-index, ]

full_tree <- rpart(
  Undecided ~ Age + HomeOwner + Female + Married +
    HouseholdSize + Income + Education + Church,
  data = train,
  method = "class",
  cp = 0
)

printcp(full_tree)
plotcp(full_tree)

xerror_min <- min(full_tree$cptable[,"xerror"])
xerror_se  <- full_tree$cptable[which.min(full_tree$cptable[,"xerror"]), "xstd"]

best_cp <- max(
  full_tree$cptable[
    full_tree$cptable[,"xerror"] < xerror_min + xerror_se,
    "CP"
  ]
)

best_tree <- prune(full_tree, cp = best_cp)

tree_prob <- predict(best_tree, test, type = "prob")[,2]
tree_class <- ifelse(tree_prob > 0.5, 1, 0)

conf_matrix <- table(Predicted = tree_class, Actual = test$Undecided)
conf_matrix

overall_error <- mean(tree_class != test$Undecided)
overall_error

class1_error <- conf_matrix["0","1"] /
  (conf_matrix["0","1"] + conf_matrix["1","1"])
class1_error

class0_error <- conf_matrix["1","0"] /
  (conf_matrix["1","0"] + conf_matrix["0","0"])
class0_error

new_voter <- data.frame(
  Age = 50,
  HomeOwner = 1,
  Female = 0,
  Married = 1,
  HouseholdSize = 4,
  Income = 150000,
  Education = 15,
  Church = 1
)

new_prob <- predict(best_tree, new_voter, type = "prob")[,2]
new_class <- ifelse(new_prob > 0.5, 1, 0)

new_prob
new_class
