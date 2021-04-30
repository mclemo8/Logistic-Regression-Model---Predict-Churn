library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
library(tidyverse)

#Importing Data
churn <- read.csv('churn_clean.csv')

#Viewing Dataset Properties
str(churn)

#Change Categorical Variables From chrs to Factors
churn$Area <- as.factor(churn$Area)
churn$Marital <- as.factor(churn$Marital)
churn$Gender <- as.factor(churn$Gender)
churn$Churn <- as.factor(churn$Churn)
churn$Techie <- as.factor(churn$Techie)
churn$Contract <- as.factor(churn$Contract)
churn$Port_modem <- as.factor(churn$Port_modem)
churn$Tablet <- as.factor(churn$Tablet)
churn$InternetService <- as.factor(churn$InternetService)
churn$Phone <- as.factor(churn$Phone)
churn$Multiple <- as.factor(churn$Multiple)
churn$OnlineSecurity <- as.factor(churn$OnlineSecurity)
churn$OnlineBackup <- as.factor(churn$OnlineBackup)
churn$DeviceProtection <- as.factor(churn$DeviceProtection)
churn$TechSupport <- as.factor(churn$TechSupport)
churn$StreamingTV <- as.factor(churn$StreamingTV)
churn$StreamingMovies <- as.factor(churn$StreamingMovies)
churn$PaperlessBilling <- as.factor(churn$PaperlessBilling)
churn$PaymentMethod <- as.factor(churn$PaymentMethod)

#Convert Double Type Variables Into Numeric Yype and Store Them in a Variable as a Data Frame
num_columns <- c("Tenure", "MonthlyCharge")
churn[num_columns] <- sapply(churn[num_columns], as.numeric)
data_int <- churn[,c("Tenure", "MonthlyCharge")]
data_int <- data.frame(scale(data_int))

#Return the Min and Max Tenure
min(churn$Tenure); max(churn$Tenure)

#Create New Variable That Categorizes Tenure 
churn <- mutate(churn, Tenure_Grouped = Tenure)
churn$Tenure_Grouped[churn$Tenure_Grouped >=0 & churn$Tenure_Grouped <= 12] <- '0-1 Years'
churn$Tenure_Grouped[churn$Tenure_Grouped > 12 & churn$Tenure_Grouped <= 24] <- '1-2 Years'
churn$Tenure_Grouped[churn$Tenure_Grouped > 24 & churn$Tenure_Grouped <= 36] <- '2-3 years'
churn$Tenure_Grouped[churn$Tenure_Grouped > 36 & churn$Tenure_Grouped <= 48] <- '3-4 years'
churn$Tenure_Grouped[churn$Tenure_Grouped > 48 & churn$Tenure_Grouped <= 60] <- '4-5 years'
churn$Tenure_Grouped[churn$Tenure_Grouped > 60 & churn$Tenure_Grouped <= 72] <- '5-6 years'
churn$Tenure_Grouped <- as.factor(churn$Tenure_Grouped)

#Check to Ensure Tenure_Grouped Has Been Added to Dataset
str(Churn)

#Analyze Numerical variables - Correlation Matrix - Bivariate
numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

#Analyze Categorical variables - Bar Plots - Univariate
p1 <- ggplot(churn, aes(x=Gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2 <- ggplot(churn, aes(x=Area)) + ggtitle("Area") + xlab("Area") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(churn, aes(x=Marital)) + ggtitle("Marital") + xlab("Marital") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(churn, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p5 <- ggplot(churn, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, p5, ncol=2)

p6 <- ggplot(churn, aes(x=Phone)) + ggtitle("Phone Service") + xlab("Phone Service") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p7 <- ggplot(churn, aes(x=Multiple)) + ggtitle("Multiple Lines of Service") + xlab("Multiple Lines of Service") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p8 <- ggplot(churn, aes(x=Port_modem)) + ggtitle("Modem") + xlab("Modem") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p9 <- ggplot(churn, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p6, p7, p8, p9, ncol=2)

p10 <- ggplot(churn, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p11 <- ggplot(churn, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p12 <- ggplot(churn, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p13 <- ggplot(churn, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p10, p11, p12, p13, ncol=2)

p14 <- ggplot(churn, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p15 <- ggplot(churn, aes(x=Tablet)) + ggtitle("Tablet") + xlab("Tablet") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p16 <- ggplot(churn, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p17 <- ggplot(churn, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p18 <- ggplot(churn, aes(x=Techie)) + ggtitle("Techie") + xlab("Techie") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p19 <- ggplot(churn, aes(x=Tenure_Grouped)) + ggtitle("Tenure_Grouped") + xlab("Tenure_Grouped") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange (p14, p15, p16, p17, p18, p19, ncol=2)

#Remove Unnecessary Variables
churn$CaseOrder <- NULL
churn$Customer_id <- NULL
churn$Interaction <- NULL
churn$UID <- NULL
churn$City <- NULL
churn$State <- NULL
churn$County <- NULL
churn$Zip <- NULL
churn$Lat <- NULL
churn$Lng <- NULL
churn$Population <- NULL
churn$TimeZone <- NULL
churn$Job <- NULL
churn$Children <- NULL
churn$Age <- NULL
churn$Income <- NULL
churn$Email <- NULL
churn$Outage_sec_perweek <- NULL
churn$Yearly_equip_failure <- NULL
churn$Contacts <- NULL
churn$Tenure <- NULL
churn$Item1 <- NULL
churn$Item2 <- NULL
churn$Item3 <- NULL
churn$Item4 <- NULL
churn$Item5 <- NULL
churn$Item6 <- NULL
churn$Item7 <- NULL
churn$Item8 <- NULL

#Check to Ensure All Unecessary Columns Have Been Removed From Dataset
str(churn)

#Split Model to Perform Testing
intrain<- createDataPartition(churn$Churn,p=0.7,list=FALSE)
set.seed(2017)
training<- churn[intrain,]
testing<- churn[-intrain,]

#Check to Ensure Split Was Done Correctly
dim(training); dim(testing)

#Fit Logistic Regression Model
LogModel <- glm(Churn ~ .,family=binomial(link="logit"),data=training)
print(summary(LogModel))

#Analyze Deviation of Variables Using ANOVA
anova(LogModel, test = "Chisq")

#Assessing the Predictive Ability of the Logistic Regression Model
testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn == "No"] <- "0"
testing$Churn[testing$Churn == "Yes"] <- "1"
fitted_results <- predict(LogModel, newdata = testing, type = "response")
fitted_results <- ifelse(fitted_results > 0.5, 1, 0)
misClasificError <- mean(fitted_results != testing$Churn)
print(paste('Logistic Regression Accuracy', 1- misClasificError))

#Logistic Regression Confusion Matrix
print("Confusion Matrix for Logistic Regression"); table(testing$Churn, fitted_results > 0.5)

#Odds Ratio
exp(cbind(OR=coef(LogModel), confint(LogModel)))

#Decision Tree
tree <- ctree(Churn~Contract+Tenure_Grouped+PaperlessBilling, training)

#Decision Tree Matrix
pred_tree <- predict(tree, testing)
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = testing$Churn)

#Decision Tree Accuracy
p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$Churn)
tab2 <- table(Predicted = pred_tree, Actual = testing$Churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))

#Random Forest Initial Model
rfModel <- randomForest(Churn ~., data = training)
print(rfModel)

#Random Forest Prediction and Confusion Matrix Initial Model
pred_rf <- predict(rfModel, testing)
table(Predicted = pred_rf, Actual = testing$Churn)

#Random Forest Error Rate Initial Model
plot(rfModel)

#Random Forest Secondary Model
t <- tuneRF(training[, -18], training[, 18], stepFactor = 0.5, plot = TRUE, ntreeTry = 200, trace = TRUE, improve = 0.05)

#Fitting the Random Forest After tuning
rfModel_new <- randomForest(Churn ~., data = training, ntree = 200, mtry = 2, importance = TRUE, proximity = TRUE)
print(rfModel_new)

#Random Forest Prediction and Confusion Matrix Secondary Model
pred_rf_new <- predict(rfModel_new, testing)
caret::confusionMatrix(pred_rf_new, testing$Churn)

#Random Forrest Feature Importance
varImpPlot(rfModel_new, sort=T, n.var = 10, main = 'Top 10 Feature Importance')

