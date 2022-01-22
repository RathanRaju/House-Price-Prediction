install.packages("tree")

library(dplyr)
library(tidyverse) # for tidy data packages, automatically loads dplyr
library(magrittr) # for piping
#install.packages("roperators")
library(roperators)
#install.packages("caret")
library(purrr)
library(ipred)
library(tree)
library(rpart)
library(ada)
library(readr)
library(ExPanDaR)
library(ggplot2)
library(ISLR)
library(nnet)
library(roperators)


house_data <- read_csv("C:\\Users\\Rathan Raju\\OneDrive\\Desktop\\Applied Statistics\\AS\\house-data.csv")
attach(house_data)

summary_table<-prepare_descriptive_table(house_data,format = "html")
summary_table 

SalePrice <- log1p(SalePrice)
ggplot(data=house_data, aes(SalePrice)) + 
  geom_histogram(col="red", aes(fill=..count..)) +
  scale_fill_gradient("Count", low="white", high="red") + 
  labs(title = "Sale price histogram", x = "Sale price", y = "Count")
summary(SalePrice)

#Boxplots 
ggplot(data=house_data, aes(y= SalePrice, x=HouseStyle, fill=HouseStyle) ) + 
  geom_boxplot() + 
  ggtitle("Distribution of HouseStyle") +  
  ylab("Sale Price") + 
  xlab("HouseStyle")
ggplot(data=house_data, aes(y= SalePrice, x=RoofMatl, fill=RoofMatl ) ) + 
  geom_boxplot() + 
  ggtitle("Distribution of RoofMatl") +  
  ylab("Sale Price") + 
  xlab("RoofMatl")
ggplot(data=house_data, aes(x= SalePrice, y=Heating, fill=Heating ) ) + 
  geom_boxplot() + 
  ggtitle("Distribution of Heating") +  
  ylab("Sale Price") + 
  xlab("Heating")
ggplot(data=house_data, aes(y= SalePrice, x=Condition1, fill=Condition1) ) + 
  geom_boxplot() + 
  ggtitle("Distribution of Condition1") +  
  ylab("Sale Price") + 
  xlab("Condition1")
ggplot(data=house_data, aes(y= SalePrice, x=Condition2, fill=Condition2) ) + 
  geom_boxplot() + 
  ggtitle("Distribution of Condition2") +  
  ylab("Sale Price") + 
  xlab("Condition2")
ggplot(data=house_data, aes(y= SalePrice, x=RoofStyle, fill=RoofStyle) ) + 
  geom_boxplot() + 
  ggtitle("Distribution of RoofStyle") +  
  ylab("Sale Price") + 
  xlab("RoofStyle")

ggplot(data=house_data, aes(y= SalePrice, x=LotFrontage, fill=LotFrontage) ) + 
  geom_boxplot() + 
  ggtitle("Distribution of lotfontage") +  
  ylab("Sale Price") + 
  xlab("Lotfrontage")
ggplot(data=house_data, aes(y= SalePrice, x=ExterQual, fill=ExterQual ) ) + 
  geom_boxplot() + 
  ggtitle("Distribution of ExterQual") +  
  ylab("Sale Price") + 
  xlab("ExterQual")
ggplot(data=house_data, aes(y= SalePrice, x=BldgType, fill=BldgType) ) + 
  geom_boxplot() + 
  ggtitle("Distribution of BldgType") +  
  ylab("Sale Price") + 
  xlab("BldgType")
ggplot(data=house_data, aes(y= SalePrice, x=ExterCond, fill=ExterCond) ) + 
  geom_boxplot() + 
  ggtitle("Distribution of ExterCond") +  
  ylab("Sale Price") + 
  xlab("ExterCond")
ggplot(data=house_data, aes(y= SalePrice, x=Foundation, fill=Foundation) ) + 
  geom_boxplot() + 
  ggtitle("Distribution of Foundation") +  
  ylab("Sale Price") + 
  xlab("Foundation")

ggplot(data=house_data, aes(y= Neighborhood, x=SalePrice, fill=Neighborhood) ) + 
  geom_boxplot() + 
  ggtitle("Distribution of Neighborhood") +  
  ylab("Sale Price") + 
  xlab("Neighborhood")
ggplot(data=house_data, aes(y= SalePrice, x=LotConfig, fill=LotConfig) ) + 
  geom_boxplot() + 
  ggtitle("Distribution of LotConfig") +  
  ylab("Sale Price") + 
  xlab("LotConfig")

ggplot(data=house_data, aes(y= SalePrice, x=Street, fill=Street) ) + 
  geom_boxplot() + 
  ggtitle("Distribution of Street") +  
  ylab("Sale Price") + 
  xlab("Street")
ggplot(data=house_data, aes(y= SalePrice, x=PavedDrive, fill=PavedDrive) ) + 
  geom_boxplot() + 
  ggtitle("Distribution of PavedDrive") +  
  ylab("Sale Price") + 
  xlab("PavedDrive")
ggplot(data=house_data, aes(y= SalePrice, x=SaleCondition, fill=SaleCondition) ) + 
  geom_boxplot() + 
  ggtitle("Distribution of SaleCondition") +  
  ylab("Sale Price") + 
  xlab("SaleCondition")

housedata<- read.csv("C:\\Users\\Rathan Raju\\OneDrive\\Desktop\\Applied Statistics\\AS\\house-data.csv")

summary(housedata)
str(housedata)
is.factor(housedata$OverallCond)
housedata$OverallCond <-factor(housedata$OverallCond)


housedataoverallcond1<- factor(housedataoverallcond1)


housedata$housedataoverallcond1 <- ifelse(housedata$OverallCond >=1 & housedata$OverallCond <=3, "Poor", "Good")
housedata$housedataoverallcond1 <- ifelse(housedata$OverallCond >=4 & housedata$OverallCond <=6, "Average", housedata$OverallCond_predict)



housedata[is.na(housedata)]<- 0
table(is.na(housedata))


###Organised the OverallCond to Average, Good and Poor


housedataoverallcond1 <- ifelse(housedata$OverallCond < 4, "Poor", 
                               ifelse(housedata$OverallCond < 6, "Average", "Good"))

table(housedataoverallcond1)




### The Multinomial Logistic Regression#####

housedataoverallcond1 = relevel(housedataoverallcond1, ref = "Average")



mlogi <- multinom(housedataoverallcond1 ~ housedata$OverallQual
                  + housedata$SalePrice + housedata$YearBuilt, data = housedata)

summary(mlogi)


### the coefficient####
exp(coef(mlogi))

head(probability <- fitted(mlogi))

housedata$predicted <- predict(mlogi, newdata = housedata, "class")

housedata$predicted


##classfication table

classtable <- table(housedataoverallcond1, housedata$predicted)

classtable


###Calculate the diagonal sum 

round((sum(diag(classtable))/sum(classtable))*100,2)


#### Z value

z <- summary(mlogi)$coefficients/summary(mlogi)$standard.errors

z

###p values

p <- (1 - pnorm(abs(z), 0, 1)) *2

p

#load data
data<-read.csv(file = "C:\\Users\\Rathan Raju\\OneDrive\\Desktop\\Applied Statistics\\AS\\house-data.csv", header =TRUE)
#structure of data
str(data)
dim(data)
View(data)
class(data)
#number of missing values in the data
table(is.na(data))

#missing data
apply(data, 2, function(col)sum(is.na(col))/length(col))

#replacing NA values according to description
data$Alley[is.na(data$Alley)] <- "No alley"
data$BsmtQual[is.na(data$BsmtQual)] <- "No basement"
data$BsmtCond[is.na(data$BsmtCond)] <- "No basement"
data$GarageType[is.na(data$GarageType)] <- "No garage"
data$GarageCond[is.na(data$GarageCond)] <- "No garage"
data$PoolQC[is.na(data$PoolQC)] <- "No pool"
data$Fence[is.na(data$Fence)] <- "No fence"
data$MiscFeature[is.na(data$MiscFeature)] <- "None"

table(is.na(data))
#drop the rows with remaining missing values
data<-na.omit(data)

#checking the class of variables
dim(data[sapply(data, is.character)])
dim(data[sapply(data, is.integer)])


#coercing the character class to factor
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
                                           as.factor)
#class of response variable
class(data$OverallCond)
#replacing the numerical values in "OverallCond" as factors based on conditions
data$OverallCond_predict<-ifelse(data$OverallCond >=1 & data$OverallCond <=3, "Poor", "Good")
data$OverallCond_predict<-ifelse(data$OverallCond >=4 & data$OverallCond <=6, "Average",data$OverallCond_predict)

#Coercing the class of response variable from character to factor 
data$OverallCond_predict<-as.factor(data$OverallCond_predict)
#coercing those predictor variables into factor which are given as integer (but are factors)
data$FullBath<-as.factor(data$FullBath)
data$BedroomAbvGr<-as.factor(data$BedroomAbvGr)
data$KitchenAbvGr<-as.factor(data$KitchenAbvGr)
data$Fireplaces<-as.factor(data$Fireplaces)


#drop original "OverallCond" column
data<-data[,-14]

#investigating the response variable
table(data$OverallCond_predict)
prop.table(table(data$OverallCond_predict))
#it is a highly imbalanced dataset
#class "Average" has 1130 cases, whereas "Good" and "Poor" have 299 and 31 cases respectively
library(ggplot2)
ggplot(data, aes(x=OverallCond_predict))+geom_bar(fill="#3366cc")+
  labs(caption ="Fig.1 Number of observations for each class",x = "Overall Condition",y = "Count")+
  theme(plot.caption = element_text(hjust=0.5, size=rel(1)))


##training
#note:the class of the variable "YearBuilt" is retained as integer, 
#because the number of factor levels in it is more than 32, which was not acceptable
library(tree)
#classification using decision trees
#train-test split before fitting the model
set.seed(101)
#considering 70% of data as train data
train<-sample(1:nrow(data), 837)
test<-data[-train,]
#fit decision tree model on train data
tree.house <- tree(OverallCond_predict ~ ., data=data, subset=train, method="class")
tree.house
#summary of the fitted model
summary(tree.house)
#plot the tree
plot(tree.house)
text(tree.house)

#predict on test data using the fitted model
tree.pred = predict(tree.house, data[-train,], type="class")
#tree.pred
#misclassification table is used to evaluate the error in the model prediction
with(data[-train,], table(tree.pred, OverallCond_predict))

#accuracy of the prediction in given by
#number of correct predictions divided by the total number of observations
(test_accuracy1<-(258 + 34 + 2) /358)

confusionMatrix(table(tree.pred, test$OverallCond_predict))


#before pruning the above tree to reduce the complexity,
#we perform cross-validation to prune the tree to its optimal number of branches
#we use misclassification error as the basis to do the above
cv.house = cv.tree(tree.house, FUN = prune.misclass)
cv.house
#gives the sizes of the trees and the respective deviance
#plot to show the path of cross-validation
plot(cv.house)

#pruning the tree 
prune.house = prune.misclass(tree.house, best = 10)
plot(prune.house)
text(prune.house)

#predicting again on test data using the new tree model
tree.pred = predict(prune.house, data[-train,], type="class")
#misclassification table/confusion matrix
with(data[-train,], table(tree.pred, OverallCond_predict))

#accuracy
(test_accuracy2<-(261 + 34 + 2) /358)

#getting the statistics for confusion matrix
#install.packages("caret")
library(caret)
library(e1071)
confusionMatrix(table(tree.pred, test$OverallCond_predict))





#Loading the dataset
df <- read.csv("C:\\Users\\Rathan Raju\\OneDrive\\Desktop\\Applied Statistics\\AS\\house-data.csv")
str(df)

#summary of dataset
summary(df)

#checking for null values
sum(is.na(df))


#replacing all the null values 
#first converting the factor variables to char


df$Alley <- chr(df$Alley)
df$Fence <- chr(df$Fence)
df$PoolQC <- chr(df$PoolQC)
df$GarageCond <- chr(df$GarageCond)
df$BsmtCond <- chr(df$BsmtCond)
df$BsmtQual <- chr(df$BsmtQual)
df$GarageType <- chr(df$GarageType)

#Replacing the na values with values in the description

df$Alley %na<-% "No alley"
df$Fence %na<-% "No Fence"
df$PoolQC %na<-% "No Pool"
df$LotFrontage %na<-% 0
df$GarageCond %na<-% "No Garage"
df$BsmtCond %na<-% "No Basement"
df$BsmtQual %na<-% "No Basement"
df$GarageType %na<-% "No Garage"
df$MasVnrArea %na<-% 0


#converting the characters back to factors

df$Alley <- factor(df$Alley)
df$Fence <- factor(df$Fence)
df$PoolQC <- factor(df$PoolQC)
df$GarageCond<- factor(df$GarageCond)
df$BsmtCond <- factor(df$BsmtCond)
df$BsmtQual <- factor(df$BsmtQual)
df$GarageType <- factor(df$GarageType)

#checking which variable has the most na values
sum(is.na(df$MiscFeature))


cols.dont.want <- c("MiscFeature", "Id")

#dropping "miscfeature" variable and "Id"
df <- df[, ! names(df) %in% cols.dont.want, drop = F]


#checking the null values for the other variables
map(df, ~sum(is.na(.)))

dim(df)
attach(df)


#question 3

# Installing the random forest package for function
#install.packages("randomForest")


# Loading the library
library(randomForest)


#Splitting into training and test dataset
set.seed(1000)
library(caret)
library(purrr)

intrain <- createDataPartition(y = df$SalePrice, p= 0.7, list = FALSE)
training <-df[intrain,]
testing <- df[-intrain,]

# Creating random forest for regression

rf <- randomForest(SalePrice ~ . ,data = training, importance = T)
print(rf)

#viewing the importance of each variable
importance(rf)

#predicting the model using test data

rf_pred <-predict(rf, newdata = testing)
rf_pred

importance    <- importance(rf)

varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'%IncMSE'],2))
varImportance

#Standard error of estimate
SSE <- mean((rf_pred-testing$SalePrice)^2)

#r_square <- 1 - (sum((testing$SalePrice - fitForest1) ^ 2) / sum((testing$SalePrice - mean(testing$SalePrice)) ^ 2))
#r_square

library(caret)

data.frame(
  R2 = R2(rf_pred, testing$SalePrice),
  RMSE = RMSE(rf_pred , testing$SalePrice),
  MAE = MAE(rf_pred , testing$SalePrice)
)

#using random forest regression we have got the R square of around 90% and we can say that the variance of its errors is 90% less than the variance 
#of the dependent variable 


plot(rf_pred,testing$SalePrice,
     xlab="predicted",ylab="actual", xaxt="n", main = "Random Forest Regression")
abline(a=0,b=1)

#SVM 

#training our model and implementing the trainControl() method. 
#This will control all the computational overheads so that we can use the train() function provided by the caret package

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm <- train(SalePrice ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_pred <- predict(svm, newdata = testing)
svm_pred

data.frame(
  R2 = R2(svm_pred, testing$SalePrice),
  RMSE = RMSE(svm_pred, testing$SalePrice),
  MAE = MAE(svm_pred , testing$SalePrice)
)

##using SVM we have got the R square of around 66% and we can say that random forest regression performed much better

plot(svm_pred, testing$SalePrice,
     xlab="predicted",ylab="actual", xaxt="n", main = "SVM")
abline(a=0,b=1)

############Resampling for test error estimation###############

# cross validation based on random forest 

mypredict.randomForest <- function(object, newdata)
predict(object, newdata = newdata, type = c("response"))

errorest(SalePrice ~ ., data=df, model=randomForest, 
         estimator = "cv", predict= mypredict.randomForest)

# bootstrap based on random forest
errorest(SalePrice ~ ., data=df,model=randomForest,
         estimator = "boot", est.para=control.errorest(nboot = 25), predict= mypredict.randomForest)




#The cv estimate of the RMSE error of randomforest is 32407.43  and the bootstrap estimate is 31378.4 .
#Lower values of RMSE indicate better fit. RMSE is a good measure of how accurately the model predicts the response, 
#and it is the most important criterion for fit if the main purpose of the model is prediction. 
#The best measure of model fit depends on the researcher's objectives, and more than one are often useful.
