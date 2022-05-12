###### COLLEGE ADMISSION
###### Loading all necessary library
library(dplyr)
library(lubridate)
library(ggplot2)
#install.packages("ggstatsplot")
library(ggstatsplot)
install.packages("caret")
library(caret)

#### Analysis Task

#### importing Data set
admission_dataset <- read.csv("College_admission.csv")  # ca is data frame having college admission data set

View(admission_dataset)

# Predicitve Analysis -----------------------

### Find the missing values. (if any, perform missing value treatment)
sum(is.na(admission_dataset))
na.omit(admission_dataset)
View(admission_dataset)

# -------------------------------------------------------------------------------
### Find outliers (if any, then perform outlier treatment)
boxplot(admission_dataset)
boxplot(admission_dataset$gre,plot = TRUE)$out
boxplot(admission_dataset$gpa,plot = TRUE)$out
#outliers_gre <- boxplot(ca$gre,plot = FALSE)$out

# # Treating outliers (gre) & treating outliers
# admission_dataset <- ca
# View(admission_dataset)
# admission_dataset <- admission_dataset[-which(ca$gre %in% outliers_gre),]
# View(admission_dataset)
# # Treating outliers (gpa) & treating outliers
# boxplot(ca$gpa,plot = FALSE)$out
# outlier_gpa <- boxplot(ca$gpa,plot = FALSE)$out
# admission_dataset <- admission_dataset[-which(ca$gpa %in% outlier_gpa),]
# View(admission_dataset)
# 
# boxplot(admission_dataset)
 ## outlier treated admission_dataset has been stored in "admission_dataset" variable
#---------------------------------------------------------------------------------
###  Find the structure of the data set and if required, 
###  transform the numeric data type to factor and vice-versa.

# finding structure
str(admission_dataset)
# Transforming integer type to factor

admission_dataset$admit <- as.factor(admission_dataset$admit)
admission_dataset$ses <- as.factor(admission_dataset$ses)
admission_dataset$rank <- as.factor(admission_dataset$rank)

admission_dataset$Gender_Male <- as.factor(admission_dataset$Gender_Male)
admission_dataset$Race <- as.factor(admission_dataset$Race)
str(admission_dataset)
# ---------------------------------------------------------------------------
### Find whether the data is normally distributed or not. 
### Use the plot to determine the same. 
hist(admission_dataset$gre)
hist(admission_dataset$gpa)

plot(density(admission_dataset$gre))
plot(density(admission_dataset$gpa))

# shapiro test to check normality distribution

shapiro.test(admission_dataset$gre) 
##### pvalue is lesser than alpha value.. <- not normally distributed
shapiro.test(admission_dataset$gpa)
##### pvalue is lesser than alpha value.. <- not normally distributed

# ------------------------------------------------------------------------
#### Normalize the data if not normally distributed.
dataNorm <- admission_dataset
dataNorm$gre <- scale(admission_dataset$gre, scale = TRUE, center =TRUE )
dataNorm$gpa <- scale(admission_dataset$gpa, scale=TRUE, center = TRUE)

View(dataNorm)

##### Use variable reduction techniques to identify significant variables.
### Run logistic model to determine the factors that influence the admission process of a student (Drop insignificant variables) 

model1 <- glm(formula=admit~.,data=dataNorm,family = binomial)
print(summary(model1))
AIC_model1 <- step(object=model1, direction="both")
AIC_model1

model2 <- glm(formula=admit~ gre + gpa,data=dataNorm,family = binomial)
print(summary(model2))
AIC_model2 <- step(object=model2, direction="both")
AIC_model2
model3 <- glm(formula = admit ~ gpa + gre + Race + rank, 
                 data=dataNorm, family = binomial)
print(summary(model3)) 
AIC_model3 <- step(object=model3, direction="both")
AIC_model3

### Calculate the accuracy of the model and run validation techniques.
# We will split our dataset into Train and Test data. 
# The Train dataset will be used to build the model and 
# the Test dataset will used to validate how well our model is performing.
set.seed(1234)
split <- sample(1:nrow(dataNorm), 0.75*nrow(dataNorm))
train_data <- dataNorm[split, ]
test_data <- dataNorm[-split, ]
View(train_data)
str(test_data)
# model building
# full model
train_data_model1 <- glm(formul=admit~., data = train_data, family=binomial)
summary(train_data_model1)


# feature selection as there are many in significant variables

step <- step(train_data_model1)
summary(step)
car::vif(step) # check vif value
step$fitted.values
?fitted.values
# probabilty of train_data
train_data_p <- predict(step, newdata=train_data, type="response")
train_data_p
# probability of test data
test_data_p <- predict(step,newdata=test_data, type="response")
test_data_p

# admit prediction on the train and test data
admit_train <- as.factor(ifelse(train_data_p<0.5,0,1))
admit_test <- as.factor(ifelse(test_data_p<0.5,0,1))

# confusion matrix
Train_data_table <- confusionMatrix(train_data$admit,admit_train)
Train_data_table
# Accuracy is 68.67%
Test_data_table <- confusionMatrix(test_data$admit, admit_test)
Test_data_table
# Accuracy is 74%

####Try other modelling techniques like decision tree and SVM and select a champion model 


### Support Vector machine model 
# install.packages("e1071")
library(e1071)


SVM_model <- svm(admit~.,data=train_data, kernel="linear")
SVM_model
SVM_test <- predict(SVM_model,newdata=test_data, type="reponse")
SVM_test

svm_table <- confusionMatrix(test_data$admit,SVM_test)
svm_table

# Accuracy is 71%

### Decision Tree Model
install.packages("ISLR")
install.packages("tree")
library(ISLR)
library(tree)

D_tree_model <- tree(admit~., data=train_data)
summary(D_tree_model)

D_tree_test <- predict(D_tree_model, newdata=test_data, type="class")
D_tree_test

D_tree_table <- confusionMatrix(test_data$admit,D_tree_test)
D_tree_table

# Accuracy is 73%

##### Random Forest Model
install.packages("randomForest")
install.packages("caTools")
library(randomForest)
library(caTools)

random_forest_model <-  randomForest(admit~., data=train_data)
random_forest_model
random_test_model <- predict(random_forest_model, newdata=test_data, type="class")
random_test_model

random_table <- confusionMatrix(test_data$admit,random_test_model)
random_table
# Accuracy is 72%

### Determine the accuracy rates for each kind of model -----------------------------
# 1. Logistic Regression <- 74%
# 2. Support Vector Machine <- 71%
# 3. Decision Tree <- 73%
# 4. Random Forest <- 72%

#---------------------------------------------------------------------------------
#### Select the most accurate model 

### Logistic Regression model gives more Accurate rate: 74% 
# --------------------------------------------------------------------------------
# Identify other Machine learning or statistical techniques
# Naive Bayes Algorithm
install.packages("naivebayes")
library(naivebayes)
naive_model <- naive_bayes(admit~.,data=train_data)
naive_model
naive_test <- predict(naive_model,newdata=test_data, type="class")
naive_test

naive_table <- confusionMatrix(test_data$admit,naive_test)
naive_table
# Accuracy is 73%
#---------------------------------------------------------------------------------
# Descriptive: 
# Categorize the average of grade point into High, Medium, and Low (with admission probability percentages) 
# and plot it on a point chart.  
# Cross grid for admission variables with GRE Categorization is shown below:
#   
# GRE	Categorized
# 0-440	Low
# 440-580	Medium
# 580+	High
admission_dataset$Category[admission_dataset$gre <= 580 | admission_dataset$gre >440] = "Medium"
admission_dataset$Category[admission_dataset$gre <= 440] = "Low"
admission_dataset$Category[admission_dataset$gre > 580] ="High"
admission_dataset$Category <- as.factor(admission_dataset$Category)

  
library(tidyverse)
install.packages("palmerpenguins")
library(palmerpenguins)
install.packages("ggbeeswarm")
library(ggbeeswarm)
install.packages("ggforce")
library(ggforce)
ggplot(data = admission_dataset) +
  aes(y = Category, x = gre, fill=Category) +
  geom_violin() +
  geom_beeswarm(cex = 2.5, groupOnX = FALSE) +
  coord_flip() + labs(title = "GRE Category")+ ylab("Category")+xlab("GRE Score")
