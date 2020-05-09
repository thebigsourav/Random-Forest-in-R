# Random Forest Classification
rm(list = ls())

library(readr)
library(class)
library(dplyr)
library(tidyverse)
library(lmtest)
library(ggplot2)
library(GGally)

# Importing the dataset
dataset<- read.csv(file.choose(), header= T)

#compactly displaying the summary of the dataset
str(dataset)

#removing id column
dataset = subset(dataset, select = -c(1) )
head(dataset)

#exploratory data analysis
#density plot for age
ggplot(dataset, aes(x=age, color=cross_sell))+geom_density()

#density plot for children
ggplot(dataset, aes(x=children, color=cross_sell))+geom_density()

#density plot for income
ggplot(dataset, aes(x=income, color=cross_sell))+geom_density()

# Violin plot for each attribute
dataNorm = dataset
head(dataNorm)
dataNorm[,c(1,4,6)] = scale(dataset[,c(1,4,6)])
dataNorm %>%
        gather(Attributes, value, c(1,4,6)) %>%
        ggplot(aes(x=reorder(Attributes, value, FUN=median), y=value, fill=Attributes,)) +
        ylim(-2,3.5)+
        geom_violin(show.legend=FALSE) +
        labs(title="Bank dataset",
             subtitle="Violin plot for each attribute") +
        theme_bw() +
        theme(axis.title.y=element_blank(),
              axis.title.x=element_blank())

# Scatter plot and correlations
ggpairs(cbind(dataset, Cluster=as.factor(dataset$cross_sell)),
        columns = c(1,4,6), aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        axisLabels="none", switch="both") +
        theme_bw()

# Formatting the variables as per requirement
dataset$age <- as.double(dataset$age)
dataset$sex <- factor(dataset$sex)
dataset$region <- factor(dataset$region)
dataset$income <- as.double(dataset$income)
dataset$married <- factor(dataset$married)
dataset$children <- as.integer(dataset$children)
dataset$car <- factor(dataset$car)
dataset$saving_ac <- factor(dataset$saving_ac)
dataset$current_ac <- factor(dataset$current_ac)
dataset$cross_sell <- as.character(dataset$cross_sell)
dataset[which(dataset$cross_sell == "YES"), ]$cross_sell <- 1
dataset[which(dataset$cross_sell == "NO"), ]$cross_sell <- 0
dataset$cross_sell <- as.integer(dataset$cross_sell)

#No Missing values are there
str(data)
sum(is.na(data))

dim(data)

# Checking class bias for the dependent variable i.e. cross_sell
table(dataset$cross_sell)

# Encoding the target feature as factor
dataset$cross_sell = factor(dataset$cross_sell, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# Creating the Training sample ---

# all 1
input_one  <- dataset[which(dataset$cross_sell == 1), ] 
# all 0
input_zero <- dataset[which(dataset$cross_sell == 0), ]  
# for repeatability of samples
set.seed(100)                                        
# 1's for training
input_one_training_rows <- sample(1:nrow(input_one), 0.8*nrow(input_one))  
# 0's for training. Pick as many 0's as 1's
input_zero_training_rows <- sample(1:nrow(input_zero), 0.8*nrow(input_zero))  
training_one <- input_one[input_one_training_rows, ]  
training_zero <- input_zero[input_zero_training_rows, ]
# row bind the 1's and 0's
trainingData <- rbind(training_one, training_zero) 
table(trainingData$cross_sell)
head(trainingData)

# Creating Test sample based on similar steps while creating training data
test_one <- input_one[-input_one_training_rows, ]
test_zero <- input_zero[-input_zero_training_rows, ]
# row bind the 1's and 0's
testData <- rbind(test_one, test_zero)  
table(testData$cross_sell)
head(testData)

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = trainingData,
                          y = trainingData$cross_sell,
                          ntree = 500)

#confusion matrix for random forest
library(caret)
library(e1071)
predictModel_Test<-predict(classifier,testData,type = "class")
confusionMatrix(predictModel_Test, as.factor(testData$cross_sell))

#visualizing one tree using party
#install.packages("party")
library(party)
x <- ctree(cross_sell ~ ., data=trainingData)
plot(x, type="simple")

#mathematical representation of a tree
getTree(classifier, k=4, labelVar = FALSE)

#visualizing the classification
plot(randomForest(cross_sell ~ ., testData, keep.forest=FALSE, ntree=500), log="y")

#visualizing one tree using CART
library(rpart)
library(rpart.plot)
one_tree_train=rpart(cross_sell~., data = trainingData, method = "class")
one_tree_test=rpart(cross_sell~., data = testData, method = "class")
prp(one_tree_test, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

#confusion matrix for CART
predictCART = predict(one_tree_train,testData,type = "class")
confusionMatrix(predictCART, as.factor(testData$cross_sell))

#random forest classification in 3d

head(dataset)
dataset1 <- dataset[,c(1,4,10)]
classifier1 <- randomForest(x = dataset1,
                          y = dataset1$cross_sell,
                          ntree = 500)

library(plotly)
income = dataset1$income
age = dataset1$age
cross_sell = dataset1$cross_sell
plot_ly(x=as.vector(income),y=as.vector(age),z=cross_sell, type="scatter3d", mode="markers", name = "Obs", marker = list(size = 3)) %>%
        add_trace(x=as.vector(income),y=as.vector(age),z=predict(classifier1, newdata=dataset1), type = "mesh3d", name = "Preds")
