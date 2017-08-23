
set.seed(42) 

#Read
df <- read.csv("Data/Voters.csv")
summary(df)
head(df)
nrow(df)

#Clean
df.cleaned <- df[complete.cases(df),]
nrow(df.cleaned)
df.cleaned <- NULL

#Balanced
table(df$Election04)
install.packages("DMwR")
library(DMwR)
df.balanced <- SMOTE(Election04 ~ ., df, perc.over = 100)
table(df.balanced$Election04)

#Randomized
df.randomized <- df.balanced[sample(nrow(df.balanced)),]

#Scale Continous
hist(df.randomized$Age)

df.randomized$Age.Scaled <- scale(df.randomized$Age)
hist(df.randomized$Age.Scaled)

#Factor Catagorical
df.randomized$Status <- as.factor(df.randomized$Status)
df.randomized$Race <- as.factor(df.randomized$Race)
df.randomized$Party <- as.factor(df.randomized$Party)
df.randomized$Gender <- as.factor(df.randomized$Gender)
df.randomized$Election01 <- as.factor(df.randomized$Election01)
df.randomized$Election02 <- as.factor(df.randomized$Election02)
df.randomized$Election03 <- as.factor(df.randomized$Election03)
df.randomized$Election04 <- as.factor(df.randomized$Election04)

df.randomized$RaceNumber <- as.numeric(df.randomized$Race)
df.randomized$PartyNumber <- as.numeric(df.randomized$Party)
df.randomized$GenderNumber <- as.numeric(df.randomized$Gender)
df.randomized$VotedNumber <- as.numeric(df.randomized$Election04)

#Create DF for model
df.model <- df.randomized
df.model$Id <- NULL
df.model$Status <- NULL
df.model$Race <- NULL
df.model$Party <- NULL
df.model$Gender <- NULL
df.model$FullName <- NULL
df.model$Address <- NULL
df.model$Age <- NULL
df.model$Election01 <- NULL
df.model$Election02 <- NULL
df.model$Election03 <- NULL
df.model$Election04 <- NULL
df.model$VotedInd <- df.model$VotedNumber - 1
df.model$VotedNumber <- NULL
df.model$VotedInd <- as.factor(df.model$VotedInd)

#Split data
install.packages("caret") 
library(caret)
library(datasets)
split <-createDataPartition(y = df.model$VotedInd, p = 0.7, list = FALSE)
df.train <-df.model[split,]
df.test <-df.model[-split,]

#Split and Remove Answer
trainClass <- df.train$VotedInd
testClass <- df.test$VotedInd
df.train$VotedInd <- NULL
df.test$VotedInd <- NULL

#Run Model
install.packages("class")
library(class)
results - knn(train = df.train, test = df.test,cl = trainClass, k=5)

#Create Confusion Matrix
confusionMatrix(results,testClass)

#Accuracy:  How much we got right
#Sensitivity (Recall): True Postitive Rate
#Specificity: True Negative Rate


#To evaluate the model, we call on package pROC for an auc score and plot:

#library(pROC)
#auc <- roc(testSplit$target, pred)
#print(auc)
## Data: pred in 1509 controls (testSplit$target 0) < 72 cases (testSplit$target 1).
## Area under the curve: 0.985
#plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
#abline(h=1,col='blue',lwd=2)
#abline(h=0,col='red',lwd=2)


