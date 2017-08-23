
set.seed(42) 

df = read.csv("Data/Voters.csv")
summary(df)
head(df)
nrow(df)

#Balanced
table(df$Election04)
install.packages("DMwR")
library(DMwR)
df.balanced <- SMOTE(Election04 ~ ., df, perc.over = 100, perc.under=200)
table(df.balanced$Election04)

df.randomized <- df.balanced[sample(nrow(df.balanced)),]

#Factor Catagorical
df.randomized$Status <- as.factor(df.randomized$Status)
df.randomized$Race <- as.factor(df.randomized$Race)
df.randomized$Party <- as.factor(df.randomized$Party)
df.randomized$Gender <- as.factor(df.randomized$Gender)
df.randomized$Election01 <- as.factor(df.randomized$Election01)
df.randomized$Election02 <- as.factor(df.randomized$Election02)
df.randomized$Election03 <- as.factor(df.randomized$Election03)
df.randomized$Election04 <- as.factor(df.randomized$Election04)
df.randomized$VotedNumber <- as.numeric(df.randomized$Election04)

#Create DF for model
df.model <- df.randomized
df.model$Id <- NULL
df.model$Status <- NULL
df.model$FullName <- NULL
df.model$Address <- NULL
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

#Run Model
install.packages("kknn")
library(kknn)

?kknn

results <- kknn(VotedInd~., df.train, df.test, k=5, kernel = "triangular", scale = TRUE)

head(results$CL)
head(results$fitted.values)

#Create Confusion Matrix
confusionMatrix(results$fitted.values,df.test$VotedInd)




