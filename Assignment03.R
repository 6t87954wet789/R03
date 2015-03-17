setwd("C:/C/Education/edX MIT 15.071 - The Analytics Edge/Unit 03 Data Files/")
getwd()

##### Logistic Regression #####

## vid 3 qq
-1.5 + 3 * 1 -0.5 * 5
exp(-1.5 + 3 * 1 -0.5 * 5)
1 / ( 1 + exp(-(-1.5 + 3 * 1 -0.5 * 5)))

## vid 4
## Logistic Regression in R

quality = read.csv("quality.csv")
str(quality)
table(quality$PoorCare)

98/131 # Baseline accuracy ~ 75%

install.packages("caTools")
library("caTools")

set.seed(88)		#not typically required - just so everyone following lecture has same random split
split = sample.split(quality$PoorCare, SplitRatio = 0.75)	#randomly splits data, while ensuring outcome variable well distributed in each piece
split #TRUE => obs in training set; FALSE => obs in test set
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)
nrow(qualityTrain)
nrow(qualityTest)

#glm ==> generalized linear model
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family=binomial) #binomial tells glm to build logistic regression model
summary(QualityLog)
# Note coefficients of OfficeVisits + Narcotics both positive, and have at least 1 *
# AIC is like adjusted R², but only between models on the same dataset. 
# AIC: Lower is better

predictTrain = predict(QualityLog, type="response")   # "response" means to give probabilities
summary(predictTrain)

tapply(predictTrain, qualityTrain$PoorCare, mean)

## qq

QQLog1 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
summary(QQLog1)

#When you resume, you're on Video 5.


