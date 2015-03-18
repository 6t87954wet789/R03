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

require("caTools") || install.packages("caTools")
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

#Video 5 Thresholding
summary(qualityTrain)
summary(qualityTest)

table(qualityTrain$PoorCare, predictTrain > 0.5)	# threshold t = 0.5

#    FALSE TRUE			
#  0    70    4			4 mistakes		4 False Positives	70 True Positives
#  1    15   10			15 mistakes		15 False Negatives	10 True Positives

Sensitivity = 10 / 25  #Sensitivity = TP / (TP + FN) ==> TRUE POSITIVE RATE
Specificity = 70 / 74  #Specificity = TN / (TN + FP) ==> TRUE NEGATIVE RATE
Sensitivity
Specificity

table(qualityTrain$PoorCare, predictTrain > 0.7)	# threshold t = 0.7
#    FALSE TRUE			
#  0    73    1			1 mistakes		1 False Positives	73 True Positives
#  1    17    8			17 mistakes		17 False Negatives	8 True Positives

Sensitivity = 8 / 25
Specificity = 73 / 74
Sensitivity
Specificity

# Increase in t ==> Decrease Sensitivity, Increase Specificity

table(qualityTrain$PoorCare, predictTrain > 0.2)	# threshold t = 0.2
Sensitivity = 16 / 25
Specificity = 54 / 74
Sensitivity
Specificity

# Video 6 ROC Curves

require("ROCR") || install.packages("ROCR")
library("ROCR")

ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf = performance(ROCRpred, "tpr", "fpr") #true positive rate, false positive rate
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE)	#color represents value of  threshold t
plot(ROCRperf, colorize=TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj=c(-0.2, 1.7))

# Video 7: Interpreting the model

table(qualityTrain$PoorCare, predictTrain > 0.3)
Sensitivity = 16 / 25
Specificity = 54 / 74
Sensitivity
Specificity

predictTest = predict(QualityLog, type="response", newdata=qualityTest)
table(qualityTest$PoorCare, predictTest > 0.3)

ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
plot( performance(ROCRpredTest, "tpr", "fpr"), colorize=TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj=c(-0.2, 1.7))
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

