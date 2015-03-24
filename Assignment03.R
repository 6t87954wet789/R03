#Assignment 3
setwd("C:/C/Education/edX MIT 15.071 - The Analytics Edge/Unit 03 Data Files")
getwd()

### PART 1 - POPULARITY OF MUSIC RECORDS

# Problem 1
songs = read.csv("songs.csv")
nrow(subset(songs, year==2010)) #373

nrow(subset(songs, artistname=="Michael Jackson")) #18

subset(songs, artistname=="Michael Jackson" & Top10==1)$songtitle

unique(songs$timesignature)
table(songs$timesignature)

subset(songs, tempo == max(songs$tempo))$songtitle	#Wanna Be Startin' Somethin'

# Problem 2
SongsTrain = subset(songs, year < 2010)
nrow(SongsTrain)	

SongsTest =  subset(songs, year >= 2010)
nrow(SongsTest)

nrow(SongsTest) + nrow(SongsTrain) - nrow(songs) #Should be 0

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

#Problem 3

cor(SongsTrain$loudness, SongsTrain$energy)	#0.7399067

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

#Problem 4
#In the remainder of this problem, we'll just use Model 3.

predictTest = predict(SongsLog3, type="response", newdata=SongsTest)
table(SongsTest$Top10, predictTest > 0.45)
  #   FALSE TRUE  <- Predicted
  # 0   309    5
  # 1    40   19
  # ^ 
  # Actual
recordsCorrect = 19+309
totalRecords = 309 + 5 + 40 + 19
accuracyRate = recordsCorrect / totalRecords
accuracyRate	#0.8793566 <- 4.1

# 4.2 baseline prediction is FALSE
#   If we chose FALSE all the time, what would accuracy be?

recordsCorrect = 309+5
totalRecords = 309 + 5 + 40 + 19
accuracyRate = recordsCorrect / totalRecords
accuracyRate	#0.8418231 <- 4.2

TP = 19
FN = 40
sensitivity = TP / (TP + FN)
sensitivity
TN = 309
FP = 5
specificity = TN / (TN + FP)
specificity


### PART 2 - PREDICTING PAROLE VIOLATORS

setwd("C:/C/Education/edX MIT 15.071 - The Analytics Edge/Unit 03 Data Files")
getwd()

parole = read.csv("parole.csv")
summary(parole)
nrow(parole)	#675

nrow(subset(parole, violator == 1))		#78

parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole)

table(parole$crime)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)		#70% to training set
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

#Problem 4

ParoleLog1 = glm(violator ~ ., data = train, family = "binomial")
summary(ParoleLog1)
exp(1.61)

params_43 = 	c(1, #male
				 1,	#white
				 50, #age
				 0, #not KY
				 0, #not LA
				 0,	#nit VA				 
				 3, #months served
				 12, #max sentence
				 0, # not multiple offences
				 1, #yes larceny
				 0,	# not not drugrelated
				 0  # not driving related
				 )

oddsFunctionLogistic = function(logisticModel, params){	
	intercept = as.numeric(logisticModel$coefficients[1])
	coeff = as.vector(logisticModel$coefficients[2:length(logisticModel$coefficients)])	
	logOdds = intercept + sum(coeff * params )
	odds = exp(logOdds)
	return(odds)
}

oddsFunctionLogistic(ParoleLog1, params_43)

probFunctionLogistic = function(logisticModel, params){
	intercept = as.numeric(logisticModel$coefficients[1])
	coeff = as.vector(logisticModel$coefficients[2:length(logisticModel$coefficients)])	
	logOdds = intercept + sum(coeff * params )
	P = 1 / 
		( 1 + exp(-1 * logOdds) )
	return(P)
}

probFunctionLogistic(ParoleLog1, params_43)

#Problem 5
pred = predict(ParoleLog1, type="response", newdata=test)
max(pred)

table(test$violator, pred > 0.5)

#   FALSE TRUE		<-- Predicted
# 0   167   12
# 1    11   12
# ^ 
# actual

TP = 12
FN = 11
sensitivity = TP / (TP + FN)
sensitivity
TN = 167
FP = 12
specificity = TN / (TN + FP)
specificity

recordsCorrect = 167 + 12
totalRecords = 12 + 12 + 11 + 167
accuracyRate = recordsCorrect / totalRecords
accuracyRate	


#Baseline comparison: 
#  If we simply assume everyone is a non-violator:
recordsCorrect = 167 + 12
totalRecords = 12 + 12 + 11 + 167
accuracyRate = recordsCorrect / totalRecords
accuracyRate

#5.4

library(ROCR)
ROCRpred = prediction(pred, test$violator)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

#6

### PART 3 - PREDICTING LOAN REPAYMENT
setwd("C:/C/Education/edX MIT 15.071 - The Analytics Edge/Unit 03 Data Files")
getwd()

loans = read.csv("loans.csv")
summary(loans)
str(loans)

require("mice") || install.packages("mice")
library("mice")
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

imputed = read.csv("loans_imputed.csv")		#Load from downloaded file in case OS-differences change outcome

set.seed(144)
library(caTools)
split = sample.split(imputed$not.fully.paid, SplitRatio = 0.7)		#70% to training set
train = subset(imputed, split == TRUE)
test = subset(imputed, split == FALSE)

nrow(train) / (nrow(test) + nrow(train))

LoadLog1 = glm(not.fully.paid ~ ., data = train, family="binomial")
summary(LoadLog1)

cFICO = -9.317e-03 
dLogitA = cFICO * 700 
dLogitB =  cFICO * 710 
dLogitA - dLogitB		#Difference in log odds
dA = exp(dLogitA)
dB = exp(dLogitB)
dA / dB 			#Ratio of odds

pred = predict(LoadLog1, type="response", newdata=test)
table(test$not.fully.paid, pred > 0.5)

#   FALSE TRUE		<-- Predicted
# 0  2400   13
# 1   457    3
# ^ 
# actual

TP = 3
FN = 457
sensitivity = TP / (TP + FN)
sensitivity
TN = 2400
FP = 13
specificity = TN / (TN + FP)
specificity

recordsCorrect = 2400 + 3
totalRecords = 2400 + 3 + 13 + 457
accuracyRate = recordsCorrect / totalRecords
message("Accuracy Rate of Model is ", accuracyRate)

#Baseline is just predicting FALSE every time.
recordsCorrect = 2400 + 
totalRecords = 2400 + 3 + 13 + 457
accuracyRate = recordsCorrect / totalRecords
message("Baseline Model accuracy rate is ", accuracyRate)
