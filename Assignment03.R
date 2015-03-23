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



