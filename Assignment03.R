#Assignment 3
setwd("C:/C/Education/edX MIT 15.071 - The Analytics Edge/Unit 03 Data Files")
getwd()

# Problem 1
songs = read.csv("songs.csv")
nrow(subset(songs, year==2010)) #373

nrow(subset(songs, artistname=="Michael Jackson")) #18

subset(songs, artistname=="Michael Jackson" & Top10==1)$songtitle

unique(songs$timesignature)
table(songs$timesignature)

subset(songs, tempo == max(songs$tempo))$songtitle	#Wanna Be Startin' Somethin'

# Problem 2
