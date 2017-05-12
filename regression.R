########################
### REGRESSION #########
########################
#set environment 
script.dir <- dirname(sys.frame(1)$ofile)
wd <- c(paste0(script.dir[1],"/project_data/"))
setwd(wd)

#read data
curry_data <- read.csv("curry.txt",header = T, sep = ",")
thompson_data <- read.csv("thompson.txt",header = T, sep = ",")
durant_data <- read.csv("durant.txt",header = T, sep =",")
green_data <- read.csv("green.txt",header = T, sep = ",")

#read FG & FGA
curry_FG <- as.numeric(as.vector(curry_data[,"FG"]))
curry_FG[is.na(curry_FG)] <- 0
curry_FGA <- as.numeric(as.vector(curry_data[,"FGA"]))
curry_FGA[is.na(curry_FGA)] <- 0

thompson_FG <- as.numeric(as.vector(thompson_data[,"FG"]))
thompson_FG[is.na(thompson_FG)] <- 0
thompson_FGA <- as.numeric(as.vector(thompson_data[,"FGA"]))
thompson_FGA[is.na(thompson_FGA)] <- 0

durant_FG <- as.numeric(as.vector(durant_data[,"FG"]))
durant_FG[is.na(durant_FG)] <- 0
durant_FGA <- as.numeric(as.vector(durant_data[,"FGA"]))
durant_FGA[is.na(durant_FGA)] <- 0

#print output & plot
model <- lm(curry_FG ~ curry_FGA)
plot(curry_FGA,curry_FG,col='green')
abline(model)
summary(model)

