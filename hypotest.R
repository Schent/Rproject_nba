#Hypothosis test proportion
## test different team's FG% between 2016 & 2017

#set environment 
script.dir <- dirname(sys.frame(1)$ofile)
wd <- c(paste0(script.dir[1],"/project_data/"))
setwd(wd)

data_team17 <- read.table("team17.txt",header = T)
data_team16 <- read.table("team16.txt",header = T)

#calculate FG%
x <- c(data_team16[,"FGM"],data_team16[,"FGA"])
y <- c(data_team17[,"FGM"],data_team17[,"FGA"])
p1 = y[1]/y[2] #2017
p2 = x[1]/x[2] #2016

#test statistic
phat = (x[1]+y[1])/(x[2]+y[2])
z = (p1-p2)/sqrt(phat*(1-phat)* ((1/x[2])+(1/y[2])))
alpha = 0.01
z.alpha = qnorm(1-alpha)

#find reject area
if(z > z.alpha) print("Reject Null Hypothesis")
print(z)
print(z.alpha)
