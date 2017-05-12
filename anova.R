######################################
#########  ANOVA ANALYSIS ############
######################################

#set environment 
script.dir <- dirname(sys.frame(1)$ofile)
wd <- c(paste0(script.dir[1],"/project_data/"))
setwd(wd)

#read data
home_data <- read.csv("home.txt",header = T)
away_data <- read.csv("away.txt",header = T)

##### Classify data with playe time (<30,30-33,33-37,>37)mins to <result>####
##### @HOME
tmp <- cbind(as.numeric(do.call(rbind,strsplit(as.vector(home_data[,"MP..."]),":",fixed = T))[,1]))
tmp <- cbind(tmp,as.numeric(c(home_data[,"FG"])))
ans <- c()
for(x in 1:(length(tmp)/2)){
  if(tmp[x] < 30){
        ans <- c(ans,"<30")
  }else if(tmp[x] <33){
        ans <- c(ans,"30-33")
  }else if(tmp[x] < 37){
        ans <- c(ans, "33-37")
  }else
        ans <- c(ans,">37")
}
tmp <- cbind(tmp,c(ans))
tmp <- cbind(tmp,c(rep("HOME")))
result <- data.frame(tmp)
colnames(result) = c("Min","FG","period","locate")

#### @AWAY
tmp <- cbind(as.numeric(do.call(rbind,strsplit(as.vector(away_data[,"MP..."]),":",fixed = T))[,1]))
tmp <- cbind(tmp,as.numeric(c(away_data[,"FG"])))
ans <- c()
for(x in 1:(length(tmp)/2)){
  if(tmp[x] < 30){
    ans <- c(ans,"<30")
  }else if(tmp[x] <33){
    ans <- c(ans,"30-33")
  }else if(tmp[x] < 37){
    ans <- c(ans, "33-37")
  }else
    ans <- c(ans,">37")
}
tmp <- cbind(tmp,c(ans))
tmp <- cbind(tmp,c(rep("AWAY")))
tmp <- data.frame(tmp)
colnames(tmp) = c("Min","FG","period","locate")
result <- rbind(result,tmp)


#Analyze <result> to FG% = FG/matches
x <- data.frame(FG=as.numeric(result[,2]),
                period=factor(rep(c(">37","33-37","30-33","<30",">37","33-37","30-33","<30"),times=c(6,10,8,4,7,11,7,4))),
                locate=factor(rep(c("HOME","AWAY"),times=c(28,29)))
)
p <- ">37"
sum <- 0
new <- rep(0,8)
j <- 1 
n <- 0
for(i in 1:57){
  state <- x[i,2]
  if(state != p){
    new[j] <- sum/n
    sum <- 0;
    p <- x[i,2]
    j <- j+1
    n <- 0
  }else{
    sum <- sum+x[i,1]
    n <- n+1;
  }
}
new[j] <- sum

#print output
y <- data.frame(FG=as.numeric(new),period=factor(rep(c(">37","33-37","30-33","<30"),2))
                ,locate=factor(rep(c("HOME","AWAY"),times=c(4,4))))
b <- aov(FG ~ period*locate, data = y)
summary(b)
