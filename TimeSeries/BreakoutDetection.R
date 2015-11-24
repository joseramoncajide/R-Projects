setwd("~/Documents/GitHub/R-Projects/TimeSeries")
#install.packages("devtools")
#devtools::install_github("twitter/BreakoutDetection")

library(BreakoutDetection)
library(lubridate)
library(zoo)

df <- read.csv2(file= "data/social.csv", sep = ";", stringsAsFactors = T, header = T, na.strings = F, dec=",")

str(df)
summary(df)



df$Fecha <- dmy(df$Fecha)

df[,2] <- sapply(df[,2], function(v) {as.numeric(gsub("\\.","", as.character(v)))})
df[,3] <- sapply(df[,3], function(v) {as.numeric(gsub("\\.","", as.character(v)))})


df$Likes <- as.numeric(df$Likes)
df$Followers <- as.numeric(df$Followers)


class(df$Likes)
class(df$Followers)
mean(df$Likes, na.rm = T)

#check !complete.cases(df)

#pago.df <- pago.df[!complete.cases(df),]
!complete.cases(df$Likes)
#Fill missing df with library(zoo)
df$Likes <- na.approx(df$Likes) 

#basic plot
plot(df$Followers)
plot(df$Likes)


#Diff
df$LikesDiff <- c(NA,diff(df$Likes))
df$FollowersDiff <- c(NA,diff(df$Followers))

#remove first row
df <- df[-1,]

help(breakout)

df.breakout <- df[,c(1,4)]
names(df.breakout) <- c("timestamp", "count")


res = breakout(df.breakout, min.size=4, method='multi', beta=.001, degree=1, plot=TRUE)
res$plot
