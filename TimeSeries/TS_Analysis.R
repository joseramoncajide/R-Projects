# Header 1
setwd("~/Documents/GitHub/R-Projects/TimeSeries")
library(lubridate)
library(zoo)
library(BreakoutDetection)
library(AnomalyDetection)
library(CausalImpact)

```{r, echo=FALSE}
df <- read.csv2(file= "data/social.csv", sep = ";", stringsAsFactors = T, header = T, na.strings = F, dec=",")
```
#str(df)
#summary(df)

df$Fecha <- dmy(df$Fecha)
df[,2] <- sapply(df[,2], function(v) {as.numeric(gsub("\\.","", as.character(v)))})
df[,3] <- sapply(df[,3], function(v) {as.numeric(gsub("\\.","", as.character(v)))})


df$Likes <- as.numeric(df$Likes)
df$Followers <- as.numeric(df$Followers)


#class(df$Likes)
#class(df$Followers)
#mean(df$Likes, na.rm = T)

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

#help(breakout)

#likes
df.likes <- df[,c(1,4)]

#followers
df.followers <- df[,c(1,5)]


df.breakout <- df.followers

names(df.breakout) <- c("timestamp", "count")

res.b = breakout(df.breakout, min.size=4, method='multi', beta=.001, degree=1, plot=TRUE)
res.b$plot

res.a = AnomalyDetectionTs(df.breakout, max_anoms=0.02, direction='both', plot=TRUE, title = "Anomalías detectadas en la serie temporal")
res.a$plot
res.a$anoms







ts <- ts(df.followers$FollowersDiff, frequency = 365,  start=c(2015, yday("2015-12-10")))

#pacf(pago.transacciones.df$Transacciones)
#einfach.ts <- ts(einfach.hits.df$hits,  frequency=365 ,start=c(2014, yday("2014-10-09")))

#pago.transacciones.ts <- ts(pago.transacciones.df$Transacciones, frequency = 365,  start=c(2014,1))
start(ts) 
end(ts) 


#library(tseries)
#adf.test(ts)
#no es estacionaria http://rstatistics.net/time-series-analysis/
plot(ts, xaxt="n", main="Ingresos de Reservas", sub="Serie temporal", xlab="", ylab="Importe en EUR")
a = seq(as.Date("2015-12-10"), by="days", length=80)
axis(1, at = decimal_date(a), labels = format(a, "%b %Y"), cex.axis=0.6)
abline(v = decimal_date(a), col='grey', lwd=0.5)
time <- time(ts)
abline(reg=lm(ts~ time) , col= "red")


#Facebook Likes

pre.period <- as.Date(c("2015-10-12", "2015-11-20"))
post.period <- as.Date(c("2015-11-21", "2015-11-24"))

impact <- CausalImpact(df.likes, pre.period, post.period)
plot(impact)
plot(impact, "original")
summary(impact)
summary(impact, "report")



