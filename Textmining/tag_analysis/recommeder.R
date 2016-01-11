library(RGA)
library("dplyr")
library("ggplot2")
require(scales)
library(forecast)
library(lubridate)
library(plyr)
library(tidyr)







yesterday <- as.Date(Sys.Date() - 1) # fin: ayer
startdate <- yesterday # inicio


#sacar solo los que han visto más de X tiempo
users <- try(get_ga(profile.id = "110348611", start.date = startdate, end.date = yesterday, metrics = "ga:productAddsToCart", dimensions = "ga:productSku,ga:dimension1,ga:dimension15", sort = "-ga:productAddsToCart", filters = "ga:dimension15!@(not set);ga:dimension1=~^GA", sampling.level = "faster", max.results = 1000), silent = F)
head(users)
names(users) <- c("product.sku", "client.id", "video.lenght", "product.adds.to.cart")
summary(users)

#escogemos aquellos usuarios que han visto más de 4 videos
#top.users <- users %>% group_by(client.id,product.sku ) %>% dplyr::summarize(count=n()) %>% filter(count > 1) %>% select(product.sku)


video.engagement <- try(get_ga(profile.id = "110348611", start.date = as.Date(Sys.Date() - 10), end.date = yesterday, metrics = "ga:uniqueEvents,ga:eventValue", dimensions = "ga:eventLabel", sort = "-ga:eventValue", filters = "ga:eventAction=@Time on screen"), silent = T)
head(video.engagement )
video.engagement %>% mutate(engament.time = event.value / unique.events) %>% select(event.label, engament.time) %>% arrange(desc(engament.time)) -> video.engagement.df
names(video.engagement.df) <- c("product.sku", "engament.time")
head(video.engagement.df)



users.all <- join_all(list(users,video.engagement.df), by='product.sku', type='left', match = "first")
head(users.all)


users.all <- users.all %>% filter(video.lenght != '(not set)') %>% select(product.sku, client.id, video.lenght, engament.time) %>% mutate( eng.ratio = engament.time / as.numeric(video.lenght) * 100)   %>% arrange(desc(eng.ratio))

head(users.all)
summary(users.all)


#### CORTE

video.mean = mean(users.all$eng.ratio, na.rm = T)

#en la consulta todos han visto video. Habría que extraer un segment con aquellos que no ha visto video y unirlo
videoEngaged <- function(x) ifelse(x > video.mean, 1, 0)
users.all[3] <- lapply(users.all[3], videoEngaged)
head(users.all, 20)
users.all$product.sku <- as.factor(users.all$product.sku)
users.all$eng.ratio <- as.integer(users.all$eng.ratio)
#### FIN CORTE



users.all <- users.all[complete.cases(users.all),]


users.all$quartile <- with(users.all, factor(
  findInterval( users.all$eng.ratio, c(-Inf,
                       quantile(users.all$eng.ratio, probs=c(0.25, .5, .75)), Inf)), 
  labels=c("1","2","3","4")
))

head(users.all, 20)

users.all$quartile <- as.integer(users.all$quartile)

str(users.all)





####  CORTE
library('reshape')
users.video.matrix <- cast(users.all, dimension1 ~ product.sku, value = 'eng.ratio', na.exclude = T)
users.video.matrix <- users.video.matrix %>% filter(users.video.matrix$dimension1 != "")
users.video.matrix[, 1]
users.video.matrix[2,]
#users.video.matrix[is.na(users.video.matrix)] <- 0
row.names(users.video.matrix) <- users.video.matrix[, 1]
#eliminar columna 1
users.video.matrix$dimension1 <- NULL

head(users.all)

#### FIN CORTE


library('reshape')


users.video.matrix <- cast(users.all, client.id ~ product.sku, value = 'quartile', na.exclude = T)
head(users.video.matrix)


######################################
#http://www.talkstats.com/showthread.php/24692-Count-non-missing-columns-for-each-row
#buscar cuantos videos ha visto cada cliente 
foo<- function(x) { 
  length(na.omit(x)) 
} 
test1<- data.frame(aggregate(users.video.matrix,by=list(users.video.matrix$client.id),FUN="foo")) # converts the data to binary (1= non missing, 0=missing) 
test1$counts<-rowSums (test1[,2:ncol(test1)], na.rm = FALSE, dims = 1) 
head(test1)

test1<-subset(test1,select=c(Group.1,counts))
colnames(test1)<-c("client.id","counts")
test<-merge(users.video.matrix,test1,by=c("client.id"))

hist(test$counts)
head(test)



test2 <- join_all(list(users.video.matrix,test), by='client.id', type='left', match = "all")
test2$counts

test3 <- test2 %>% arrange(desc(counts)) %>% filter(counts > 3)
######################################



row.names(users.video.matrix) <- users.video.matrix[, 1]
users.video.matrix$client.id <- NULL
tail(users.video.matrix)


#alt
#library(reshape2)
#users.video.matrix<-acast(users.all, client.id ~ product.sku)
#class(users.video.matrix)

#install.packages("recommenderlab")
library("recommenderlab")

users.video.mx <- data.matrix(users.video.matrix)
head(users.video.mx, 1)
r <- as(users.video.mx, "realRatingMatrix")
r

as(r, "list")
as(r, "matrix")
tail(as(r, "data.frame"))


r_m <- normalize(r)
r_m
ratings.norm.df <- as(r_m, "data.frame")
as(r_m, "list")
image(r, main = "Raw Ratings")
image(r_m, main = "Normalized Ratings")
image(sample(r, 50), main = "Raw ratings")

r_b <- binarize(r, minRating=3)
as(r_b, "matrix")


rowCounts(r[1,])
as(r[1,], "list")
rowMeans(r[1,])
hist(getRatings(r))
hist(getRatings(normalize(r)))

#hist(rowCounts(r), breaks=5)


dim(r)

rec=Recommender(r[1:500],method="POPULAR")
#rec=Recommender(r[1:5],method="UBCF")
#rec=Recommender(r[1:200],method="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=5, minRating=1))
rec=Recommender(r[1:500],method="IBCF", param=list(normalize = "Z-score",method="Jaccard",minRating=1))
rec=Recommender(r[1:500],method="IBCF")
print(rec)
names(getModel(rec))
getModel(rec)$topN
getModel(rec)$ratings


recommended.items.u1<- predict(rec, r["GA1.2.962287276.1423160815",], n=5)
as(recommended.items.u15348, "list")
recommended.items.u12 <- predict(rec, r["GA1.2.963892523.1449454878",], n=5)
as(recommended.items.u12, "list")

recom <- predict(rec, r[201:202], n=10)
recom

as(recom, "list")


ratings.df <- as(r, "data.frame")

users.video.matrix %>% select(pichaloca_es_7258) %>% na.omit()
#users.video.matrix %>% summarise (n=n())


qplot(getRatings(r), binwidth = 1, 
      main = "Histogram of ratings", xlab = "Rating")
qplot(colMeans(r), binwidth = 1, 
      main = "Mean rating of Movies", 
      xlab = "Rating", 
      ylab = "# of movies")

recom3 <- bestN(recom, n = 3)
recom3
as(recom3, "list")


#######################################################
dim(r)
rec=Recommender(r[1:25],method="POPULAR")
#rec=Recommender(r[1:400],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
#rec=Recommender(r[1:400],method="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=5, minRating=1))
#rec=Recommender(r[1:100],method="IBCF", param=list(normalize = "Z-score",method="Jaccard",minRating=1))
print(rec)
names(getModel(rec))
getModel(rec)$nn
getModel(rec)$topN


head(r, 1)
rowMeans(r[2,])
hist(getRatings(r), breaks=10)
hist(getRatings(normalize(r, method="Z-score")), breaks=10)

recom <- predict(rec, r[1:42], n=100)
recom
as(recom, "list")


recom3 <- bestN(recom, n = 3)
as(recom3, "list")

r.sampe <- sample(r, 50)
r.sampe




recom2 <- predict(rec, r[1:42], type="ratings")
as(recom2,"matrix")[,1:10]
as(recom2,"list")



recommenderRegistry$get_entries(dataType = "realRatingMatrix")


similarities <- cor(users.video.matrix)
nrow(similarities)
ncol(similarities)
similarities[2, 2]
similarities[1, 2]
distances <- -log((similarities / 2) + 0.5)

k.nearest.neighbors <- function(i, distances, k = 25)
{
  return(order(distances[i, ])[2:(k + 1)])
}

installation.probability <- function(user, package, users.video.matrix, distances, k = 25)
{
  neighbors <- k.nearest.neighbors(package, distances, k = k)
  
  return(mean(sapply(neighbors, function (neighbor) {users.video.matrix[user, neighbor]})))
}

distances["pichaloca_es_9922", ]
k.nearest.neighbors("pichaloca_es_9922", distances, k = 25)

installation.probability("GA1.2.94798120.1447303260", "pichaloca_es_9922", users.video.matrix, distances)




##############################################

video.engagementbyclientid <- try(get_ga(profile.id = "110348611", start.date = startdate, end.date = yesterday, metrics = "ga:uniqueEvents,ga:eventValue", dimensions = "ga:dimension1,ga:eventLabel", max.results = 1000, sort = "-ga:eventValue", filters = "ga:eventAction=@Time on screen"), silent = F)
head(video.engagementbyclientid )
video.engagementbyclientid %>% mutate(engament.time = event.value / unique.events) %>% select(dimension1, event.label, engament.time) %>% arrange(desc(engament.time)) -> video.engagementbyclientid
names(video.engagementbyclientid) <- c("client.id","product.sku", "engament.time")
head(video.engagementbyclientid)


library('reshape')
users.video.matrix <- cast(video.engagementbyclientid, client.id ~ product.sku, value = 'engament.time', na.exclude = T)
users.video.matrix <- users.video.matrix %>% filter(users.video.matrix$client.id != "")
users.video.matrix[, 1]
users.video.matrix[2,]
