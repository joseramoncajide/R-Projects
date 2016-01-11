library(RGA)
library("dplyr")
library("ggplot2")
require(scales)
library(forecast)
library(lubridate)
library(plyr)
library(tidyr)


yesterday <- as.Date(Sys.Date() - 1) # fin: ayer
startdate.socialmetrics <- yesterday %m-% months(5) # inicio
startdate <- yesterday # inicio


#los hits deben consultarse sobre la vista de Backup que no tiene filtros y no deshecha hits: 110336561

videos <- try(get_ga(profile.id = "110348611", start.date = startdate, end.date = yesterday, metrics = "ga:productListViews,ga:productDetailViews,ga:productAddsToCart,ga:cartToDetailRate", dimensions = "ga:productSku,ga:dimension13,ga:dimension15,ga:dimension16", sort = "-ga:productAddsToCart", filters = "ga:dimension13!=(not set)", sampling.level = "faster"), silent = F)


#get_unsampled_report(account.id = "38248820", webproperty.id = "UA-38248820-1", profile.id = "110348611",                   unsampled.report.id, token)

head(videos)
#names(head(videos[,-1]))

videos -> videos.df

names(videos.df) <- c("product.sku", "tags", "lenght", "rating", "impressions","detail","play","playtodetailrate")
head(videos.df)

#interacciones sociales

social <- try(get_ga(profile.id = "110348611", start.date = startdate.socialmetrics, end.date = yesterday, metrics = "ga:uniqueSocialInteractions", dimensions = "ga:socialInteractionNetwork,ga:socialInteractionTarget", sort = "-ga:uniqueSocialInteractions", filters = "ga:pagePath=@video;ga:socialInteractionTarget!~^http"), silent = T)

head(social)

social.df <- social %>% filter(social.interaction.network != 'undefined' ) %>% spread(social.interaction.network, unique.social.interactions)
head(social.df)
tail(social.df)


social_metrics <- function(x) ifelse(is.na(x), 0, ifelse(x > 0, 1, 0))

#si ha sido compartido en rrss 1, si no 0
social.df[2:3] <- lapply(social.df[2:3], social_metrics)
tail(social.df)
names(social.df) <- c("product.sku","facebook.shared", "twitter.shared")


#timeonscreen

video.engagement <- try(get_ga(profile.id = "110348611", start.date = startdate, end.date = yesterday, metrics = "ga:uniqueEvents,ga:eventValue", dimensions = "ga:eventLabel", sort = "-ga:eventValue", filters = "ga:eventAction=@Time on screen"), silent = T)
head(video.engagement )

#el objetivo es tener los engagement time de cada video para cruzarlos con los tags
video.engagement %>% mutate(engament.time = event.value / unique.events) %>% select(event.label, engament.time) %>% arrange(desc(engament.time)) -> video.engagement.df
names(video.engagement.df) <- c("product.sku", "engament.time")
head(video.engagement.df)



video.engagement.classification <- join_all(list(videos.df,video.engagement.df), by='product.sku', type='left', match = "all")
video.engagement.classification <- video.engagement.classification %>% select(product.sku, lenght, engament.time) %>% mutate( eng.ratio = engament.time / lenght * 100)
head(video.engagement.classification)
summary(video.engagement.classification)

#econtrar valor extremo

which.max( video.engagement.classification[,4] )
video.engagement.classification[,4],decreasing=T)[1]
#valor extremo
extremo <- row.names(video.engagement.classification[order(video.engagement.classification[,4],decreasing=T)[1],])

eng.ratio.quantile <- quantile(video.engagement.classification$eng.ratio, probs=c(.25, .75), na.rm = T)

video.engagement.classification$quartile <- with(video.engagement.classification, cut(eng.ratio, 
                                breaks=quantile(eng.ratio, probs=seq(0,1, by=0.25), na.rm = T), 
                                include.lowest=TRUE))

video.engagement.classification <- video.engagement.classification %>% mutate(eng.factor = ntile(eng.ratio, 4)) %>% select(product.sku, eng.factor )
head(video.engagement.classification)


ifelse(sss, 0, )

#de los usuarios que buscan


#analizar kw url páginas de videos


para cada una de las columnas numéricas del data set... función devuelve términos frecuentes
names(videos.df)
c("impresiones", "clicks")

head(videos.df)
#seleccionamos el 5% superior
videos.detailview <- videos.df %>% group_by(dimension13) %>% filter(product.detail.views > 0 | product.adds.to.cart > 0) %>% summarise(product.sku , views = sum(product.detail.views))  %>% filter(views > quantile(views, prob = c(0.95))) %>% arrange(-views)
head(videos.detailview)

videos.detailview <-

   videos.df %>% group_by(dimension13) %>% filter(product.detail.views > 0 | product.adds.to.cart > 0) %>% filter(product.detail.views > quantile(product.detail.views, prob = c(0.95))) %>% arrange(-product.detail.views)

     
#videos.df %>% group_by(dimension13) %>% filter(product.detail.views > 0 | product.adds.to.cart > 0) %>% summarise(product.sku , views = sum(product.detail.views))  %>% filter(views > quantile(views, prob = c(0.95))) %>% arrange(-views)

#video.join.df <- join_all(list(videos.detailview,videos.df,video.engagement.df, social.df, video.engagement.classification), by='product.sku', type='left', match = "all")

video.engagement.classification
video.join.df <- join_all(list(videos.df,video.engagement.df, social.df, video.engagement.classification), by='product.sku', type='left', match = "all")

video.join.df <- join_all(list(videos.df,video.engagement.df, social.df, video.engagement.classification), by='product.sku', type='left', match = "all")
#colnames(video.join.df)[1] <- NULL
#video.join.df$dimension13.y <- NULL
video.join.df[,2] <- NULL
head(video.join.df)
#################################################################################

lmdata <- video.join.df %>% filter(engament.time < 12000 | lenght > 7000 ) %>% select(-(facebook.shared), -(twitter.shared))
head(lmdata)
lmdata <- lmdata[complete.cases(lmdata),]

plot(lmdata$engament.time, lmdata$lenght)
testlm <-  lm(play ~  rating + impressions + detail, data=lmdata)
summary(testlm)
require(foreign)
require(MASS)
summary(rr.bisquare <- rlm(play ~  rating + impressions + detail, data=lmdata))



biweights <- data.frame(product.sku = lmdata$product.sku, resid = rr.bisquare$resid, weight = rr.bisquare$w)
biweights2 <- biweights[order(rr.bisquare$w), ]
biweights2[1:15, ]


plot(testlm)

video.tmp <- video.join.df %>% select(product.sku, lenght, rating, impressions, detail, play, facebook.shared, twitter.shared, eng.factor)
video.tmp[7:8] <- lapply(video.tmp[7:8], social_metrics)
head(video.tmp)
class(video.tmp$eng.factor)

video.tmp$eng.factor <- ifelse(is.na(video.tmp$eng.factor), 0, ifelse(video.tmp$eng.factor > 2, 1, 0))
head(video.tmp)
video.tmp$eng.factor <- as.factor(video.tmp$eng.factor)
levels(video.tmp$eng.factor)

mylogit <- glm(eng.factor ~ lenght + rating + +impressions + detail + play, data = video.tmp, family = "binomial")
summary(mylogit)
confint(mylogit)

wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)


library(party)
video.tmp.data <- video.tmp %>% select(-(product.sku))
kk <- ctree(eng.factor ~ lenght + rating + play + facebook.shared + twitter.shared, data = na.omit(video.tmp.data))
plot(kk)

library(rpart)
fit = rpart(eng.factor ~ lenght + rating + play + facebook.shared + twitter.shared, method="class", data=video.tmp.data)
printcp(fit)
plotcp(fit)
summary(fit)


plot(fit, uniform=TRUE, main="Classification Tree for Chemicals")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
table(subset(video.tmp.data, play>=0.5)$eng.factor)

video.tmp.data$play
library(randomForest)
fit <- randomForest(eng.factor ~ lenght + rating + impressions,   data = (video.tmp %>% select(-(product.sku))))
print(fit) # view results 
importance(fit) # importance of each predictor


Objetivo de conversión engament.time > xxx
facebook.shares SI|NO
twitter.shares SI|NO 
video duración POR TRAMOS
video votos POR TRAMOS

###CORRE


metrics <- select(video.join.df, -(dimension13),-(product.sku),-(shares), -(twitter.shares))
head(metrics)
kk <- as.data.frame(scale(metrics))
plot(kk)

#ESTO SOLO PARA EL TEXT MINING
#ESTO NO HACE FALTA HACERLO

#install.packages("splitstackshape")
library(splitstackshape)
videos.join.df.long <- cSplit(video.join.df, "dimension13", sep = "|", direction = "long")
head(videos.join.df.long)
names(head(videos.join.df.long))

library(tm)

all_tags <- paste(video.join.df$dimension13, collapse=" ")
class(all_tags)
corpus <- Corpus(VectorSource(all_tags))
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))


dtm <- TermDocumentMatrix(corpus)

findFreqTerms(dtm, 10)
findAssocs(dtm, "dormido", corlimit=0.15)
#cruzar con las categorias más vistas


dtmss <- removeSparseTerms(dtm, 0.15) 
inspect(dtmss)   

library(cluster)   
d <- dist(dtmss, method="euclidian")   
fit <- hclust(d, method="ward.D2")   
fit   
plot(fit, hang=-1) 

library(fpc)   
d <- dist(dtm, method="euclidian")   
kfit <- kmeans(d, 3)   
#clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 
clusplot(as.matrix(d), diss=TRUE, kfit$cluster, color=TRUE, shade = TRUE, lines=0)
class(d)

mymeans <- kmeans(dtm,5)
mymeans
summary(mymeans)
m2 <- as.matrix(dtm)
dm <- dist(scale(m2))
fit <- hclust(dm, method="ward.D2")
plot(fit)
plot(dtm, col=mymeans$cluster)


m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="DocumentTermMatrix.csv") 


dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space, maximum.   
inspect(dtmss)   
###





#metricas de video
#top 10 keywords por impresiones
#top 10 keywords por páginas de detalle
#top 10 keywords por reproducciones
#top 10 keywords por playtodetail rate
#top 10 keywords por video rating
#top 10 keywords por redes sociales
#top 10 keywords por timeonscreen
#metricas de página
#og:description
#pageTitle
#keywords url