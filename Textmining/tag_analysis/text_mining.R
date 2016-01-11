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

videos <- try(get_ga(profile.id = "110348611", start.date = startdate, end.date = yesterday, metrics = "ga:productListViews,ga:productDetailViews,ga:productAddsToCart,ga:cartToDetailRate", dimensions = "ga:productSku,ga:dimension13,ga:dimension15,ga:dimension16,ga:productName", sort = "-ga:productAddsToCart", filters = "ga:dimension13!=(not set)", sampling.level = "faster"), silent = F)


#get_unsampled_report(account.id = "38248820", webproperty.id = "UA-38248820-1", profile.id = "110348611",                   unsampled.report.id, token)

head(videos)
#names(head(videos[,-1]))

videos -> videos.df

names(videos.df) <- c("product.sku", "tags", "lenght", "rating", "product.name", "impressions","detail","play","playtodetailrate")
head(videos.df)




#timeonscreen

video.engagement <- try(get_ga(profile.id = "110348611", start.date = startdate, end.date = yesterday, metrics = "ga:uniqueEvents,ga:eventValue", dimensions = "ga:eventLabel", sort = "-ga:eventValue", filters = "ga:eventAction=@Time on screen"), silent = T)
head(video.engagement)

#el objetivo es tener los engagement time de cada video para cruzarlos con los tags
video.engagement %>% mutate(engament.time = event.value / unique.events) %>% select(event.label, engament.time) %>% arrange(desc(engament.time)) -> video.engagement.df
names(video.engagement.df) <- c("product.sku", "engament.time")
head(video.engagement.df)



video.engagement.classification <- join_all(list(videos.df,video.engagement.df), by='product.sku', type='left', match = "all")
#video.engagement.classification <- video.engagement.classification %>% select(product.sku, lenght, engament.time) %>% mutate( eng.ratio = engament.time / lenght * 100) %>% arrange(-(eng.ratio))
video.engagement.classification <- video.engagement.classification %>% mutate( eng.ratio = engament.time / lenght * 100) %>% arrange(-(eng.ratio))
head(video.engagement.classification)
summary(video.engagement.classification)


video.join.df <- video.engagement.classification %>%  arrange(-play)
head(video.join.df)

#TOP 25% videos con play
video.join.df <-  video.engagement.classification %>%  filter(play > quantile(play, 0.25)) %>% arrange(-play)
head(video.join.df)


#TOP 25% videos con play
video.top.play <-  video.engagement.classification %>%  filter(play > quantile(play, 0.25)) %>% arrange(-play)
head(video.top.play)

#TOP 25% videos con engagement
video.top.engagementratio <-  video.engagement.classification %>% na.omit() %>%  filter(eng.ratio > quantile(eng.ratio, 0.25)) %>% arrange(-eng.ratio)
head(video.top.engagementratio)

#TOP 25% videos por ratio
video.top.rating <-  video.engagement.classification %>%  filter(rating > quantile(rating, 0.25)) %>% arrange(-rating)
head(video.top.rating)

#SWITCH
video.join.df <- video.top.rating





#kk <- videos.df %>%  select(rating, product.sku)
#head(kk)

#as.data.frame(xtabs(rating ~ product.sku,data="kk"))




#ggplot(video.join.df, aes(play)) + geom_density()

#boxplot(video.join.df$play,data=video.join.df)

#boxplot.stats(video.join.df$play)$out


#video.join.df <- video.join.df[!video.join.df %in% boxplot.stats(video.join.df$play)$out]


#################################################################################



library(tm)

#all_tags <- paste(video.join.df$tags, collapse=" ")

corpus <- Corpus(VectorSource(video.join.df$tags))
dtm <- TermDocumentMatrix(corpus)

freq <- colSums(as.matrix(dtm))   
length(freq)   
findFreqTerms(dtm, 100)
findAssocs(dtm2, "abdominales", corlimit=0.15)





wf <- data.frame(word=names(freq), freq=freq)   
head(wf)  


##
corpus <- Corpus(VectorSource(video.join.df$product.name)) #product.name o tag
corpus = tm_map(corpus, removeWords, c(stopwords("spanish"), "vs."))
inspect(corpus)
#when creating the term document matrix, i use the 'global bounds' argument to limit the number of terms, which seems also to be able to filter low-frequency terms https://www.kaggle.com/c/job-salary-prediction/forums/t/4076/large-scale-text-mining-in-r/80253
#tdm <- TermDocumentMatrix(corpus, control=list(bounds=list(global=c(floor(length(corpus)*0.05), Inf))))

tdm <- TermDocumentMatrix(corpus)
inspect(tdm)
(freq.terms <- findFreqTerms(tdm, lowfreq = 100))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 100)

df <- data.frame(term = names(term.freq), freq = term.freq)
library(ggplot2)

ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()
findAssocs(tdm, "piscina", 0.2)

findAssocs(tdm, "adolescente", corlimit=0.15)

#terminos relacionados
piscina_df <- data.frame(findAssocs(dtm, c("piscina"), corlimit=0.2))





library(graph)

library(Rgraphviz)



plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)

findFreqTerms(tdm, lowfreq = 10)
#get the top N words in order of their abundance
m <- as.matrix(tdm)
tag.popularity <- sort(rowSums(m), decreasing=TRUE)
head(tag.popularity, 30)

recipes.m = as.matrix(tdm)
popularity.of.ingredients = sort(colSums(recipes.m), decreasing=TRUE)
tag.popularity = data.frame(tag = names(tag.popularity), plays=tag.popularity)
tag.popularity$tag = reorder(tag.popularity$tag, tag.popularity$plays)

ggplot(tag.popularity[1:30,], aes(x=tag, y=plays)) + geom_point(size=5, colour="red") + coord_flip() +
  ggtitle("Top 30 Tags") + 
  theme(axis.text.x=element_text(size=13,face="bold", colour="black"), axis.text.y=element_text(size=13,colour="black",
                                                                                                face="bold"), axis.title.x=element_text(size=14, face="bold"), axis.title.y=element_text(size=14,face="bold"),
        plot.title=element_text(size=24,face="bold"))

+ labs(title = "MAIN TITLE")



#hacer un documento por cada consulta de videos por impresiones, clicks, detalle, etc. y comparar los términos más frecuentes
#http://stackoverflow.com/questions/15506118/make-dataframe-of-top-n-frequent-terms-for-multiple-corpora-using-tm-package-in

# remove sparse terms

tdm2 <- removeSparseTerms(tdm, sparse = 0.99)

m2 <- as.matrix(tdm2)

# cluster terms

distMatrix <- dist(scale(m2))

fit <- hclust(distMatrix, method = "ward.D2")
plot(fit)
rect.hclust(fit, k = 6) # cut tree into 6 clusters research position university analysis network social


m3 <- t(m2) # transpose the matrix to cluster documents (tweets)

set.seed(122) # set a fixed random seed

k <- 6 # number of clusters

kmeansResult <- kmeans(m3, k)

round(kmeansResult$centers, digits = 3)



for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep = ""))
  
  s <- sort(kmeansResult$centers[i, ], decreasing = T)
  
  cat(names(s)[1:5], "\n")
}

dtm <- as.DocumentTermMatrix(tdm)
library(topicmodels)
lda <- LDA(dtm, k = 8)
(term <- terms(lda, 6))
term <- apply(term, MARGIN = 2, paste, collapse = ", ")
topic <- topics(lda, 1)
topics <- data.frame(date=as.IDate(tweets.df$created), topic)

#http://rforwork.info/2014/02/17/a-delicious-analysis/
recipes.m = as.matrix(dtm)
popularity.of.ingredients = sort(colSums(recipes.m), decreasing=TRUE)
popularity.of.ingredients = data.frame(ingredients = names(popularity.of.ingredients), num_recipes=popularity.of.ingredients)
popularity.of.ingredients$ingredients = reorder(popularity.of.ingredients$ingredients, popularity.of.ingredients$num_recipes)

ggplot(popularity.of.ingredients[1:30,], aes(x=ingredients, y=num_recipes)) + geom_point(size=5, colour="red") + coord_flip() +
  ggtitle("Recipe Popularity of Top 30 Ingredients") + 
  theme(axis.text.x=element_text(size=13,face="bold", colour="black"), axis.text.y=element_text(size=13,colour="black",
                                                                                                face="bold"), axis.title.x=element_text(size=14, face="bold"), axis.title.y=element_text(size=14,face="bold"),
        plot.title=element_text(size=24,face="bold"))

recipes.lda = LDA(dtm, 50)
t = terms(recipes.lda,5)


library(glmnet)
#install.packages("caret")
library(caret)
mymodel <- cv.glmnet(y=log(video.join.df$play), x=as.matrix(dtm), family = "gaussian", nfolds = 10, alpha = 1 )
plot(mymodel)
relevant <- predict(mymodel, s="lambda.min" , type="coefficients")[,1]
relevant <- relevant[relevant != 0]
relevant <- sort(relevant)
relevant <- tail(relevant[setdiff(names(relevant), "(Intercept)")], 25)
par(las = 2, mar=c(5,7,4,2) + 0.1)


barplot(relevant, horiz = T, cex.names = 0.75, col = "lightblue", main="ssds")
  
  
  ###################
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