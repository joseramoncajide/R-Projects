rm(list = ls());gc()

paquetes <- c("rmarkdown","magrittr")

if( any(!paquetes %in% rownames(installed.packages())) ){
  install.packages(paquetes[!paquetes %in% rownames(installed.packages())])
}

library(RGA)
library("dplyr")
library("ggplot2")
#require(scales)
#library(forecast)
#library(lubridate)
library(plyr)
library(tidyr)
library(tm)
library(dplyr)

yesterday <- as.Date(Sys.Date() - 1) # fin: ayer
#startdate <- yesterday # inicio
startdate <- as.Date(Sys.Date() - 10) # inicio

#authorize(new.auth = TRUE)


video.categories.metrics.df <- try(
  get_ga(profile.id = "110348611", 
         start.date = "2daysAgo", 
         end.date = "yesterday", 
         metrics = "ga:productDetailViews,ga:productAddsToCart,ga:cartToDetailRate", 
         dimensions = "ga:date,ga:productSku,ga:productName,ga:productCategoryHierarchy", 
         sort = "-ga:productAddsToCart", 
         filters = "ga:productCategoryHierarchy!@(not set)", 
         sampling.level = "faster"), 
  silent = F)

names(video.categories.metrics.df) <- c(
  "date",
  "product.sku", 
  "product.name", 
  "product.category", 
  "detail", 
  "play",
  "playtodetailrate")
head(video.categories.metrics.df)

video.categories.metrics.df
test <- tbl_df(video.categories.metrics.df)


video.pornstar.metrics.df <- try(
  get_ga(profile.id = "110348611", 
         start.date = "2daysAgo", 
         end.date = "yesterday", 
         metrics = "ga:productListViews,ga:productDetailViews,ga:productAddsToCart,ga:cartToDetailRate", 
         dimensions = "ga:date,ga:productSku,ga:dimension17,ga:dimension15,ga:productName", 
         sort = "-ga:productAddsToCart", 
         filters = "ga:dimension17!=(not set)", 
         sampling.level = "faster"), 
  silent = F)

names(video.pornstar.metrics.df) <- c(
  "date",
  "product.sku", 
  "porn.star", 
  "lenght", 
  "product.name", 
  "impressions",
  "detail","play",
  "playtodetailrate")
head(video.pornstar.metrics.df)


#filtrae por productAddsToCart >= 0

 
         video.interaction.metrics.df <- try(
           get_ga(profile.id = "110348611", 
                  start.date = "2daysAgo", 
                  end.date = "yesterday", 
                  metrics = "ga:productListViews,ga:productDetailViews,ga:productAddsToCart,ga:cartToDetailRate", 
                  dimensions = "ga:date,ga:productSku,ga:dimension13,ga:dimension15,ga:dimension16,ga:productName", 
                  sort = "-ga:productAddsToCart", 
                  filters = "ga:dimension13!=(not set)", 
                  sampling.level = "HIGHER_PRECISION"), 
           silent = F)
         
         names(video.interaction.metrics.df) <- c(
           "date",
           "product.sku", 
           "tags", 
           "lenght", 
           "rating", 
           "product.name", 
           "impressions",
           "detail","play",
           "playtodetailrate")
         summary(video.interaction.metrics.df)



#interacciones sociales : OJO AL FILTRO

video.social.metrics.df <- try(
  get_ga(profile.id = "110348611", 
         start.date = "2daysAgo", 
         end.date = "yesterday", 
         metrics = "ga:uniqueSocialInteractions", 
         dimensions = "ga:socialInteractionNetwork,ga:socialInteractionTarget", 
         sort = "-ga:uniqueSocialInteractions", 
         filters = "ga:socialInteractionNetwork!=undefined;ga:pagePath=@video;ga:socialInteractionTarget!~^http", 
         sampling.level = "faster"), 
  silent = F)

head(video.social.metrics.df)

names(video.social.metrics.df) <- c(
  "social.interaction.network",
  "product.sku", 
  "social.interactions")
head(video.social.metrics.df)




#LOS MAS VISTOS POR CATEGORIA

#Elimnamos los videos que no han sido reproducidos



video.engagement.metrics.df <- try(
  get_ga(profile.id = "110348611", 
         start.date = "7daysAgo", 
         end.date = "yesterday", 
         metrics = "ga:uniqueEvents,ga:eventValue", 
         dimensions = "ga:eventLabel", 
         sort = "-ga:eventValue", 
         filters = "ga:eventAction=@Time on screen"), 
  silent = F)

head(video.engagement.metrics.df)

#el objetivo es tener los engagement time de cada video para cruzarlos con los tags
video.engagement.metrics.df <- video.engagement.metrics.df %>% 
  filter(grepl("^[a-zA-Z]{1,}_[a-z]{2}_[0-9]{4,5}$", event.label)) %>% 
  mutate(engament.time = event.value / unique.events) %>% 
  select(event.label, engament.time) %>% 
  arrange(desc(engament.time)) 

names(video.engagement.metrics.df) <- c("product.sku", "engament.time")

head(video.engagement.metrics.df)

video.df <- join_all(
  list(video.interaction.metrics.df,video.engagement.metrics.df),
  by='product.sku',
  type='left',
  match = "all")

#elimnamos los vídeos que no han generado engagement time
video.df <- video.df %>%  filter(!is.na((engament.time)))

summary(video.df)


#videos con mayor play
video.df <- video.df %>%
  filter(play > quantile(play, 0.9)) %>% 
  mutate( eng.ratio = engament.time / lenght * 100) %>% 
  filter(eng.ratio < 100) %>% 
  arrange(-(eng.ratio)) 


head(video.df)



video.pornstar.df <- join_all(
  list(video.pornstar.metrics.df,video.engagement.metrics.df),
  by='product.sku',
  type='left',
  match = "all")
head(video.pornstar.df)

video.pornstar.df <- video.pornstar.df %>%
  filter(play > quantile(play, 0.9)) %>% 
  mutate( eng.ratio = engament.time / lenght * 100) %>% 
  filter(eng.ratio < 100) %>% 
  arrange(-(eng.ratio)) 

#listados

#LOS MAS VISTOS: TOP PLAYS
#detach(package:plyr)
head(video.interaction.metrics.df)
top_plays.df  <- filter(video.interaction.metrics.df, grepl("^[a-zA-Z]{1,}_[a-z]{2}_[0-9]{4,5}$", product.sku)) %>%  group_by(product.sku, product.name) %>% dplyr::summarise(avg.play = mean(play, na.rm=T))  %>% ungroup() %>% arrange(desc(avg.play)) %>% top_n(20, wt = avg.play)
head(top_plays.df)

tmp_reorder_v1 <- with(top_plays.df,reorder(top_plays.df$product.name, top_plays.df$avg.play))
v1 <- ggplot(top_plays.df, aes(x=tmp_reorder_v1, y=avg.play, fill=factor(product.name))) + geom_bar(stat = "identity") + coord_flip()  + theme_bw(base_size = 12, base_family = "Helvetica") + theme(legend.position="none")

#TOP ENGAMENT
summary()
top_engament.df <- video.df %>% filter(grepl("^[a-zA-Z]{1,}_[a-z]{2}_[0-9]{4,5}$", product.sku)) %>% select(product.sku, product.name, play, eng.ratio) %>%  group_by(product.sku, product.name) %>%  dplyr::summarise(avg.eng.ratio = mean(eng.ratio, na.rm=T)) %>% ungroup() %>%  dplyr::arrange(desc(avg.eng.ratio)) %>% top_n(20, wt = avg.eng.ratio)
head(top_engament.df)

tmp_reorder_v2 <- with(top_engament.df,reorder(top_engament.df$product.name, top_engament.df$avg.eng.ratio))
v2 <- ggplot(top_engament.df, aes(x=tmp_reorder_v2, y=avg.eng.ratio, fill=factor(product.name))) + geom_bar(stat = "identity") + coord_flip()  + theme_bw(base_size = 12, base_family = "Helvetica") + theme(legend.position="none")

#LOS MAS VISTOS: TOP PLAYS POR ACTRIZ
library(splitstackshape)
top_pornstar_plays.df <- video.pornstar.metrics.df %>% filter(grepl("^[a-zA-Z]{1,}_[a-z]{2}_[0-9]{4,5}$", product.sku)) %>% select(product.sku, product.name,porn.star, play) %>% cSplit("porn.star", sep = "|", direction = "long") %>%  group_by(porn.star) %>%  dplyr::summarise(plays = mean(play, na.rm=T)) %>% ungroup() %>%  dplyr::arrange(desc(plays)) %>% top_n(20, wt = plays)

head(top_pornstar_plays.df)


tmp_reorder_p1 <- with(top_pornstar_plays.df,reorder(top_pornstar_plays.df$porn.star, top_pornstar_plays.df$plays))
p1 <- ggplot(top_pornstar_plays.df, aes(x=tmp_reorder_p1, y=plays, fill=factor(porn.star))) + geom_bar(stat = "identity") + coord_flip()  + theme_bw(base_size = 12, base_family = "Helvetica") + theme(legend.position="none")

top_pornstar_engament.df <- video.pornstar.df %>% filter(grepl("^[a-zA-Z]{1,}_[a-z]{2}_[0-9]{4,5}$", product.sku)) %>% select(product.sku, product.name,porn.star, play, eng.ratio) %>% cSplit("porn.star", sep = "|", direction = "long") %>%  group_by(porn.star) %>%  dplyr::summarise(avg.eng.ratio = mean(eng.ratio, na.rm=T)) %>% ungroup() %>%  dplyr::arrange(desc(avg.eng.ratio)) %>% top_n(20, wt = avg.eng.ratio)

head(top_pornstar_engament.df)


tmp_reorder_p2 <- with(top_pornstar_engament.df,reorder(top_pornstar_engament.df$porn.star, top_pornstar_engament.df$avg.eng.ratio))
p2 <- ggplot(top_pornstar_engament.df, aes(x=tmp_reorder_p2, y=avg.eng.ratio, fill=factor(porn.star))) + geom_bar(stat = "identity") + coord_flip()  + theme_bw(base_size = 12, base_family = "Helvetica") + theme(legend.position="none")


#TOP 10 VIDEOS POR CATEGORIA
head(video.categories.metrics.df)

top_n_by_category.df <- video.categories.metrics.df %>% filter(grepl("^[a-zA-Z]{1,}_[a-z]{2}_[0-9]{4,5}$", product.sku)) %>% select(product.sku, product.name,product.category, play) %>% cSplit("product.category", sep = "|", direction = "long") %>% group_by(product.category, product.sku, product.name) %>%  dplyr::summarise(avg.play = mean(play, na.rm=T)) %>% group_by(product.category) %>% top_n(5, wt = avg.play) %>% dplyr::arrange(desc(avg.play)) 

head(top_n_by_category.df)
tail(top_n_by_category.df)
#http://stackoverflow.com/questions/6644997/showing-data-values-on-stacked-bar-chart-in-ggplot2
Data <- ddply(top_n_by_category.df, .(product.category), 
              transform, pos = cumsum(avg.play) - (0.5 * avg.play)
)

ggplot(Data, aes(x=product.category, y=avg.play, fill=factor(product.name))) + geom_bar(stat = "identity") + coord_flip()  + theme_bw(base_size = 12, base_family = "Helvetica") + geom_text(aes(label = product.sku ,y= pos), size = 3) + theme(legend.position="none")   

c1 <- ggplot(top_n_by_category.df, aes(x=product.category, y=avg.play, fill=factor(product.name))) + geom_bar(stat = "identity") + coord_flip()  + theme_bw(base_size = 12, base_family = "Helvetica") + geom_text(aes(label = product.sku), size = 3) + theme(legend.position="none")   


c2 <- ggplot(top_n_by_category.df, aes(x=factor(product.sku), y=avg.play, fill = factor(product.category))) + geom_bar(stat = "identity")  +  theme_bw(base_size = 12, base_family = "Helvetica")  + theme(legend.position="none", axis.text=element_text(size=8)) + facet_wrap(~ product.category, scales="free_x", ncol=4) 


#TOP CATEGORIAS

#LOS MAS VOTADOS
#detach(package:plyr)
head(video.interaction.metrics.df)
top_rating.df  <- video.interaction.metrics.df %>% filter(grepl("^[a-zA-Z]{1,}_[a-z]{2}_[0-9]{4,5}$", product.sku)) %>%  group_by(product.sku, product.name) %>% dplyr::summarise(avg.rating = mean(rating, na.rm=T))  %>% ungroup() %>% arrange(desc(avg.rating)) %>% top_n(20, wt = avg.rating)
head(top_rating.df)

tmp_reorder_r1 <- with(top_rating.df,reorder(top_rating.df$product.name, top_rating.df$avg.rating))
v1 <- ggplot(top_rating.df, aes(x=tmp_reorder_r1, y=avg.rating, fill=factor(product.name))) + geom_bar(stat = "identity") + coord_flip()  + theme_bw(base_size = 12, base_family = "Helvetica") + theme(legend.position="none")


#LOS MAS COMPARTIDOS




#LOS MAS COMPARTIDOS

#top_shares.df <- video.social.metrics.df %>% group_by(product.sku) %>% dplyr::summarise(shares = sum(social.interactions, na.rm=T)) %>% ungroup() %>% arrange(desc(shares)) 

top_shares.df <- join_all(list(video.social.metrics.df, video.categories.metrics.df), by = "product.sku", type = "left", match = "first") %>%  select(product.sku, product.name, social.interactions, social.interaction.network) %>% group_by(product.sku, product.name) %>% dplyr::summarise(shares = sum(social.interactions, na.rm=T)) %>% ungroup()  %>% arrange(desc(shares)) %>% top_n(20, wt = shares)
head(top_shares.df) 


tmp_reorder_s1 <- with(top_shares.df,reorder(top_shares.df$product.sku, top_shares.df$shares))
s1 <- ggplot(top_shares.df, aes(x=tmp_reorder_s1, y=shares, fill=factor(product.sku))) + geom_bar(stat = "identity") + coord_flip()  + theme_bw(base_size = 12, base_family = "Helvetica") + theme(legend.position="none")   



multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(v1, v2, cols=2)
multiplot(p1, p2, cols=2)





### RANk Y ORDENAR
#Seleccionamos el top 10% de los vídeos
video.by.eng.ratio.top <-  video.df %>% na.omit() %>%  filter(eng.ratio > quantile(eng.ratio, 0.90)) %>% arrange(-eng.ratio) 
head(video.by.eng.ratio.top)
dim(video.by.eng.ratio.top)


video.by.eng.ratio.mid <- video.df %>% na.omit() %>% filter(eng.ratio < quantile(eng.ratio, 0.90), eng.ratio > quantile(eng.ratio, 0.50)) %>% arrange(-eng.ratio) 
dim(video.by.eng.ratio.mid )

#DROP TEST: Videos con bajo eng.ratio
dim(video.df %>% na.omit() %>%  filter(eng.ratio < quantile(eng.ratio, 0.50)) %>% arrange(-eng.ratio) )

summary(video.by.eng.ratio.mid)
summary(video.by.eng.ratio.top)

#switch
videos.df <- video.by.eng.ratio.top
head(videos.df)



corpus <- Corpus(VectorSource(c(videos.df$product.name, videos.df$tags))) #product.name o tag
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, c(stopwords("spanish"), "vs.","|"))
remove_sep <- function(x) gsub("|", "", x)
### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE) 
corpus <- tm_map(corpus, content_transformer(remove_sep))  
inspect(corpus)[0]

#for (i in c(1:2, 320)) {
#  cat(paste0("[", i, "] "))
#  writeLines(strwrap(as.character(corpus[[i]]), 60))
#}

control <- list(stopwords = TRUE,
                removePunctuation = TRUE,
                removeNumbers = TRUE)
tdm <- TermDocumentMatrix(corpus, control)
inspect(tdm)

#word.freq <- rowSums(as.matrix(tdm))

#/Machine Learning/
tdm.matrix <- as.matrix(tdm)
dim(tdm.matrix)
counts <- rowSums(tdm.matrix)
kw.df <- data.frame(cbind(names(counts),
                            as.numeric(counts)),
                      stringsAsFactors = FALSE)
names(kw.df) <- c("term", "frequency")
kw.df$frequency <- as.numeric(kw.df$frequency)
kw.occurrence <- sapply(1:nrow(tdm.matrix),
                          function(i)
                          {
                            length(which(tdm.matrix[i, ] > 0)) / ncol(tdm.matrix)
                          })
kw.density <- kw.df$frequency / sum(kw.df$frequency)
kw.df <- transform(kw.df,
                     density = kw.density,
                     occurrence = kw.occurrence)


kw.df.top <- kw.df
kw.df.mid <- kw.df

head(kw.df.top)

#kw exclusivas de top
kw.uncommon.df <- subset(kw.df.top, !(term %in% kw.df.mid$term))
#kw compartidas
kw.common.df <- subset(kw.df.top, (term %in% kw.df.mid$term))
#kw.common.df <- merge(kw.df.top, kw.df.bottom, by = "term", sort = F)
head(kw.common.df)
#palabras en top que no están en bottom
head(kw.uncommon.df)

head(kw.df.top %>% arrange(-(occurrence)), 30)


topkw.list <- kw.df.top$term

#con todas
topkw.list <- kw.df$term
#############
models <- lapply(topkw.list, function(x) {
  kw.df.top$term
})


##??
kw.df.top$term %in% video.df$product.name
data.frame(v1=video.df$product.name, v4=m[match(video.df$product.name, kw.df.top$term ), 2])


rank.common <- as.data.frame(
  transform(kw.common.df, 
            term.rank = ave(occurrence, 
                            FUN = function(x) rank(-x, ties.method = "first")))
)

rank.common <- rank.common %>% arrange((term.rank)) 

rank.uncommon <- as.data.frame(
  transform(kw.uncommon.df, 
            term.rank = ave(occurrence, 
                            FUN = function(x) rank(-x, ties.method = "first")))
)

rank.uncommon <- rank.uncommon %>% arrange((term.rank)) 

#########################################



get.tdm <- function(doc.vec)
{
  control <- list(stopwords = TRUE,
                  removePunctuation = TRUE,
                  removeNumbers = TRUE,
                  minDocFreq = 2)
  doc.corpus <- Corpus(VectorSource(doc.vec))
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)
  return(doc.dtm)
}




classify.video <- function(kw, rank, prior = 0.5, c = 1e-6)
{

  
  msg.tdm <- get.tdm(kw)
  msg.freq <- rowSums(as.matrix(msg.tdm))
  # Find intersections of words
  msg.match <- intersect(names(msg.freq), rank$term)
  # Now, we just perform the naive Bayes calculation
  if(length(msg.match) < 1)
  {
    return(prior * c ^ (length(msg.freq)))
  }
  else
  {
    match.probs <- rank$occurrence[match(msg.match, rank$term)]
    return(prior * prod(match.probs) * c ^ (length(msg.freq) - length(msg.match)))
  }
}


classify.video(c(head(videos.df)[1,]$product.name,head(videos.df)[1,]$tags), rank.common)
classify.video("viejo", rank.common)
classify.video("heidi", rank.common)
classify.video("superman", rank.common)
classify.video("jovencitos", rank.common)

video.classifier <- function(kw)
{

  pr.highengament <- classify.video(kw, rank.common)
  pr.lowengament <- classify.video(kw, rank.uncommon)
  related.kw <- findAssocs(tdm, kw, corlimit=0.15)
  print(pr.highengament)

  return(c(pr.highengament, pr.lowengament,related.kw, ifelse(pr.highengament > pr.lowengament, 1, 0)))
}

head(rank.common)
write.csv(rank.common, file="kw_rank_common.csv", sep = ",") 


video.classifier("jovencitos")

#########################################



head(kw.df)

toplot <-kw.df.mid

toplot$term = reorder(toplot$term, toplot$occurrence)
ggplot(
  toplot %>% arrange(-(occurrence)) %>% top_n(30)
  , aes(x=term, y=occurrence)) + geom_point(size=5, colour="red") + coord_flip() +
  ggtitle("Top 30 Kw") + 
  theme(axis.text.x=element_text(size=13,face="bold", colour="black"), axis.text.y=element_text(size=13,colour="black",
                                                                                                face="bold"), axis.title.x=element_text(size=14, face="bold"), axis.title.y=element_text(size=14,face="bold"),
        plot.title=element_text(size=24,face="bold"))


########################################


#########################################

library(fpc)   
d <- dist(tdm, method="euclidian")   
kfit <- kmeans(d, 3)   
#clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 
clusplot(as.matrix(d), diss=TRUE, kfit$cluster, color=TRUE, shade = TRUE, lines=0)

######################
docs <- Corpus(VectorSource(c(videos.df$product.name, videos.df$tags)))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, c(stopwords("spanish"), "vs."))
inspect(docs)[1]
dtm <- DocumentTermMatrix(docs)
dtmss <- removeSparseTerms(dtm, 0.97)
inspect(dtmss) 
library(cluster)   
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward.D")   
fit   
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters 

#https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html

kfit <- kmeans(d, 5)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   


#set.seed(123)
#train_ind <- sample(seq_len(nrow(kw.common.df)), size = floor(0.75 * nrow(kw.common.df)))
#train <- kw.common.df[train_ind, ]
#test <- kw.common.df[-train_ind, ]

library(topicmodels)

tdm2 <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE, removeNumbers = TRUE,minWordLength=3,wordLengths = c(1, Inf)))
dtm2 <- as.DocumentTermMatrix(tdm2)
lda <- LDA(dtm2, k = 8) # find 8 topics
term <- terms(lda, 6) # first 4 terms of every topic
term
term <- apply(term, MARGIN = 2, paste, collapse = ", ")
require(data.table)
topic <- topics(lda, 1)
class(videos.df$date)
topics <- data.frame(date=as.IDate(videos.df$date), topic)
qplot(date, ..count.., data=topics, geom="density",
      fill=term[topic], position="stack")
summary(videos.df$date)
####################### GLM NET

#SIN TERMNINAR https://github.com/johnmyleswhite/ML_for_Hackers/blob/master/06-Regularization/chapter06.R

# CORPUS SOBRE rank.uncommon

ranked.keywords.df <- data.frame(Text = rank.uncommon$term, rank=rank.uncommon$term.rank)


set.seed(123)
train_ind <- sample(seq_len(nrow(ranked.keywords.df)), size = floor(0.75 * nrow(ranked.keywords.df)))
training <- as.matrix(ranked.keywords.df[train_ind, ])
test <- ranked.keywords.df[-train_ind, ]

library(glmnet)
glm.fit <- glmnet(training$Text, training$rank)

row.names(ranked.keywords.df) <- 1:nrow(ranked.keywords.df)




corpus_rank <- Corpus(VectorSource(rank.uncommon$term))
corpus_rank <- tm_map(corpus_rank, tolower)
corpus_rank <- tm_map(corpus_rank, stripWhitespace)
corpus_rank <- tm_map(corpus_rank, removeWords, stopwords('spanish'))
inspect(corpus_rank)
dtm_rank <- DocumentTermMatrix(corpus_rank)
x <- as.matrix(dtm2)
dim(x)


######################

library(randomForest)
names(videos.df)
fit <- randomForest(eng.ratio ~ .,   data = videos.df %>% select(impressions, detail, play, lenght, rating ,eng.ratio))
print(fit) 
#actualmente no hay relación entre las impresiones, detalles y clicks, sólo 
importance(fit) 

fit2 <- randomForest(eng.ratio ~ .,   data = video.by.eng.ratio.top %>% select(lenght, rating, play, eng.ratio))
print(fit2) # view results 
importance(fit2) 
#sin emabrgo el 82% del engagent rate lo dan las variables anteriores

library(party)

mytree.top <- ctree(eng.ratio ~ lenght + rating + play, data = na.omit(video.by.eng.ratio.top))
plot(mytree.top)
mytree.mid <- ctree(eng.ratio ~ lenght + rating + play, data = na.omit(video.by.eng.ratio.mid))
plot(mytree.mid)
#length top
586/60 #10 minutos duración video
#length mid > 929
929/60 #15 minutos duración video
1178/60 #20 minutos duración video

select(-(product.sku), -(tags), -(product.name))
video.by.eng.ratio.top %>% select(impressions, detail, play, eng.ratio)
##########################################
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 100)

df <- data.frame(term = names(term.freq), freq = term.freq)
head(df)
df
#rank
rank <- as.data.frame(
  transform(df, 
          term.rank = ave(freq, 
                          FUN = function(x) rank(-x, ties.method = "first")))
)

rank <- rank %>% arrange((term.rank)) 

write.csv(rank, file="rank.csv") 

install.packages("lda")
library(lda)
text <- dtm2ldaformat(tdm, omit_empty = FALSE) 
text$documents
set.seed(2013)
K <- 25 # topics become more coherent with more iterations
system.time(result <- lda.collapsed.gibbs.sampler(text$documents,
                                                  K,
                                                  text$vocab,
                                                  1000,
                                                  alpha = 50/K,
                                                  eta = 200/ncol(tdm),
))


topwords <- top.topic.words(result$topics, 10, by.score = TRUE) # 10 most likely words for each topic
p_topic <- as.vector(result$topic_sums / sum(result$topic_sums)) # Distribution of topics across Corpus
topdocs <- top.topic.documents(result$document_sums) # Most likely documents for each topic
names(result)
names(result)
dim(result$assignments) # For each document, vector of integers with topic assignment per word
dim(result$topics) # Per topic word assignments -- 
dim(result$topic_sums) # Number of times words assigned to each topic
result$topic_sums / sum(result$topic_sums) # distribution of topic assignments
dim(result$document_sums) # topic x document matrix -- number of times words in each document assigned to a topic

K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02

theta <- t(apply(result$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(result$topics) + eta, 2, function(x) x/sum(x)))
term.table <- table(unlist(doc.list))
head(corpus)

newsLdaModel <- LDA(tdm,method="VEM",k=4)
get_terms(newsLdaModel, k=10)
get_topics(newsLdaModel)
# the topic contribution of each topic to each document
postTopics <- data.frame(posterior(newsLdaModel)$topics)
x <- sapply(rownames(postTopics),strsplit,'_')
paperNames <- sapply(x, head, n=1)
postTopics["paper"]<- paperNames
library(plyr)
byPaperTopics <-ddply(postTopics, "paper", numcolwise(mean))
barplot(byPaperTopics$X14, names.arg=byPaperTopics$paper, beside=TRUE)