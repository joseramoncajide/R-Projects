rm(list = ls())


path <- '.'


# env ---------------------------------------------------------------------

Sys.setenv(RGA_CLIENT_ID = "", RGA_CLIENT_SECRET = "")

ga.profileId <- '110633069'


# libraries ---------------------------------------------------------------

if(!exists("package", mode="function")) source("util.R")

list.of.packages <- c('Rcpp', 'data.table',  'ggplot2', 'RGA', 'lubridate', 'tidyr', 'plyr', 'dplyr', 'splitstackshape', 'jsonlite', 'RSQLite', 'markdown', 'knitr')

package(list.of.packages, update=FALSE, verbose=TRUE, install=FALSE, quiet=FALSE)

lapply(list.of.packages, library, character.only = TRUE)

rm(list=c('list.of.packages', 'package'))


# dirs --------------------------------------------------------------------

setwd(path)

dir.create(file.path(path, 'data'), showWarnings = FALSE)


# dbs ---------------------------------------------------------------------

sqlite    <- dbDriver("SQLite")
tubes.db <- dbConnect(sqlite,"data/tubes.db")



# func --------------------------------------------------------------------

export_results <- function(data, tbl_name) {
  dbWriteTable(tubes.db, as.character(tbl_name), as.data.frame(data), overwrite = T)
  write.csv(data, file=paste('data/',tbl_name,'.csv', sep=''), row.names=FALSE)  
  write(toJSON(data), file=paste('data/',tbl_name,'.json', sep='')) 
}



# get data ----------------------------------------------------------------

start.date <- floor_date(Sys.Date() - 14, "week") + 1

end.date <- floor_date(Sys.Date() -7 , "week") 



ga_token <- authorize(cache = ".gatoken")


video.categories.metrics.df <- try(
  get_ga(profile.id = ga.profileId, 
         start.date = start.date, 
         end.date = end.date, 
         metrics = "ga:productDetailViews,ga:productAddsToCart,ga:cartToDetailRate", 
         dimensions = "ga:dimension4,ga:productSku,ga:productName,ga:productCategoryHierarchy", 
         sort = "-ga:productAddsToCart", 
         filters = "ga:productCategoryHierarchy!@(not set);ga:productAddsToCart>100", 
         max.results = NULL,
         sampling.level = "HIGHER_PRECISION", token = ga_token), 
  silent = FALSE)

names(video.categories.metrics.df) <- c(
  "site",
  "product.sku", 
  "product.name", 
  "product.category", 
  "detail", 
  "play",
  "playtodetailrate")
# head(video.categories.metrics.df)

video.pornstar.metrics.df <- try(
  get_ga(profile.id = ga.profileId, 
         start.date = start.date, 
         end.date = end.date, 
         metrics = "ga:productListViews,ga:productDetailViews,ga:productAddsToCart,ga:cartToDetailRate", 
         dimensions = "ga:dimension4,ga:productSku,ga:dimension17,ga:dimension15,ga:productName", 
         sort = "-ga:productAddsToCart", 
         filters = "ga:dimension17!=(not set);ga:productAddsToCart>100", 
         sampling.level = "HIGHER_PRECISION", token = ga_token), 
  silent = TRUE)

names(video.pornstar.metrics.df) <- c(
  "site",
  "product.sku", 
  "porn.star", 
  "lenght", 
  "product.name", 
  "impressions",
  "detail","play",
  "playtodetailrate"
)
# head(video.pornstar.metrics.df)


video.interaction.metrics.df <- try(
  get_ga(
    profile.id = ga.profileId, 
    start.date = start.date, 
    end.date = end.date, 
    metrics = "ga:productListViews,ga:productDetailViews,ga:productAddsToCart,ga:cartToDetailRate", 
    dimensions = "ga:dimension4,ga:productSku,ga:dimension13,ga:dimension15,ga:dimension16,ga:productName", 
    sort = "-ga:productAddsToCart", 
    filters = "ga:dimension13!=(not set);ga:productAddsToCart>100", 
    sampling.level = "HIGHER_PRECISION", token = ga_token), 
  silent = TRUE)

names(video.interaction.metrics.df) <- c(
  "site",
  "product.sku", 
  "tags", 
  "lenght", 
  "rating", 
  "product.name", 
  "impressions",
  "detail","play",
  "playtodetailrate"
)

video.interaction.metrics.df <- video.interaction.metrics.df %>%  filter(product.sku != '(other)') %>% mutate(video.id = sapply(strsplit(as.character(product.sku), "_"), tail, 1))
# str(video.interaction.metrics.df)

video.interaction.metrics.df$lenght <- as.numeric(video.interaction.metrics.df$lenght)
video.interaction.metrics.df$rating <- as.numeric(video.interaction.metrics.df$rating)


video.social.metrics.df <- try(
  get_ga(profile.id = ga.profileId, 
         start.date = start.date, 
         end.date = end.date, 
         metrics = "ga:uniqueSocialInteractions", 
         dimensions = "ga:dimension4,ga:socialInteractionTarget", 
         sort = "-ga:uniqueSocialInteractions", 
         filters = "ga:socialInteractionNetwork!=undefined;ga:pagePath=~(/video/|/films/|/видео/|/filmy/|/videolari/|/porn-video/|/filme/|/videos/);ga:socialInteractionTarget!~^http;ga:uniqueSocialInteractions>0",
         sampling.level = "HIGHER_PRECISION", token = ga_token), 
  silent = TRUE)

# head(video.social.metrics.df)

names(video.social.metrics.df) <- c(
  "site",
  "product.sku", 
  "social.interactions")

video.social.metrics.df <- video.social.metrics.df %>% 
  mutate(video.id = sapply(strsplit(as.character(product.sku), "_"), tail, 1))

# head(video.social.metrics.df)

video.engagement.metrics.df <- try(
  get_ga(profile.id = ga.profileId, 
         start.date = start.date, 
         end.date = end.date, 
         metrics = "ga:uniqueEvents,ga:eventValue", 
         dimensions = "ga:eventLabel", 
         sort = "-ga:eventValue", 
         filters = "ga:eventAction=@Time on screen;ga:uniqueEvents>0",
         sampling.level = "HIGHER_PRECISION", token = ga_token),  
  silent = TRUE)

# head(video.engagement.metrics.df)

#el objetivo es tener los engagement time de cada video para cruzarlos con los tags
video.engagement.metrics.df <- video.engagement.metrics.df %>% 
  filter(grepl("^[a-zA-Z]{1,}_[a-z]{2}_[0-9]{4,5}$", event.label)) %>% 
  mutate(engament.time = event.value / unique.events) %>% 
  select(event.label, engament.time) %>% 
  arrange(desc(engament.time)) %>% 
  mutate(video.id = sapply(strsplit(as.character(event.label), "_"), tail, 1))

names(video.engagement.metrics.df) <- c("product.sku", "engament.time", "video.id")

# head(video.engagement.metrics.df)

video.df <- join_all(
  list(video.interaction.metrics.df,video.engagement.metrics.df),
  by=c('video.id','product.sku'),
  type='left',
  match = "first")

# video.df <- left_join(video.interaction.metrics.df,video.engagement.metrics.df,
#   by='product.sku',
#   type='left',
#   match = "all")
# 
# 
# video.df <- join_all(
#   list(video.interaction.metrics.df,video.engagement.metrics.df),
#   by='product.sku',
#   type='left',
#   match = "all")

#elimnamos los vídeos que no han generado engagement time
video.df <- video.df %>%  filter(!is.na((engament.time)))

# head(video.df)

video.df$lenght <- as.numeric(video.df$lenght)

#videos con mayor play
video.df <- video.df %>%
  filter(play > quantile(play, 0.9)) %>% 
  mutate( eng.ratio = engament.time / lenght * 100) %>% 
  filter(eng.ratio < 100) %>% 
  arrange(-(eng.ratio)) 



# head(video.df)

video.pornstar.df <- join_all(
  list(video.pornstar.metrics.df,video.engagement.metrics.df),
  by='product.sku',
  type='left',
  match = "all")
# head(video.pornstar.df)

video.pornstar.df <- video.pornstar.df %>%
  filter(play > quantile(play, 0.9)) %>% 
  mutate( eng.ratio = engament.time / lenght * 100) %>% 
  filter(eng.ratio < 100) %>% 
  arrange(-(eng.ratio)) 



# tops --------------------------------------------------------------------


# top played videos -------------------------------------------------------

top_plays.df  <-
  video.df %>% 
  filter(grepl("^[a-zA-Z]{1,}_[a-z]{2}_[0-9]{4,5}$", product.sku)) %>%  
  group_by(site, product.sku, product.name) %>% 
  dplyr::summarise(plays = sum(play))  %>% 
  arrange(desc(plays)) %>% 
  group_by(site) %>% 
  mutate(rank = rank(-plays, ties.method = "first")) %>% 
  ungroup() %>% 
  select(site, product.name, product.sku, plays,rank) %>% 
  arrange(site, rank) 

export_results(top_plays.df, "top_plays")


# top videos by engagement ------------------------------------------------

top_engament.df <- 
  video.df %>% 
  filter(grepl("^[a-zA-Z]{1,}_[a-z]{2}_[0-9]{4,5}$", product.sku)) %>% 
  select(site, product.sku, product.name, play, eng.ratio) %>%  
  group_by(site, product.sku, product.name) %>%  
  dplyr::summarise(eng.ratio = sum(eng.ratio, na.rm=T)) %>% 
  ungroup() %>%  
  dplyr::arrange(desc(eng.ratio)) %>% 
  group_by(site) %>% 
  mutate(rank = rank(-eng.ratio, ties.method = "first")) %>% 
  ungroup() %>% 
  select(site, product.name, product.sku, eng.ratio,rank) %>% 
  arrange(site, rank)

export_results(top_engament.df, "top_engagement")


# top pornstarts by plays -------------------------------------------------

top_pornstar_plays.df <- 
  video.pornstar.metrics.df %>% 
  filter(grepl("^[a-zA-Z]{1,}_[a-z]{2}_[0-9]{4,5}$", product.sku)) %>% 
  select(site, product.sku, product.name, porn.star, play) %>% 
  cSplit("porn.star", sep = "|", direction = "long") %>%  
  group_by(site, porn.star) %>%  
  dplyr::summarise(plays = sum(play, na.rm=T)) %>% 
  ungroup() %>%  
  dplyr::arrange(desc(plays)) %>% 
  group_by(site) %>% 
  mutate(rank = rank(-plays, ties.method = "first")) %>% 
  ungroup() %>% 
  select(site, porn.star, plays,rank) %>% 
  arrange(site, rank) 

export_results(top_pornstar_plays.df, "top_pornstar_plays")


# top pornstars by engagement ---------------------------------------------

top_pornstar_engament.df <- 
  video.pornstar.df %>% #???????video.pornstar.df
  filter(grepl("^[a-zA-Z]{1,}_[a-z]{2}_[0-9]{4,5}$", product.sku)) %>% 
  select(site, product.sku, product.name, porn.star, eng.ratio) %>% 
  cSplit("porn.star", sep = "|", direction = "long") %>%  
  group_by(site, porn.star) %>%  
  dplyr::summarise(eng.ratio = mean(eng.ratio, na.rm=T)) %>% 
  ungroup() %>%  
  dplyr::arrange(desc(eng.ratio)) %>% 
  group_by(site) %>% 
  mutate(rank = rank(-eng.ratio, ties.method = "first")) %>% 
  ungroup() %>% 
  select(site, porn.star, eng.ratio,rank) %>% 
  arrange(site, rank) 

export_results(top_pornstar_engament.df, "top_pornstar_engament") 

# top videos by category --------------------------------------------------

top_n_by_category.df <- 
  video.categories.metrics.df %>% 
  filter(grepl("^[a-zA-Z]{1,}_[a-z]{2}_[0-9]{4,5}$", product.sku)) %>% 
  select(site, product.sku, product.name, product.category, play) %>% 
  cSplit("product.category", sep = "|", direction = "long")  %>% 
  group_by(site, product.category, product.sku, product.name) %>%  
  dplyr::summarise(plays = sum(play, na.rm=T)) %>% 
  group_by(product.category) %>% 
  mutate(rank = rank(-plays, ties.method = "first")) %>% 
  ungroup() %>% 
  dplyr::arrange(desc(plays), site, product.category ) %>% 
  ungroup() %>% 
  select(site, product.sku, product.name, product.category, plays, rank) %>% 
  arrange(site, product.category, rank) 

export_results(top_n_by_category.df, "top_n_by_category")



# top rated ---------------------------------------------------------------

top_rating.df  <- 
  video.interaction.metrics.df %>% 
  filter(grepl("^[a-zA-Z]{1,}_[a-z]{2}_[0-9]{4,5}$", product.sku)) %>%  
  group_by(site, product.sku, product.name) %>% 
  dplyr::summarise(ratings = sum(rating))  %>% 
  arrange(desc(ratings)) %>% 
  group_by(site) %>% 
  mutate(rank = rank(-ratings, ties.method = "first")) %>% 
  ungroup() %>% 
  select(site, product.name, product.sku, ratings,rank) %>% 
  arrange(site, rank) 

export_results(top_rating.df, "top_rated")


# top shared --------------------------------------------------------------

top_shares.df <- 
  join_all(list(video.social.metrics.df, video.categories.metrics.df[ , -which(names(video.categories.metrics.df) %in% c("site"))]), by = "product.sku", type = "left", match = "first") %>%
  select(site, product.sku, product.name, social.interactions) %>%
  group_by(site, product.sku, product.name) %>% 
  dplyr::summarise(shares = sum(social.interactions, na.rm=T))  %>%
  ungroup() %>% 
  arrange(desc(shares)) %>% 
  group_by(site) %>% 
  mutate(rank = rank(-shares, ties.method = "first")) %>% 
  ungroup() %>% 
  select(site, product.name, product.sku, shares,rank) %>% 
  arrange(site, rank) 

export_results(top_shares.df, "top_shared")

# markdown report ---------------------------------------------------------


knit("report.Rmd")
rmarkdown::render("report.md")
browseURL(paste('file://', file.path(getwd(),'report.html'), sep=''))


dbDisconnect(tubes.db)


#rm(list = ls())