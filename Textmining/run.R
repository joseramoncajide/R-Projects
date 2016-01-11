
rm(list = ls())


path <- '~/Documents/GitHub/R-Projects/Textmining/tag_analysis'


# env ---------------------------------------------------------------------

Sys.setenv(RGA_CLIENT_ID = "1099046822903-fihc0lcqnv1gj4fe5k7sllbv9ejj0nqo.apps.googleusercontent.com", RGA_CLIENT_SECRET = "ZHs5oDJ1lZERgvhj-fW3_CKj")

ga.profileId <- '110633069'


# libraries ---------------------------------------------------------------

if(!exists("package", mode="function")) source("util.R")

list.of.packages <- c('ggplot2', 'RGA', 'lubridate', 'plyr', 'tidyr', 'dplyr', 'splitstackshape', 'jsonlite', 'RSQLite', 'knitr', 'markdown')

package(list.of.packages, update=FALSE)

lapply(list.of.packages, library, character.only = TRUE)

rm(list=c('list.of.packages', 'package'))


# dirs --------------------------------------------------------------------

setwd(path)

dir.create(file.path(path, 'data'), showWarnings = FALSE)


# dbs ---------------------------------------------------------------------

sqlite    <- dbDriver("SQLite")
tubes.db <- dbConnect(sqlite,"tubes.db")

# get data ----------------------------------------------------------------

start.date <- floor_date(Sys.Date() - 14, "week") + 1

end.date <- floor_date(Sys.Date() -7 , "week") 



ga_token <- authorize(cache = ".gatoken")

week(end.date)

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
         dimensions = "ga:dimension4,ga:socialInteractionNetwork,ga:socialInteractionTarget", 
         sort = "-ga:uniqueSocialInteractions", 
         filters = "ga:socialInteractionNetwork!=undefined;ga:pagePath=~(/video/|/films/|/видео/|/filmy/|/videolari/|/porn-video/|/filme/|/videos/);ga:socialInteractionTarget!~^http;ga:uniqueSocialInteractions>0",
         sampling.level = "HIGHER_PRECISION", token = ga_token), 
  silent = TRUE)

# head(video.social.metrics.df)

names(video.social.metrics.df) <- c(
  "site",
  "social.interaction.network",
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

top_plays.df  <- filter(video.df, grepl("^[a-zA-Z]{1,}_[a-z]{2}_[0-9]{4,5}$", product.sku)) %>%  group_by(site, video.id, product.name) %>% dplyr::summarise(plays = sum(play))  %>% arrange(desc(plays)) %>% group_by(site) %>% mutate(rank = rank(-plays, ties.method = "first")) %>% ungroup() %>% select(site, product.name, video.id, plays,rank) %>% arrange(site, rank)

#dbWriteTable(tubes.db, "top_played", as.data.frame(top_plays.df[0, ]), overwrite = T)
dbWriteTable(conn = tubes.db, "top_played", as.data.frame(top_plays.df), overwrite = T)

dbWriteTable(conn = tubes.db, name = "top_played", value = "top_played_videos.csv", 
             row.names = FALSE, header = TRUE, overwrite = T)
dbListFields(tubes.db, "top_played")
dbDisconnect(tubes.db)

# markdown report ---------------------------------------------------------



# ok
 knit("report_tubes_top.Rmd")
 markdownToHTML('report_tubes_top.md', 'report_tubes_top.html')
 browseURL(paste('file://', file.path(getwd(),'report_tubes_top.html'), sep=''))
# #install.packages("knitr")
# #install.packages("markdown")
# 
# # Load packages
# require(knitr)
# require(markdown)





# Create .md, .html, and .pdf files
# knit("report_tubes_top.Rmd")
# markdownToHTML('report_tubes_top.md', 'report_tubes_top.html')


# rmarkdown::render("report_tubes_top.Rmd", "pdf_document")
