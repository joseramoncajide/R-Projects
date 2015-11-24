library(ggplot2)
library(plyr)
library(dplyr)

library(ganalytics)
myQuery <- GaQuery( 54924641 )  #ks

period <- c("2015-09-01", "2015-11-02")


GaDateRange(myQuery) <- period


# Report both pageviews and sessions
GaMetrics(myQuery) <- c("ga:hits", "ga:sessions", "ga:transactionRevenue")
GaDimensions(myQuery) <- c("ga:date","ga:dimension14")

SortBy(myQuery) <- "-sessions"

MaxResults(myQuery) <- 175440

# Filter for Sunday sessions only
#filtro <- Expr("dimension14", "=", "996914893.1443713586")
#TableFilter(myQuery) <- filtro

data2 <- GetGaData(myQuery)

summary(data2)

head(data2 %>% group_by("dimension14"))

by_clientid <- group_by(data2, dimension14)

result <- summarise(by_clientid,
          count = n(),
          sessions = mean(sessions, na.rm = TRUE),
          hits = mean(hits, na.rm = TRUE))

ggplot(result, aes(sessions, hits)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()