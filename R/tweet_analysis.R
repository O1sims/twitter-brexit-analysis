library(jsonlite)
library(magrittr)


jp.tweets.json <- getwd() %>% 
  paste0("/jp_biz.json") %>% 
  jsonlite::read_json()

datetime <- favourites <- c()
for (i in 1:length(jp.tweets.json)) {
  datetime %<>% append(
    as.POSIXct(
      x = strptime(
        x = paste0((jp.tweets.json[[i]]$created_at %>% 
                      strsplit(" "))[[1]][c(2,3,6)], 
                   collapse = " "), 
        format = "%b %d %Y")))
  favourites %<>% append(jp.tweets.json[[i]]$favorite_count)
}

qplot(
  x = tweet_timeline, 
  y = favourites,
  alpha = 0.5)
