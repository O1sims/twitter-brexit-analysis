library(ggplot2)
library(jsonlite)
library(magrittr)
library(lubridate)



jp.tweets.json <- getwd() %>% 
  paste0("/data/bbc/jp_biz.json") %>% 
  jsonlite::read_json()

rm.tweets.json <- getwd() %>% 
  paste0("/data/belfast-telegraph/ryanmcaleerbiz.json") %>% 
  jsonlite::read_json()

jm.tweets.json <- getwd() %>% 
  paste0("/data/ulster-business/newsmulg.json") %>% 
  jsonlite::read_json()

news.tweets.json <- c(
  jp.tweets.json)

tweetId <- datetime <- favourites <- 
  retweets <- text <- theme <- party <- c()
for (i in 1:length(news.tweets.json)) {
  datetime %<>% append(
    as.POSIXct(
      x = strptime(
        x = paste0((news.tweets.json[[i]]$created_at %>% 
                      strsplit(" "))[[1]][c(2,3,6)], 
                   collapse = " "), 
        format = "%b %d %Y")))
  favourites %<>% append(news.tweets.json[[i]]$favorite_count)
  retweets %<>% append(news.tweets.json[[i]]$retweet_count)
  tweetId %<>% append(news.tweets.json[[i]]$id_str)
  tweetText <- news.tweets.json[[i]]$text
  if (grepl(paste(c(
    "brexit", "withdrawal", "deal", "chequers", "european union", " eu", "brussels", "import", "export", "trade", 
    "leave", "cameron", "merkel", "macron", "remain", "wto", "border", "backstop", "tusk", "theresa may", "barnier", "exit"), collapse = "|"), tweetText, ignore.case = TRUE)) {
    theme %<>% append(" UK/EU Relations")
  } else if (grepl(paste(c("education", "school", "skill", "universit", "qub", "uu"), collapse = "|"), tweetText, ignore.case = TRUE)) {
    theme %<>% append("Education")
  } else if (grepl(paste(c("york street", "street", "transport", "interchange", "a5", "road"), collapse = "|"), tweetText, ignore.case = TRUE)) {
    theme %<>% append("Infrastructure")
  } else if (grepl(paste(c("stormont", "assembly", "executive", "minister"), collapse = "|"), tweetText, ignore.case = TRUE)) {
    theme %<>% append("NI Assembly")
  } else if (grepl(paste(c("energy", "coal", "gas", "power"), collapse = "|"), tweetText, ignore.case = TRUE)) {
    theme %<>% append("Energy")
  } else if (grepl(paste(c("productivity", "innovation"), collapse = "|"), tweetText, ignore.case = TRUE)) {
    theme %<>% append("Productivity")
  } else {
    theme %<>% append("unknown")
  }
  if (grepl(paste(c("dup", "democratic unionist party"), collapse = "|"), tweetText, ignore.case = TRUE)) {
    party %<>% append("DUP")
  } else if (grepl("sinn fein", tweetText, ignore.case = TRUE)) {
    party %<>% append("Sinn Fein")
  } else if (grepl(paste(c("uup", "ulster unionist party"), collapse = "|"), tweetText, ignore.case = TRUE)) {
    party %<>% append("UUP")
  } else if (grepl(paste(c("sdlp", "social democratic and labour party"), collapse = "|"), tweetText, ignore.case = TRUE)) {
    party %<>% append("SDLP")
  } else if (grepl(paste(c("alliance"), collapse = "|"), tweetText, ignore.case = TRUE)) {
    party %<>% append("Alliance")
  } else {
    party %<>% append("unknown")
  }
  text %<>% append(tweetText)
}

news.tweets.df <- data.frame(
  tweetId, ymd(datetime), favourites, 
  retweets, text, theme, party,
  stringsAsFactors = FALSE) %>% 
  subset(theme != "unknown")

news.tweets.df %>%
  ggplot() +
  geom_histogram(
    mapping = aes(
      x = news.tweets.df$ymd.datetime., 
      fill = news.tweets.df$theme),
    colour = "#EFEFEF",
    bins = 68) + 
  xlab("") + ylab("Count") +
  ggthemes::scale_fill_tableau() +
  theme_minimal() +
  theme(legend.position = "bottom")

