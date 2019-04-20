library(ggplot2)
library(jsonlite)
library(magrittr)
library(lubridate)



bbc.ni.tweets.json <- getwd() %>% 
  paste0("/data/bbc/bbcnewsni.json") %>% 
  jsonlite::read_json()

jp.tweets.json <- getwd() %>% 
  paste0("/data/bbc/jp_biz.json") %>% 
  jsonlite::read_json()

news.tweets.json <- c(
  bbc.ni.tweets.json,
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
    theme %<>% append("Education & Skills")
  } else if (grepl(paste(c("york street", "street", "transport", "interchange", "a5", "road"), collapse = "|"), tweetText, ignore.case = TRUE)) {
    theme %<>% append("Infrastructure")
  } else if (grepl(paste(c("stormont", "assembly", "executive", "minister"), collapse = "|"), tweetText, ignore.case = TRUE)) {
    theme %<>% append("NI Assembly")
  } else if (grepl(paste(c("energy", "coal", "gas", "power"), collapse = "|"), tweetText, ignore.case = TRUE)) {
    theme %<>% append("Energy")
  } else if (grepl(paste(c("productivity", "innovation"), collapse = "|"), tweetText, ignore.case = TRUE)) {
    theme %<>% append("Productivity")
  } else if (grepl(paste(c("equality", "lgbt", "abortion", "rights", "gay", "lesbian"), collapse = "|"), tweetText, ignore.case = TRUE)) {
    theme %<>% append("Rights & Equality")
  } else if (grepl(paste(c("nhs", "hospital", "healthcare"), collapse = "|"), tweetText, ignore.case = TRUE)) {
    theme %<>% append("Healthcare")
  } else {
    theme %<>% append("unknown")
  }
  if (grepl(paste(c("dup", "democratic unionist party", "arlene", "foster", "sammy wilson", "nigel dodds"), collapse = "|"), tweetText, ignore.case = TRUE)) {
    party %<>% append("DUP")
  } else if (grepl(c("sinn fein", "sinn féin", "mary lou mcdonald", "michelle o'neill"), tweetText, ignore.case = TRUE)) {
    party %<>% append("Sinn Féin")
  } else if (grepl(paste(c("uup", "ulster unionist party", "robin swann", "may steele", "reg empey"), collapse = "|"), tweetText, ignore.case = TRUE)) {
    party %<>% append("UUP")
  } else if (grepl(paste(c("sdlp", "social democratic and labour party", "colum eastwood", "nichola mallon"), collapse = "|"), tweetText, ignore.case = TRUE)) {
    party %<>% append("SDLP")
  } else if (grepl(paste(c("alliance", "naomi long", "stephen farry"), collapse = "|"), tweetText, ignore.case = TRUE)) {
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
  subset(party != "unknown")

news.tweets.df %>%
  ggplot() +
  geom_histogram(
    mapping = aes(
      x = news.tweets.df$ymd.datetime., 
      fill = news.tweets.df$party),
    colour = "#EFEFEF",
    bins = 68) + 
  xlab("") + ylab("") +
  ggthemes::scale_fill_tableau(name="") +
  scale_x_date(date_breaks = "years" , date_labels = "%Y") +
  theme_minimal() +
  theme(legend.position = "bottom")

