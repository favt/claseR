#library
library(tidyverse)
library(readxl)


####################################
# levantar la base de datos xlsx
data_tweets = read_excel("Data/data_tweets.xlsx", progress = TRUE)
# View(data_tweets)

#######################################
# pivot table desde la base original
# reduce de 343,866 casos a 209,824

pivot_base = data_tweets %>% group_by(country, language, screen_name) %>% summarise(total_tweets = n(), followers = max(followers_count), friends = max(friends_count), .groups = "drop")

#agregar tweets share column por country ubicada junto a total_tweets
pivot_country = pivot_base %>% group_by(country) %>% summarise(total_tweets = sum(total_tweets), followers = sum(followers), friends = sum(friends), .groups = "drop")
pivot_country = pivot_country %>% mutate(tweets_share = total_tweets / sum(total_tweets, na.rm = TRUE), .after = "total_tweets")

#summary de la tabla resumida por country
summary(pivot_country)

#histogramas (limpiar data extremos)
hist(x = pivot_country$followers, main = "Histograma followers total")
hist(x = pivot_country$total_tweets, main = "Histograma tweets total")

#dispersion (limpiar data extremos)
plot(x = pivot_country$total_tweets, y = pivot_country$followers, main = "Relación tweets / followers total")



