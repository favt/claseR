#library
library(tidyverse)
library(readxl)
library(ggplot2)
library(qdapRegex)
library(textclean)
library(tidytext)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(ggwordcloud)
library(treemapify)

####################################
# levantar la base de datos xlsx
data_tweets = read_excel("Data/data_tweets.xlsx")
# View(data_tweets)

#######################################
# pivot table desde la base original
# reduce de 343,866 casos a 211,044
# filter = language
# data clean

# filtro de idioma reduce de 343,866 obs a 211,044
# data_tweets_EN_ES = data_tweets %>% filter(language == "english" | language == "spanish") 
data_tweets_EN = data_tweets %>% select(screen_name, language, tweettext, textsentiment) %>% filter(language == "english") 

# limpiar url / tags
# limpiar emojis por palabra descriptiva
# limpiar <> de los emojis
# limpiar hastags
# limpiar menciones
# limpiar RT
# limpiar espacios
data_tweets_EN = data_tweets_EN %>% mutate(textsentiment = replace_url(tweettext, pattern = qdapRegex::grab("rm_url"), replacement = ""))
data_tweets_EN = data_tweets_EN %>% mutate(textsentiment = replace_emoji(textsentiment))
data_tweets_EN = data_tweets_EN %>% mutate(textsentiment = str_replace_all(textsentiment, "<\\w+>", ""))
data_tweets_EN = data_tweets_EN %>% mutate(textsentiment = str_replace_all(textsentiment, "\\d", ""))
data_tweets_EN = data_tweets_EN %>% mutate(textsentiment = str_replace_all(textsentiment, "#\\S+", ""))
data_tweets_EN = data_tweets_EN %>% mutate(textsentiment = str_replace_all(textsentiment, "@\\S+", ""))
data_tweets_EN = data_tweets_EN %>% mutate(textsentiment = str_replace(textsentiment, "RT ", ""))
data_tweets_EN = data_tweets_EN %>% mutate(textsentiment = trimws(textsentiment))

sw = stop_words %>% select(word)

# unique tweets
data_Unique = data_tweets_EN %>% filter(nchar(textsentiment) > 10) %>% distinct(textsentiment=tolower(textsentiment))

data_Unique.corpus = data_Unique %>% tm::VectorSource() %>% tm::Corpus()

data_Unique.corpus.clean = data_Unique.corpus %>% 
  tm_map(removeWords, stopwords("english")) %>% # limpiar stopwords
  tm_map(removeWords, c("will", "let", "ring")) %>%   # limpiar palabras puntuales
  tm_map(removePunctuation) %>%   # limpiar signos de puntuación
  tm_map(stripWhitespace)   # eliminar espacios en blanco dobles


data_Unique.Frequency = data_Unique.corpus.clean %>% 
  tm::TermDocumentMatrix() %>% 
  as.matrix() %>% as.data.frame() %>% 
  tibble::rownames_to_column() %>%
  dplyr::rename(word = 1, freq = 2) %>%
  dplyr::arrange(desc(freq)) %>% top_n(50)


ggplot(data_Unique.Frequency, aes(area = freq, fill = word, label =  paste(word, freq, sep = "\n"))) +
  geom_treemap()+
  theme(legend.position = "none")+
  scale_fill_discrete()+
  geom_treemap_text()


ggplot(data = data_Unique.Frequency, 
       aes(label = word, size = freq, col = as.character(freq))) + 
  geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
                      grid_size = 1, eccentricity = .9)+
  scale_size_area(max_size = 14)+
  scale_color_brewer(palette = "Paired", direction = -1)+
  theme_void()



# data_tweets_EN_Unique = data_tweets_EN_Unique %>% mutate(sentiment = analyzeSentiment(textsentiment, language = "english", aggregate = NULL, removeStopwords = TRUE))

# data_tweets_Check = data_tweets_EN %>% filter(screen_name == "EchosOG")
# data_tweets_Check = data_tweets_Check %>% mutate(textsentiment = replace_url(tweettext, pattern = qdapRegex::grab("rm_url"), replacement = ""))
# 
# # <\w+> eliminar caracteres emoji convertidos a texto
# data_tweets_Check = data_tweets_Check %>% mutate(textsentiment = replace_emoji(textsentiment))
# data_tweets_Check = data_tweets_Check %>% mutate(textsentiment = str_replace_all(textsentiment, "<\\w+>", ""))




# data_tweets_Check = data_tweets_Check %>% mutate(textsentiment = strip(textsentiment))
# data_tweets_Check = data_tweets_Check %>% mutate(textsentiment = strip(textsentiment))



# tw = "Gaff's ready, full kit w*nk*r style! #UCLFinal… https://t.co/QurGMVB4Ra"
# replace_url(tw, pattern = qdapRegex::grab("rm_url"), replacement = "")
# tw


pivot_base = data_tweets %>% group_by(country, language, screen_name) %>% summarise(total_tweets = n(), followers = max(followers_count), friends = max(friends_count), .groups = "drop")

#agregar tweets share column por country ubicada junto a total_tweets
pivot_country = pivot_base %>% group_by(country) %>% summarise(total_tweets = sum(total_tweets), followers = sum(followers), friends = sum(friends), .groups = "drop")
pivot_country = pivot_country %>% mutate(tweets_share = total_tweets / sum(total_tweets, na.rm = TRUE), .after = "total_tweets")

#summary de la tabla resumida por country
summary(pivot_country)

#histogramas (limpiar data extremos)
# hist(x = pivot_country$followers, main = "Histograma followers total")
# hist(x = pivot_country$total_tweets, main = "Histograma tweets total")

ggplot(pivot_base) +
  geom_bar(aes(x = language,
                     y=mean(total_tweets)),  # para que el área sea 1
                 bins=10,
                 fill='lightblue',
                 col='black') +
  labs(x = 'Notas de 2009', y = 'Frecuencias', title = 'Histograma') +
  facet_wrap( ~ language, nrow = 3) +
  theme_minimal() +  
  theme(panel.grid = element_blank())  # elimina grid de fondo


#dispersion (limpiar data extremos)
plot(x = pivot_country$total_tweets, y = pivot_country$followers, main = "Relación tweets / followers total")



