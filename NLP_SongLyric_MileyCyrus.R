#Library for the packages we need 
library(dplyr)
library(tidyr)
library(rjson)
library(magrittr)
library(purrr)
library(ggplot2)
library(tm)
library(wordcloud)
library(syuzhet)
library(circlize)
library(tidytext)
library(reshape2)
library(ggpubr)

#Getting the data that we dowloaded in Jupyter Notebook
partyintheusa <- fromJSON(file="lyrics_mileycyrus_partyintheu.s.a..json")
theclimb <- fromJSON(file="lyrics_mileycyrus_theclimb.json")
wreckingball <- fromJSON(file="lyrics_mileycyrus_wreckingball.json")
wecantstop <- fromJSON(file="lyrics_mileycyrus_wecantstop.json")
malibu <- fromJSON(file="lyrics_mileycyrus_malibu.json")
total<-toJSON(Map(c, theclimb$songs, 
                  partyintheusa$songs, 
                  wreckingball$songs,
                  wecantstop$songs, 
                  malibu$songs))
yr_2009<- toJSON(Map(c, theclimb, partyintheusa))
yr_2013<- toJSON(Map(c, wecantstop, wreckingball))
yr_2017<- malibu

total_text<-total

#Removing punctuations and alphanumeric content using gsub
total_text<- gsub("\\\\n"," ",total_text)
total_text<- gsub("\\\\"," ",total_text)
total_text<- gsub("\\\\n"," ",total_text)
total_text<- gsub('[[:punct:]]+', ' ', total_text)
total_text<- gsub("([[:alpha:]])\1+", " ", total_text)
total_text<- gsub("u2019t"," ",total_text)
total_text<- gsub("u2019s"," ",total_text)
total_text<- gsub("u2019re"," ",total_text)
total_text<- gsub("2009"," ",total_text)
total_text<- gsub("2013"," ",total_text)
total_text<- gsub("1000x1000x1"," ",total_text)
total_text<- gsub("u00a0"," ",total_text)

#creating a text corpus
corp <- Corpus(VectorSource(total_text))
# Converting the text to lowercase
corp <- tm_map(corp, content_transformer(tolower))
# Removing english common stopwords
corp <- tm_map(corp, removeWords, stopwords("english"))
#tm_map(corpus, removePunctuation)

# creating term document matrix 
tdm <- TermDocumentMatrix(corp)
# defining tdm as matrix
m <- as.matrix(tdm)
# getting word counts in decreasing order
freq = sort(rowSums(m), decreasing=TRUE) 
# creating a data frame with words and their frequencies
lyrics <- data.frame(word=names(freq), freq=freq)

lyrics <- lyrics[1:300,]


                                ## plotting word cloud##
set.seed(123)
wordcloud(words = lyrics$word, freq = lyrics$freq, 
          min.freq = 1,scale=c(3,1),
          max.words=200, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))


                               ##positive negative words##

d<- data_frame(txt=total)
d<-d %>%unnest_tokens(word, txt)

tidy_lyrics<- d
set.seed(123)
par(bg="black")
tidy_lyrics %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F86D87", "#766df8"), max.words = 250)


                    ##Plotting for the sentiments year wise##
yr_2009<- toJSON(Map(c, theclimb$songs, partyintheusa$songs))
yr_2013<- toJSON(Map(c, wecantstop$songs, wreckingball$songs))
yr_2017<- malibu$songs
#Removing punctuations and alphanumeric content
yr_2009<- gsub("\\\\n"," ",yr_2009)
yr_2009<- gsub("\\\\"," ",yr_2009)
yr_2009<- gsub("\\\\n"," ",yr_2009)
yr_2009<- gsub('[[:punct:]]+', ' ', yr_2009)
yr_2009<- gsub("([[:alpha:]])\1+", " ", yr_2009)
yr_2009<- gsub("u2019t"," ",yr_2009)
yr_2009<- gsub("u2019s"," ",yr_2009)
yr_2009<- gsub("u2019re"," ",yr_2009)
yr_2009<- gsub("2009"," ",yr_2009)
yr_2009<- gsub("2013"," ",yr_2009)
yr_2009<- gsub("1000x1000x1"," ",yr_2009)
yr_2009<- gsub("u00a0"," ",yr_2009)
#creating a text corpus
yr2009 <- Corpus(VectorSource(yr_2009))
# Converting the text to lowercase
yr2009 <- tm_map(yr2009, content_transformer(tolower))
# Removing english common stopwords
yr2009 <- tm_map(yr2009, removeWords, stopwords("english"))
#tm_map(corpus, removePunctuation)


#Removing punctuations and alphanumeric content
yr_2013<- gsub("\\\\n"," ",yr_2013)
yr_2013<- gsub("\\\\"," ",yr_2013)
yr_2013<- gsub("\\\\n"," ",yr_2013)
yr_2013<- gsub('[[:punct:]]+', ' ', yr_2013)
yr_2013<- gsub("([[:alpha:]])\1+", " ", yr_2013)
yr_2013<- gsub("u2019t"," ",yr_2013)
yr_2013<- gsub("u2019s"," ",yr_2013)
yr_2013<- gsub("u2019re"," ",yr_2013)
yr_2013<- gsub("2009"," ",yr_2013)
yr_2013<- gsub("2013"," ",yr_2013)
yr_2013<- gsub("1000x1000x1"," ",yr_2013)
yr_2013<- gsub("u00a0"," ",yr_2013)
#creating a text corpus
yr2013 <- Corpus(VectorSource(yr_2013))
# Converting the text to lowercase
yr2013 <- tm_map(yr2013, content_transformer(tolower))
# Removing english common stopwords
yr2013 <- tm_map(yr2013, removeWords, stopwords("english"))
#tm_map(corpus, removePunctuation)

#Removing punctuations and alphanumeric content
yr_2017<- gsub("\\\\n"," ",yr_2017)
yr_2017<- gsub("\\\\"," ",yr_2017)
yr_2017<- gsub("\\\\n"," ",yr_2017)
yr_2017<- gsub('[[:punct:]]+', ' ', yr_2017)
yr_2017<- gsub("([[:alpha:]])\1+", " ", yr_2017)
yr_2017<- gsub("u2019t"," ",yr_2017)
yr_2017<- gsub("u2019s"," ",yr_2017)
yr_2017<- gsub("u2019re"," ",yr_2017)
yr_2017<- gsub("2009"," ",yr_2017)
yr_2017<- gsub("2013"," ",yr_2017)
yr_2017<- gsub("1000x1000x1"," ",yr_2017)
yr_2017<- gsub("u00a0"," ",yr_2017)
#yr_2017<- grep("u2019.|u2019.."," ",yr_2017)
#creating a text corpus
yr2017 <- Corpus(VectorSource(yr_2017))
# Converting the text to lowercase
yr2017 <- tm_map(yr2017, content_transformer(tolower))
# Removing english common stopwords
yr2017 <- tm_map(yr2017, removeWords, stopwords("english"))
#tm_map(corpus, removePunctuation)



yr_2009_text<- yr_2009
##sentiments expressed in year 2009
# Getting the sentiment value for the lyrics
ty_sentiment_2009 <- get_nrc_sentiment((yr_2009_text))
# Dataframe with cumulative value of the sentiments
Sentimentscores_2009 <- data.frame(colSums(ty_sentiment_2009[,]))
# Dataframe with sentiment and score as columns
names(Sentimentscores_2009) <- "Score"
Sentimentscores_2009 <- cbind("sentiment"=rownames(Sentimentscores_2009),Sentimentscores_2009)
rownames(Sentimentscores_2009) <- NULL

yr_2013_text<- yr_2013
##sentiments expressed in year 2013
# Getting the sentiment value for the lyrics
ty_sentiment_2013 <- get_nrc_sentiment((yr_2013_text))
# Dataframe with cumulative value of the sentiments
Sentimentscores_2013 <- data.frame(colSums(ty_sentiment_2013[,]))
# Dataframe with sentiment and score as columns
names(Sentimentscores_2013) <- "Score"
Sentimentscores_2013 <- cbind("sentiment"=rownames(Sentimentscores_2013),Sentimentscores_2013)
rownames(Sentimentscores_2013) <- NULL

yr_2017_text<- yr_2017
##sentiments expressed in year 2017
# Getting the sentiment value for the lyrics
ty_sentiment_2017 <- get_nrc_sentiment((yr_2017_text))
# Dataframe with cumulative value of the sentiments
Sentimentscores_2017 <- data.frame(colSums(ty_sentiment_2017[,]))
# Dataframe with sentiment and score as columns
names(Sentimentscores_2017) <- "Score"
Sentimentscores_2017 <- cbind("sentiment"=rownames(Sentimentscores_2009),Sentimentscores_2009)
rownames(Sentimentscores_2017) <- NULL


                         ## Plotting sentiments year wise##
g_2009<-ggplot(data=Sentimentscores_2009,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores in 2009")+
  ggtitle("Sentiment based on scores in 2009")+
  theme_minimal() 
g_2013<-ggplot(data=Sentimentscores_2013,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores in 2013")+
  ggtitle("Sentiment based on scores in 2013")+
  theme_minimal() 
Sentimentscores_2017=Sentimentscores_2017[,-1]
g_2017<-ggplot(data=Sentimentscores_2017,aes(x=Sentimentscores_2017$sentiment,y=Sentimentscores_2017$Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores in 2017")+
  ggtitle("Sentiment based on scores in 2017")+
  theme_minimal() 

ggarrange(g_2009,g_2013,g_2017,nrow = 3)

                  ##top words used to describe different emotions##
tidy_lyrics<-d
song_wrd_count <- length(tidy_lyrics)

lyric_sentiment <- tidy_lyrics %>% 
  inner_join(get_sentiments("nrc"),by="word")

lyric_sentiment %>% 
  count(word,sentiment,sort=TRUE) %>% 
  group_by(sentiment)%>%top_n(n=10) %>% 
  ungroup() %>%
  ggplot(aes(x=reorder(word,n),y=n,fill=sentiment)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment,scales="free") +
  xlab("Sentiments") + ylab("Scores")+
  ggtitle("Top words used to express emotions and sentiments") +
  coord_flip()












