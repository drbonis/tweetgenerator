#Tweet Generator
#Author: Julio Bonis

#install.packages("rtweet")
library(rtweet)
#install.packages("Hmisc")
library(Hmisc)
#install.packages("dplyr")
library(dplyr)
#install.packages("tidytext")
library(tidytext)
library(tidyr)



#install.packages("widyr")
library(widyr)





tokens<-fromJSON(file="tokens.json")
create_token(
  app = tokens$app,
  consumer_key = tokens$consumer_key,
  consumer_secret <- tokens$consumer_secret,
  access_token = tokens$access_token,
  access_secret = tokens$access_secret)


retrieve_tweets<-function(username){
  tweets <- get_timelines(c(username),n=10000, include_rts=FALSE, lan="es")
  write_as_csv(tweets,paste0(username,".csv"))
  return(tweets)
}

load_tweets<-function(username){
  return(read_twitter_csv(paste0(username,".csv")))
}

build_model<-function(username) {
  tweets_raw<-load_tweets(username)
  text_df<-data.frame(line=1:length(tweets_raw$text),text=tweets_raw$text)
  text_df$text<-paste0("aaainicio ",text_df$text, " finzzz")
  text_df$text<-gsub("@\\w+", "", text_df$text)
  text_df$text<-gsub("https\\:\\/\\/.*\\/.*\\s", "", text_df$text)
  syntaxis<<-text_df %>%
    unnest_tokens(word,text)
  model_words_count <- text_df %>%
    unnest_tokens(word,text) %>%
    count(word, sort = TRUE)
  model_bigrams_count <- text_df %>%
    unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
    count(bigram, sort = TRUE) %>%
    separate(bigram,c("word1","word2"),sep=" ")
  model<-merge(model_bigrams_count,model_words_count,by.x="word2",by.y="word", suffixes=c(".all",".word2"))
  model<-merge(model,model_words_count,by.x="word1",by.y="word")
  model$n.word1<-model$n
  model$n<-model$n.all/(model$n.word1+model$n.word2)
  return(model)
}


get_next_word<-function(myword,word_pairs){
  word_pairs <- word_pairs %>%
    filter(word1 == tolower(myword))
  word_pairs$cum<-cumsum(word_pairs$n)
  delme<<-word_pairs
  random_next<-word_pairs[which.min(abs(word_pairs$cum-runif(1,0,last(word_pairs$cum)))),]$word2
  return(random_next)
}

build_random_tweet<-function(word_pairs_name,sentence_length,tweet_length){
  
  word_pairs<-get(word_pairs_name)
  tweet_words<-c("aaainicio")
  nwords<-0
  swords<-0
  while(tail(tweet_words,1)!="finzzz" & nwords<tweet_length) {
    new_word<-get_next_word(tail(tweet_words,n=1),word_pairs)
    
    if(new_word=="finzzz"){
      tweet_words[length(tweet_words)]<-paste0(tweet_words[length(tweet_words)],". ")
      tweet_words<-c(tweet_words,new_word)
    } else {
      if(nrow(word_pairs[word_pairs$word1=="aaainicio"&word_pairs$word2==new_word,])>0
         & swords>sentence_length
         ){
        #la nueva palabra es inicio de tweet
        tweet_words[length(tweet_words)]<-paste0(tweet_words[length(tweet_words)],". ")
        tweet_words<-c(tweet_words,capitalize(new_word))
        swords<-0
      } else {
        tweet_words<-c(tweet_words,new_word)
        swords<-swords+1
      }
    }
    nwords<-nwords+1
  }
  tweet_words[2]<-capitalize(tweet_words[2])
  tweet_words<-tweet_words[-1]
  tweet_words<-tweet_words[-length(tweet_words)]
  return(paste(tweet_words,collapse=" "))
}




