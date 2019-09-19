setwd("~/Documents/GitHub/SMAGroup3/james")
library(stringr)

all_reviews_movie<-read.csv("all_reviews_movie_0-1866.csv", stringsAsFactors = FALSE)
str(all_reviews_movie)

review_comments<-all_reviews_movie$comments_body
FreqCapWords<-sapply(review_comments,CountWordsCap)

alphanumeric<-str_replace_all(review_comments[2], "[^[:alnum:]]", " ")
CountWordsCap(review_comments[10])
ListWordsCap(review_comments[10])
nchar(alphanumeric)
is.na(as.numeric("1930"))
is.na(as.numeric("abc"))

# FUNCTIONS

# Is it in capital letters?
# remove the numbers and words with less than 2 letters
IsWordCap<-function(word){
  if (word==toupper(word) & is.na(as.numeric(word)) & nchar(word)>=2 )
  {
    out<-TRUE
    }
  else {out<-FALSE}
  return(out)
}

# Count the numbers of words in capital letters in a review
CountWordsCap<-function(review){
  review_clean<- str_replace_all(review, "[^[:alnum:]]", " ")
  review_split <- str_split(review_clean, boundary("word"))
  tf_vector<-sapply(unlist(review_split), IsWordCap)
  n<-sum(tf_vector)
  return(n)
}

# List of words in capital letters in a review
ListWordsCap<-function(review){
  review_clean<- str_replace_all(review, "[^[:alnum:]]", " ")
  review_split <- str_split(review_clean, boundary("word"))
  tf_vector<-sapply(unlist(review_split), IsWordCap)
  list_words<-unlist(review_split)[tf_vector]
  return(list_words)
}






