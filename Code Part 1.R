# 1. Library --------------------------------------------------------------

library("jsonlite")
library("tidyverse")
library("lubridate")
library("ggplot2")
library("tidytext")
library("qdap")
library("tm")
library("tokenizers")
library("RWeka")
library("wordcloud")
library("stringi")



# 2. Uploading Data -------------------------------------------------------

reviews <- read.csv("employee_reviews.csv",stringsAsFactors=FALSE)

head(reviews)

View(reviews[1:10,])

str(reviews)


# 3. Functions ------------------------------------------------------------

# Cleaning of Text data
qdap_clean <- function(x){
  x <- replace_abbreviation(x)
  x <- replace_contraction(x)
  x <- replace_number(x)
  x <- replace_ordinal(x)
  x <- replace_symbol(x)
  x <- tolower(x)
  return(x)
}


# To control tokenizer and TDM 

tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

??NGramTokenizer


tm_clean <- 
  function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, 
                   c(stopwords("en"), "Google", "Amazon","Facebook","Microsoft", 
                      "Netflix","Apple", "company"))
  return(corpus)
  }


removeWords

# Polarity

pol_subsections <- function(df) {
  x.pos <- subset(df$text, df$polarity > 0)
  x.neg <- subset(df$text, df$polarity < 0)
  x.pos <- paste(x.pos, collapse = " ")
  x.neg <- paste(x.neg, collapse = " ")
  all.terms <- c(x.pos, x.neg)
  return(all.terms)
}



term_freq_fun <- 
  function(x){
    x <- qdap_clean(x)
    x <- VCorpus(VectorSource(x))
    x <- tm_clean(x)
    x <- TermDocumentMatrix(x,control=list(tokenize=tokenizer))
    x <- as.matrix(x)
    x <- rowSums(x)
    x <- sort(x,decreasing=TRUE)
    return(x)
  }


# 4. EDA ------------------------------------------------------------------

str(reviews)

reviews$dates <- as.Date(as.character(reviews$dates), " %B %d, %Y")

names(reviews)

table(reviews$company)


sum(stri_count_words(microsoft$pros))
sum(stri_count_words(microsoft$cons))




View(netflix %>% 
  group_by(overall.ratings) %>% 
  summarize(total_reviews=n(),
            word_count_pros = sum(stri_count_words(pros)),
            word_count_cons = sum(stri_count_words(cons)))                               )





colSums(is.na(reviews))
# No NAs in relevant columns

amazon <- 
  reviews %>% 
  filter(company=="amazon") 

amazon_pros <- amazon$pros

amazon_cons <- amazon$cons

table(format(amazon$dates,"%Y"))


apple <- 
  reviews %>% 
  filter(company=="apple") 

apple_pros <- apple$pros

apple_cons <- apple$cons

table(format(apple$dates,"%Y"))


facebook <- 
  reviews %>% 
  filter(company=="facebook") 

facebook_pros <- facebook$pros

facebook_cons <- facebook$cons

table(format(facebook$dates,"%Y"))


google <- 
  reviews %>% 
  filter(company=="google") 

google_pros <- google$pros

google_cons <- google$cons

table(format(google$dates,"%Y"))

microsoft <- 
  reviews %>% 
  filter(company=="microsoft") 

microsoft_pros <- microsoft$pros

microsoft_cons <- microsoft$cons

table(format(microsoft$dates,"%Y"))

netflix <- 
  reviews %>% 
  filter(company=="netflix") 

netflix_pros <- netflix$pros
  
netflix_cons <- netflix$cons

table(format(netflix$dates,"%Y"))



netflix_pros_qclean <- qdap_clean(netflix_pros)
netflix_pros_corpus <- VCorpus(VectorSource(netflix_pros_qclean))
netflix_pros_corp <- tm_clean(netflix_pros_corpus)

netflix_pros_corp_tdm <- TermDocumentMatrix(
                                  netflix_pros_corp, 
                                  control = list(tokenize = tokenizer)
                                  )

netflix_pros_corp_tdm_m <- as.matrix(netflix_pros_corp_tdm)

netflix_pros_freq <- rowSums(netflix_pros_corp_tdm_m)

netflix_pros_freq <- sort(netflix_pros_freq,decreasing=TRUE)

netflix_pros_freq[1:5]

wordcloud(names(netflix_pros_freq),netflix_pros_freq,max.words=25,color="blue")




netflix_cons_freq <- term_freq_fun(netflix_cons)
netflix_cons_freq[1:5]
wordcloud(names(netflix_cons_freq),netflix_cons_freq,max.words=100,color="blue")


google_pros_freq <- term_freq_fun(google_pros)
google_pros_freq[1:5]
wordcloud(names(google_pros_freq),google_pros_freq,max.words=25,color="blue")


gc()

################################### Year on Year ############################

## Amazon

View(amazon[1:5,])

amazon %>% group_by(year(dates)) %>% summarise(count=n())

company_year_pros <- function(comp,yr)
{
comp <- reviews %>% filter(company==comp,year(dates)==yr) %>% select(pros)
comp_qclean <- qdap_clean(comp)
comp_corpus <- VCorpus(VectorSource(comp_qclean))
comp_corp <- tm_clean(comp_corpus)

comp_tdm <- TermDocumentMatrix(
  comp_corp, 
  control = list(tokenize = tokenizer)
)

comp_tdm_m <- as.matrix(comp_tdm)

comp_freq <- rowSums(comp_tdm_m)

comp_freq <- sort(comp_freq,decreasing=TRUE)

return(comp_freq)
}

amazon_pros <- company_year_pros("amazon",2008)

amazon_pros_v2 <- data.frame(names(amazon_pros[1:length(unname(amazon_pros))]),unname(amazon_pros))

colnames(amazon_pros_v2) <- c("word","count")


ggplot(amazon_pros_v2[1:5,],aes(y=count,x=reorder(word,count)))+
  geom_bar(stat = "identity", show.legend = FALSE, fill="darkturquoise") +
  xlab("Frequent Words") +
  ylab("Number of Occurrances") +
  ggtitle("Amazon Pros 2018")+
  coord_flip() 

library(ggthemes)




ggplot(aes(drlib::reorder_within(word2, contribution, word1), 
           contribution, 
           # Color is based on positive or negative emotion
           fill = contribution > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  xlab("Words preceded by topic under analysis") +
  ylab("Sentiment score * Number of occurrances") +
  ggtitle(paste("Contributing words for rating : ", as.character(star)))+
  drlib::scale_x_reordered() +
  facet_wrap( ~ word1,scales = "free", nrow = 1) +
  coord_flip()

