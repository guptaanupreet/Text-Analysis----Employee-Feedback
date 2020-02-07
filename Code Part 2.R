# 1. Library --------------------------------------------------------------

install.packages("ngram")

library(ggplot2)
library(tm)
library(ngram)
library(gridExtra)
library(wordcloud)
library(stringr)
library(cowplot)


# 2. Uploading Data -------------------------------------------------------

# reviews <- read.csv("employee_reviews.csv",stringsAsFactors=FALSE)

# amazon_reviews<-reviews[reviews$company=="amazon",c(7,8)]
# 
# #3.Creating corpus and cleaning it
# 
# cleaning_corpus<-function(context){
# text_corpus_pros<-Corpus(VectorSource(context))
# toSpace <- content_transformer(function(x, pattern) gsub(pattern, "", x))
# text_corpus_pros <- tm_map(text_corpus_pros, toSpace, "[^[:print:]]")
# text_corpus_pros <- tm_map(text_corpus_pros, removePunctuation)
# text_corpus_pros <- tm_map(text_corpus_pros, removeNumbers)
# text_corpus_pros <- tm_map(text_corpus_pros, content_transformer(tolower))
# text_corpus_pros <- tm_map(text_corpus_pros, stripWhitespace)
# return(text_corpus_pros)
# }
# 
# corpus_no_stopwords<-function(corpus){
# text_corpus_pros_no_stopwords <- tm_map(corpus, removeWords,stopwords("english"))
# text_corpus_pros_no_stopwords <- tm_map(text_corpus_pros_no_stopwords, stripWhitespace)
# return(text_corpus_pros_no_stopwords )
# }
# 
# # 3. Functions ------------------------------------------------------------
# ngram_freqdf <- function(tdm, sparsity){
#   freq <- sort(rowSums(as.matrix(removeSparseTerms(tdm, sparsity))), decreasing = TRUE)
#   return(data.frame(word = names(freq), freq = freq))
# }
# 
# ngram_barplot <- function(df, title){
#   dfsub <- subset(df[1:5,])
#   ggplot(dfsub, aes(x = reorder(word, -freq), y = freq)) +
#     geom_bar(stat = "identity",fill="darkturquoise") + 
#     labs(x = "Words", y = "Count", title = title) + 
#     theme(axis.text.x = element_text(angle = , hjust = 1), plot.title = element_text(hjust = 0.5))
# }
# 
# #PROS
# 
# text_corpus_pros<-cleaning_corpus(amazon_reviews$pros)
# text_corpus_pros_no_stopwords<-corpus_no_stopwords(text_corpus_pros)
# #one gram
# onegram_pros_tdm<-TermDocumentMatrix(text_corpus_pros)
# onegram_pros_freqdf<-ngram_freqdf(onegram_pros_tdm, 0.99)
# 
# onegram_prosNS_tdm <- TermDocumentMatrix(text_corpus_pros_no_stopwords)
# onegram_prosNS_freqdf <- ngram_freqdf(onegram_prosNS_tdm, 0.99)
# 
# #bi gram
# 
# pros_str <- concatenate( lapply ( text_corpus_pros , "[", 1) )
# prosNS_str<-concatenate( lapply ( text_corpus_pros_no_stopwords , "[", 1) )
# 
# twograms_pros<-ngram(pros_str,2)
# twogramsNS_pros<-ngram(prosNS_str,2)
# 
# twogram_pros_freqdf<-get.phrasetable ( twograms_pros )
# twogram_pros_freqdf<-twogram_pros_freqdf[,1:2]
# colnames(twogram_pros_freqdf)[1]<-"word"
# 
# twogram_prosNS_freqdf<-get.phrasetable ( twogramsNS_pros )
# twogram_prosNS_freqdf<-twogram_prosNS_freqdf[,1:2]
# colnames(twogram_prosNS_freqdf)[1]<-"word"
# 
# #barplots
# 
# 
# onegram_barplot <- ngram_barplot(onegram_pros_freqdf,"Top 20 Individual Words ")
# onegramNS_barplot <- ngram_barplot(onegram_prosNS_freqdf,"Top 20 Words (removed stopwords)")
# twogram_barplot <- ngram_barplot(twogram_pros_freqdf,"Top 20 Bigrams")
# twogramNS_barplot <- ngram_barplot(twogram_prosNS_freqdf,"Top 20 Bigrams (removed stopwords)")
# 
# 
# onegram_barplot
# onegramNS_barplot
# twogram_barplot
# twogramNS_barplot
# 
# grid.arrange(onegram_barplot,twogram_barplot, onegramNS_barplot,twogramNS_barplot,
#              ncol = 2, nrow = 2)
# 
# 
# 
# #CONS
# text_corpus_cons<-cleaning_corpus(amazon_reviews$cons)
# text_corpus_cons_no_stopwords<-corpus_no_stopwords(text_corpus_cons)
# consNS_str<-concatenate( lapply ( text_corpus_cons_no_stopwords , "[", 1) )
# twogramsNS_cons<-ngram(consNS_str,2)
# twogram_consNS_freqdf<-get.phrasetable ( twogramsNS_cons )
# twogram_consNS_freqdf<-twogram_consNS_freqdf[,1:2]
# colnames(twogram_consNS_freqdf)[1]<-"word"
# 
# wordcloud(twogram_consNS_freqdf$word,twogram_consNS_freqdf$freq,max.words=40,color="red")


#####final project
#### year and company wise
reviews <- read.csv("employee_reviews.csv",stringsAsFactors=FALSE)
reviews$dates<-str_sub(reviews$date,-4,-1)
company_names<-c("amazon","apple","microsoft")
early_years<-as.character(2015:2018)

##functions
cleaning_corpus<-function(context){
  text_corpus_pros<-Corpus(VectorSource(context))
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, "", x))
  text_corpus_pros <- tm_map(text_corpus_pros, toSpace, "[^[:print:]]")
  text_corpus_pros <- tm_map(text_corpus_pros, removePunctuation)
  text_corpus_pros <- tm_map(text_corpus_pros, removeNumbers)
  text_corpus_pros <- tm_map(text_corpus_pros, content_transformer(tolower))
  text_corpus_pros <- tm_map(text_corpus_pros, stripWhitespace)
  return(text_corpus_pros)
}

corpus_no_stopwords<-function(corpus){
text_corpus_pros_no_stopwords <- tm_map(corpus, removeWords,c(stopwords("english"),"amazon","apple","microsoft","Amazon","Apple","Microsoft","company","dot","com","worklife","balance","life","can","Can","Customers","customers","customer","Customer","like"))
text_corpus_pros_no_stopwords <- tm_map(text_corpus_pros_no_stopwords, stripWhitespace)
return(text_corpus_pros_no_stopwords )
}


ngram_barplot <- function(df, title){
  dfsub <- subset(df[1:5,])
  p<-ggplot(dfsub, aes(x = reorder(word, -freq), y = freq)) +
    geom_bar(stat = "identity",fill="darkturquoise") +
    labs(y = "Words", x = "Count", title = title) +
    theme(axis.text.x = element_text(angle =45 , hjust = 1), plot.title = element_text(hjust = 0.5))
  return(p)
}


#Pros
    c="apple"
    y="2015"
    x<- reviews[reviews$company==c & reviews$dates==y,]
    #pros 7, cons 8
    x<-x[,7]
    text_corpus_pros<-cleaning_corpus(x)
    text_corpus_pros_no_stopwords<-corpus_no_stopwords(text_corpus_pros)
    prosNS_str<-concatenate( lapply ( text_corpus_pros_no_stopwords , "[", 1) )
    twogramsNS_pros<-ngram(prosNS_str,2)
    twogram_prosNS_freqdf<-get.phrasetable ( twogramsNS_pros )
    twogram_prosNS_freqdf<-twogram_prosNS_freqdf[,1:2]
    colnames(twogram_prosNS_freqdf)[1]<-"word"
    dfsub <- subset(twogram_prosNS_freqdf[1:5,])
    plot1 <- ggplot(dfsub, aes(x = reorder(word, freq), y = freq)) +
    geom_bar(stat = "identity",fill="darkturquoise") +
    labs(x = "Words", y = "Frequency") +
    ggtitle(paste("Top 5 Pros of",c,y))+
    coord_flip()
    
    y="2016"
    x<- reviews[reviews$company==c & reviews$dates==y,]
    x<-x[,7]
    text_corpus_pros<-cleaning_corpus(x)
    text_corpus_pros_no_stopwords<-corpus_no_stopwords(text_corpus_pros)
    prosNS_str<-concatenate( lapply ( text_corpus_pros_no_stopwords , "[", 1) )
    twogramsNS_pros<-ngram(prosNS_str,2)
    twogram_prosNS_freqdf<-get.phrasetable ( twogramsNS_pros )
    twogram_prosNS_freqdf<-twogram_prosNS_freqdf[,1:2]
    colnames(twogram_prosNS_freqdf)[1]<-"word"
    dfsub <- subset(twogram_prosNS_freqdf[1:5,])
    plot2 <- ggplot(dfsub, aes(x = reorder(word, freq), y = freq)) +
      geom_bar(stat = "identity",fill="darkturquoise") +
      labs(x = "Words", y = "Frequency") +
      ggtitle(paste("Top 5 Pros of",c,y))+
      coord_flip()
    

    y="2017"
    x<- reviews[reviews$company==c & reviews$dates==y,]
    x<-x[,7]
    text_corpus_pros<-cleaning_corpus(x)
    text_corpus_pros_no_stopwords<-corpus_no_stopwords(text_corpus_pros)
    prosNS_str<-concatenate( lapply ( text_corpus_pros_no_stopwords , "[", 1) )
    twogramsNS_pros<-ngram(prosNS_str,2)
    twogram_prosNS_freqdf<-get.phrasetable ( twogramsNS_pros )
    twogram_prosNS_freqdf<-twogram_prosNS_freqdf[,1:2]
    colnames(twogram_prosNS_freqdf)[1]<-"word"
    dfsub <- subset(twogram_prosNS_freqdf[1:5,])
    plot3 <- ggplot(dfsub, aes(x = reorder(word, freq), y = freq)) +
      geom_bar(stat = "identity",fill="darkturquoise") +
      labs(x = "Words", y = "Frequency") +
      ggtitle(paste("Top 5 Pros of",c,y))+
      coord_flip()
    
    
    y="2018"
    x<- reviews[reviews$company==c & reviews$dates==y,]
    x<-x[,7]
    text_corpus_pros<-cleaning_corpus(x)
    text_corpus_pros_no_stopwords<-corpus_no_stopwords(text_corpus_pros)
    prosNS_str<-concatenate( lapply ( text_corpus_pros_no_stopwords , "[", 1) )
    twogramsNS_pros<-ngram(prosNS_str,2)
    twogram_prosNS_freqdf<-get.phrasetable ( twogramsNS_pros )
    twogram_prosNS_freqdf<-twogram_prosNS_freqdf[,1:2]
    colnames(twogram_prosNS_freqdf)[1]<-"word"
    dfsub <- subset(twogram_prosNS_freqdf[1:5,])
    plot4 <-     ggplot(dfsub, aes(x = reorder(word, freq), y = freq)) +
      geom_bar(stat = "identity",fill="darkturquoise") +
      labs(x = "Words", y = "Frequency") +
      ggtitle(paste("Top 5 Pros of",c,y))+
      coord_flip()
  


plot_grid(plot1, plot2,plot3,plot4, nrow=1,ncol=4)


#Cons
y="2015"
x<- reviews[reviews$company==c & reviews$dates==y,]
#pros 7, cons 8
x<-x[,8]
text_corpus_pros<-cleaning_corpus(x)
text_corpus_pros_no_stopwords<-corpus_no_stopwords(text_corpus_pros)
prosNS_str<-concatenate( lapply ( text_corpus_pros_no_stopwords , "[", 1) )
twogramsNS_pros<-ngram(prosNS_str,2)
twogram_prosNS_freqdf<-get.phrasetable ( twogramsNS_pros )
twogram_prosNS_freqdf<-twogram_prosNS_freqdf[,1:2]
colnames(twogram_prosNS_freqdf)[1]<-"word"
dfsub <- subset(twogram_prosNS_freqdf[1:5,])
plot1 <- ggplot(dfsub, aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity",fill="red") +
  labs(x = "Words", y = "Frequency") +
  ggtitle(paste("Top 5 Cons of",c,y))+
  coord_flip()

y="2016"
x<- reviews[reviews$company==c & reviews$dates==y,]
x<-x[,8]
text_corpus_pros<-cleaning_corpus(x)
text_corpus_pros_no_stopwords<-corpus_no_stopwords(text_corpus_pros)
prosNS_str<-concatenate( lapply ( text_corpus_pros_no_stopwords , "[", 1) )
twogramsNS_pros<-ngram(prosNS_str,2)
twogram_prosNS_freqdf<-get.phrasetable ( twogramsNS_pros )
twogram_prosNS_freqdf<-twogram_prosNS_freqdf[,1:2]
colnames(twogram_prosNS_freqdf)[1]<-"word"
dfsub <- subset(twogram_prosNS_freqdf[1:5,])
plot2 <- ggplot(dfsub, aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity",fill="red") +
  labs(x = "Words", y = "Frequency") +
  ggtitle(paste("Top 5 Cons of",c,y))+
  coord_flip()


y="2017"
x<- reviews[reviews$company==c & reviews$dates==y,]
x<-x[,8]
text_corpus_pros<-cleaning_corpus(x)
text_corpus_pros_no_stopwords<-corpus_no_stopwords(text_corpus_pros)
prosNS_str<-concatenate( lapply ( text_corpus_pros_no_stopwords , "[", 1) )
twogramsNS_pros<-ngram(prosNS_str,2)
twogram_prosNS_freqdf<-get.phrasetable ( twogramsNS_pros )
twogram_prosNS_freqdf<-twogram_prosNS_freqdf[,1:2]
colnames(twogram_prosNS_freqdf)[1]<-"word"
dfsub <- subset(twogram_prosNS_freqdf[1:5,])
plot3 <- ggplot(dfsub, aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity",fill="red") +
  labs(x = "Words", y = "Frequency") +
  ggtitle(paste("Top 5 Cons of",c,y))+
  coord_flip()


y="2018"
x<- reviews[reviews$company==c & reviews$dates==y,]
x<-x[,8]
text_corpus_pros<-cleaning_corpus(x)
text_corpus_pros_no_stopwords<-corpus_no_stopwords(text_corpus_pros)
prosNS_str<-concatenate( lapply ( text_corpus_pros_no_stopwords , "[", 1) )
twogramsNS_pros<-ngram(prosNS_str,2)
twogram_prosNS_freqdf<-get.phrasetable ( twogramsNS_pros )
twogram_prosNS_freqdf<-twogram_prosNS_freqdf[,1:2]
colnames(twogram_prosNS_freqdf)[1]<-"word"
dfsub <- subset(twogram_prosNS_freqdf[1:5,])
plot4 <-     ggplot(dfsub, aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity",fill="red") +
  labs(x = "Words", y = "Frequency") +
  ggtitle(paste("Top 5 Cons of",c,y))+
  coord_flip()



plot_grid(plot1, plot2,plot3,plot4, nrow=1,ncol=4)




