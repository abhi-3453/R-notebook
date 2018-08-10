## INTITIALZING LIBRARIES ###
library(twitteR)
library(tm)
library(wordcloud)
### INITIALIZING KEYS###
ckey = '0kTqJDKE2c8TdywghKlySEzv4'
skey = 'mhyEoGiHojIYPu2sMqJKcQdgDg0hUGKn7YhLJ7HugR8dbbs8ec'
tok= '793381580144271360-1ozqmrRDMTNIm7ROFnMWReX9Zv5d5LE'
toksec= 'kVAD0H5egYEEBQvIQoGa88KGo6L8GKK76f01cUutjwOTH'

###SETTING UP TWITTER ###
setup_twitter_oauth( consumer_key =  ckey,consumer_secret =  skey ,access_token =  tok ,access_secret =  toksec)

### STORING TWEETS ###
ai_tweets= searchTwitter('artificial intelligence',n=300,lang = 'en')
ai_text= sapply(ai_tweets, function(x) x$getText())

### CONVERTIG FROM 'UTF-8' TO 'ASCII'###
ai_text <- iconv(ai_text,"UTF-8","ASCII")

### CREATING A CORPUS ###  
ai.corpus <- Corpus(VectorSource(ai_text))

###CREATING A DOCUMENT MATRIX ###
ai.matrix = TermDocumentMatrix(ai.corpus,
                               control= list(removePunctuation=TRUE,
                                             stopwords= c('artificial intelligence', stopwords('english')),
                                             removeNumbers= TRUE,
                                             tolower= TRUE))
ai_doc_mat =  as.matrix(ai.matrix)

### SORTING FREQUENCIES ###
wordfreq <- sort(rowSums(ai_doc_mat), decreasing = T)
dm <- data.frame(word= names(wordfreq), freq=wordfreq )

### CREATING A WORDCLOUD ###
wordcloud(dm$word,dm$freq,random.order = FALSE,colors = brewer.pal(7,'Dark2'))
