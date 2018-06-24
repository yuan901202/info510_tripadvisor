library(tm)
library(wordcloud)
library(slam)
library(topicmodels)
library(sentiment)
library(plyr)
library(ggplot2)
library(RColorBrewer)

#Load Text
con <- file("~/Desktop/University/2018 T1/review_data/south_attraction_reviews_only.txt", "rt")
reviews = readLines(con)
close(con)

#Clean Text
#reviews = gsub("(RT|via)((?:\b\W*@\w+)+)","",reviews)
reviews = gsub("http[^[:blank:]]+", "", reviews)
#reviews = gsub("@\w+", "", reviews)
reviews = gsub("[ t]{2,}", "", reviews)
#reviews = gsub("^\s+|\s+$", "", reviews)
#reviews = gsub("\d+", "", reviews)
reviews = gsub("[[:digit:]]", "", reviews) # take number out of txt
corpus = Corpus(VectorSource(reviews))
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,stripWhitespace)
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus,removeWords,stopwords('english'))
tdm = DocumentTermMatrix(corpus) # Creating a Term document Matrix

# create tf-idf matrix
term_tfidf <- tapply(tdm$v/row_sums(tdm)[tdm$i], tdm$j, mean) * log2(nDocs(tdm)/col_sums(tdm > 0))
summary(term_tfidf)
tdm <- tdm[,term_tfidf >= 0.1]
tdm <- tdm[row_sums(tdm) > 0,]
summary(col_sums(tdm))

#Deciding best K value using Log-likelihood method
best.model <- lapply(seq(2, 50, by = 1), function(d){LDA(tdm, d)})
best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))

#calculating LDA
k = 10; #number of topics
SEED = 1000; # number of reviews used
CSC_TM <-list(VEM = LDA(tdm, k = k, control = list(seed = SEED)),VEM_fixed = LDA(tdm, k = k,control = list(estimate.alpha = FALSE, seed = SEED)),Gibbs = LDA(tdm, k = k, method = "Gibbs",control = list(seed = SEED, burnin = 1000,thin = 100, iter = 1000)),CTM = CTM(tdm, k = k,control = list(seed = SEED,var = list(tol = 10^-4), em = list(tol = 10^-3))))

#To compare the fitted models we first investigate the values of the models fitted with VEM and estimated and with VEM and fixed
sapply(CSC_TM[1:2], slot, "alpha")
sapply(CSC_TM, function(x) mean(apply(posterior(x)$topics, 1, function(z) - sum(z * log(z)))))
Topic <- topics(CSC_TM[["VEM"]], 1)
Terms <- terms(CSC_TM[["VEM"]], 8)
Terms
write.csv(Terms, file="~/desktop/topics.csv")

for(i in 1:k){
  docs <- reviews[as.data.frame(which(Topic==i))[,1]]
  #do sentiment analysis on docs here
  # define "tolower error handling" function
  try.error = function(x)
  {
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    # result
    return(y)
  }
  # lower case using try.error with sapply
  reviewData = sapply(docs, try.error)
  
  # remove NAs in reviewData
  reviewData = reviewData[!is.na(reviewData)]
  names(reviewData) = NULL
  
  # classify emotion
  class_emo = classify_emotion(reviewData, algorithm="bayes", prior=1.0)
  # get emotion best fit
  emotion = class_emo[,7]
  # substitute NA's by "unknown"
  emotion[is.na(emotion)] = "unknown"
  
  # classify polarity
  class_pol = classify_polarity(reviewData, algorithm="bayes")
  # get polarity best fit
  polarity = class_pol[,4]
  
  # data frame with results
  sent_df = data.frame(text=reviewData, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
  
  # sort data frame
  sent_df = within(sent_df,
                   emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
  
  # save to csv file
  write.csv(sent_df, file = paste0("~/Desktop/topic_",i,"_sent.csv"))
  
  # plot distribution of emotions
  ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of reviews") +
  labs(title = "Sentiment Analysis of reviews about attraction\n(classification by emotion)")
  
  # plot distribution of polarity
  ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of reviews") +
  labs(title = "Sentiment Analysis of reviews about attraction\n(classification by polarity)")
  
  # separating text by emotion
  emos = levels(factor(sent_df$emotion))
  nemo = length(emos)
  emo.docs = rep("", nemo)
  for (i in 1:nemo)
  {
    tmp = reviewData[emotion == emos[i]]
    emo.docs[i] = paste(tmp, collapse=" ")
  }
  
  reviews[as.data.frame(which(Topic==5))[,1]] #show reviews belongs to topic
  
  # remove stopwords
  emo.docs = removeWords(emo.docs, stopwords("english"))
  # create corpus
  corpus = Corpus(VectorSource(emo.docs))
  tdm = TermDocumentMatrix(corpus)
  tdm = as.matrix(tdm)
  colnames(tdm) = emos
}
