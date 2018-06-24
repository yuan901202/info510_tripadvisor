# required pakacges
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(beepr) # add notification sound

# read review data
#reviewData <- read.csv("~/Desktop/University/2018 T1/review_data/north_attraction_reviews.csv", header=TRUE, sep=",")[ ,c('Review_Content')]
reviewData <- read.delim("~/Desktop/University/2018 T1/review_data/nz_attraction_reviews.txt", quote="")

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
reviewData = sapply(reviewData, try.error)

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

#save sentiment analysis to csv
write.csv(sent_df, file="~/Desktop/University/2018 T1/review_resutls/nz_attraction/nz_sent.csv")

#save exact emotion and polarity number
write.table(table(emotion), "~/Desktop/University/2018 T1/review_resutls/nz_attraction/emotion.txt", sep="\t", row.names=FALSE)
write.table(table(polarity), "~/Desktop/University/2018 T1/review_resutls/nz_attraction/polarity.txt", sep="\t", row.names=FALSE)

# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
geom_bar(aes(y=..count.., fill=emotion)) +
scale_fill_brewer(palette="Dark2") +
labs(x="emotion categories", y="number of reviews") +
labs(title = "Sentiment Analysis of reviews about New Zealand attraction\n(classification by emotion)")

# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
geom_bar(aes(y=..count.., fill=polarity)) +
scale_fill_brewer(palette="RdGy") +
labs(x="polarity categories", y="number of reviews") +
labs(title = "Sentiment Analysis of reviews about New Zealand attraction\n(classification by polarity)")

# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
    tmp = reviewData[emotion == emos[i]]
    emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),scale = c(3,.5), random.order = FALSE, title.size = 1.5)

beep(1)
beep(2)
beep(3)
beep(4)
beep(5)