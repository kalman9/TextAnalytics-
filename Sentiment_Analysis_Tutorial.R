#Install Dependencies 
library("devtools", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
require(devtools)
library("Rstem", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("sentiment", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("wordcloud", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("RColorBrewer", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("tm", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")
library("SnowballC", lib.loc="/Library/Frameworks/R.framework/Versions/3.3/Resources/library")

#Load Data 
trump <- readLines('/Users/Kalman/Documents/SENIOR/Business Analytics/project/Speeches/Trump (R)')
df <- data.frame(trump)
trumpData <- df[df$trump, ]

#Clean: remove what isn't necessary (i.e. not words)
trumpData = gsub("[[:punct:]]", "", trumpData)
trumpData = gsub("[[:digit:]]", "", trumpData)
trumpData = gsub("http\\w+", "", trumpData)
trumpData = gsub("[ \t]{2,}", "", trumpData)
trumpData = gsub("^\\s+|\\s+$", "", trumpData)
try.error = function(x) {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
}
trumpData = sapply(trumpData, try.error)
trumpData = trumpData[!is.na(trumpData)]
names(trumpData) = NULL

#Sentiment Analysis 
class_emo = classify_emotion(trumpData, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(trumpData, algorithm="bayes")
polarity = class_pol[,4]

sent_df = data.frame(text=trumpData, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

#Graph Emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="")

#Graph Polarities
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="")

#Create Word Cloud
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = trumpData[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}
emo.docs = removeWords(emo.docs, stopwords("english"))
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.25), random.order = FALSE,
                 title.size = 1, max.words=500)
