library(koRpus)
library(qdapDictionaries)
library(qdap);library(stopwords);library(SnowballC)
library(tm)
library(ggplot2)
library(wordcloud)

tweets=read.csv('tweets.csv',header=T, quote = "", row.names = NULL,stringsAsFactors = FALSE)
attach(tweets)

myUneditedCorpus = Corpus(VectorSource(text))

###TRANSFORMING TEXT
# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(text))
# convert to lower case # myCorpus <- tm_map(myCorpus, tolower)
# tm v0.6
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
# tm v0.6
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
# add two extra stop words: "available" and "via"
myStopwords <- c(stopwords("english"), "available", "via")
# remove "r" and "big" from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)


###STEMMING WORDS
# keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)
# inspect documents (tweets) numbered 11 to 15
inspect(myCorpus[11:15])

stemCompletion_mod <- function(x,dict=dictCorpus) {
  PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),dictionary=dict, type="shortest"),sep="", collapse=" ")))
}

myCorpus2 <- lapply(myCorpus, stemCompletion_mod, dict = myCorpusCopy)

dataframe <- data.frame(text=unlist(sapply(myCorpus2, `[`, "content")), 
                        stringsAsFactors=F)

# tweets2=tweets
# tweets[[2]]=dataframe$text
# 
# write.csv(tweets, file='tweetsprocessed.csv')
# 
# tp = read.csv('tweetsprocessed.csv',header=T, quote = "", row.names = NULL,stringsAsFactors = FALSE)

myCorpus3 = Corpus(VectorSource(dataframe$text))

inspect(myCorpus3[11:15])

# # # tm v0.6
# stemCompletion2 <- function(x, dictionary) {
#   x <- unlist(strsplit(as.character(x), " "))
#   # Unexpectedly, stemCompletion completes an empty string to
#   # a word in dictionary. Remove empty string to avoid above issue.
#   x <- x[x != ""]
#   x <- stemCompletion(x, dictionary=dictionary)
#   x <- paste(x, sep="", collapse=" ")
#   PlainTextDocument(stripWhitespace(x))
# }
# myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
# 
# myCorpus2 <- Corpus(VectorSource(myCorpus))
# 
# inspect(myCorpus2[11:15])
# 
# myCorpus <- Corpus(VectorSource(myCorpus))
# 
# inspect(myCorpus[11:15])


### TERM DOC MAT
tdm <- TermDocumentMatrix(myCorpus3, control=list(wordLengths=c(1,Inf)))
tdm
idx <- which(dimnames(tdm)$Terms == "r")
inspect(tdm[idx+(0:5),101:110])
tdmUnedited = TermDocumentMatrix(myUneditedCorpus, control=list(wordLengths=c(1,Inf)))
tdmUnedited

###WORD FREQ unedited vs processed text
findFreqTerms(tdm,lowfreq=190)             #most freq words after processing
findFreqTerms(tdmUnedited,lowfreq=290)     #most freq words originally (take note stopwords are most freq)
inspect(myCorpus3[20:24])                  #tweets after processing
inspect(myUneditedCorpus[20:24])           #original tweets

termFreq = rowSums(as.matrix(tdm))
termFreq = subset(termFreq, termFreq>=190)
df <- data.frame(term=names(termFreq), freq=termFreq)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") + 
  xlab("Terms") + ylab("Count") + coord_flip()

###WORD CLOUD
m = as.matrix(tdm)
# calculate the frequency of words and sort it descendingly by frequency
wordFreq <- sort(rowSums(m), decreasing=TRUE)
# colors
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]
# word cloud
set.seed(375) # to make it reproducible
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F, 
          colors=pal)
