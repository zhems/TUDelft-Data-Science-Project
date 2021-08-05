library(koRpus)
library(qdapDictionaries)
library(qdap);library(stopwords);library(SnowballC)
library(tm)
library(ggplot2)
library(wordcloud)
library(stm)

tweets=read.csv('tweets.csv',header=T, quote = "", row.names = NULL,stringsAsFactors = FALSE)
attach(tweets)

myUneditedCorpus = Corpus(VectorSource(text))

###TRANSFORMING TEXT
# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(text))
# convert to lower case # myCorpus <- tm_map(myCorpus, tolower)
# tm v0.6
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# replace U.S. with USA
replaceUS <- function(x) gsub("u.s.", " america ", x)
# replace punctuation with space
removePunc <- function(x) gsub("[[:punct:]]", " ", x)
# remove &amp
removeAndSymbol <- function(x) gsub("&amp", " ", x)
# remove username
removeUsername <- function(x) gsub("@\\w+", " ", x)
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", " ", x)
# tm v0.6
myCorpus <- tm_map(myCorpus, content_transformer(removeAndSymbol))
myCorpus <- tm_map(myCorpus, content_transformer(removeUsername))
myCorpus <- tm_map(myCorpus, content_transformer(replaceUS))
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
# myCorpus <- tm_map(myCorpus, content_transformer(removePunc))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
# add two extra stop words: "available" and "via"
myStopwords <- c(letters, "available", "via", "rt", stopwords("english"))
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
  PlainTextDocument(stripWhitespace(paste(stemCompletion(gsub("hillari", "hillary", unlist(strsplit(as.character(x)," "))),dictionary=dict, type="prevalent"),sep="", collapse=" ")))
}

myCorpus2 <- lapply(myCorpus, stemCompletion_mod, dict = myCorpusCopy)
dataframe <- data.frame(text=unlist(sapply(myCorpus2, `[`, "content")), 
                        stringsAsFactors=F)


myCorpus3 = Corpus(VectorSource(dataframe$text))

inspect(myCorpus3[11:15])



### TERM DOC MAT
tdm <- TermDocumentMatrix(myCorpus3, control=list(wordLengths=c(1,Inf)))
tdm
idx <- which(dimnames(tdm)$Terms == "r")
inspect(tdm)
tdmUnedited = TermDocumentMatrix(myUneditedCorpus, control=list(wordLengths=c(1,Inf)))
tdmUnedited

### TERM DOC MAT 2 (without word completion)
tdm2  <- TermDocumentMatrix(myCorpus)
tdm2

###WORD FREQ unedited vs processed text
findFreqTerms(tdm,lowfreq=190)             #most freq words after processing
findFreqTerms(tdmUnedited,lowfreq=290)     #most freq words originally (take note stopwords are most freq)
inspect(myCorpus3[20:24])                  #tweets after processing
inspect(myCorpus[20:24])                   #tweets after stemming, before stem completion
inspect(myUneditedCorpus[20:24])           #original tweets

# Most frequent terms for stem completed words
termFreq = rowSums(as.matrix(tdm))
termFreq = subset(termFreq, termFreq>=190)
df <- data.frame(term=names(termFreq), freq=termFreq)
df <- transform(df, term = reorder(term, freq))
ggplot(df, aes(x=term, y=freq, fill=freq)) + 
  geom_bar(stat="identity") + 
  xlab("Terms") + ylab("Count") + coord_flip()  +
  geom_text(aes(x=term, y=freq,label=freq), colour="black", fontface = 'bold', family = 'sans', size=4, hjust=-0.2) + 
  scale_fill_gradient(name = legend, high = 'dodgerblue', low = 'deepskyblue', guide = FALSE) + 
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.x=element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5), axis.line = element_line(colour = "black")) +
  ggtitle("Most Frequent Words")

# Most frequent terms for non stem completed words
termFreq2 = rowSums(as.matrix(tdm2))
termFreq2 = subset(termFreq2, termFreq2>=190)
df <- data.frame(term=names(termFreq2), freq=termFreq2)
df <- transform(df, term = reorder(term, freq))
ggplot(df, aes(x=term, y=freq, fill=freq)) + 
  geom_bar(stat="identity") + 
  xlab("Terms") + ylab("Count") + coord_flip()  +
  geom_text(aes(x=term, y=freq,label=freq), colour="black", fontface = 'bold', family = 'sans', size=4, hjust=-0.2) + 
  scale_fill_gradient(name = legend, high = 'dodgerblue', low = 'deepskyblue', guide = FALSE) + 
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.x=element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(), axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5), axis.line = element_line(colour = "black")) +
  ggtitle("Most Frequent Words (not stem completed)")

###WORD CLOUD
m = as.matrix(tdm)
# calculate the frequency of words and sort it descendingly by frequency
wordFreq <- sort(rowSums(m), decreasing=TRUE)
# colors
pal <- brewer.pal(9, "GnBu")
pal <- pal[-(1:4)]
# word cloud 
set.seed(375) # to make it reproducible
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=50, random.order=F, 
          colors=pal)

# remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse=0.95)
m2 <- as.matrix(tdm2)
##k-means
# transpose the matrix to cluster documents (tweets)
m3 <- t(m2)
# set a fixed random seed
set.seed(122)

#Elbow Method for finding the optimal number of clusters
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- m2
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
print(wss)
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# k-means clustering of tweets
k <- 6
kmeansResult <- kmeans(m3, k)
# cluster centers
round(kmeansResult$centers, digits=3)

for (i in 1:k) {
  cat(paste("cluster ", i, ":  ", sep=""))
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:3], "\n")
  
  # print the tweets of every cluster
  # print(tweet[which(kmeansResult$cluster==i)]) 
}

##hierarchical clustering
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="ward.D")
plot(fit)
# cut tree into 6 clusters
rect.hclust(fit, k=6)
(groups <- cutree(fit, k=6))



###STM
tweets2=tweets
processed = textProcessor(tweets2$text, metadata = tweets2)
plotRemoved(processed$documents, lower.thresh = seq(1, 40, by = 1))
out <- prepDocuments(processed$documents,processed$vocab,processed$meta,
                     lower.thresh = 2)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta
if(length(out$docs.removed)>0){
  tweets2 <- tweets2[-out$docs.removed,]
}
storage <- searchK(out$documents, out$vocab, K = seq(5,20,by=1),
                    data = meta)

ggplot(data=storage$results, aes(semcoh,exclus,
                                 label=5:20)) + geom_text(size=8) +
  ylab("Exclusivity") + xlab("Semantic Coherence") +
  theme(axis.title.x = element_text(vjust=-0.5),axis.text=element_text(size=16)) + 
  theme(axis.title.y = element_text(vjust=1.3),axis.title=element_text(size=16))
#9, 12, 20

shortdocs = tweets2$text

fit9 <- stm(out$documents, out$vocab, K = 9,
            max.em.its = 120,	data = out$meta, init.type="Spectral")
labelTopics(fit9,n=15)
findThoughts(fit9,texts = shortdocs, n = 10,topics = 9)

label <- c('Topic 1: Super Tuesday','Topic 2: Campaign',
           'Topic 3: Badmouthing Others (Rubio, Obama, Clinton, Democrats)','Topic 4: Thanking Republican Supporters',
           'Topic 5: FBI and Scandals','Topic 6: Trump Slogans and Rhetoric (MAGA)',
           'Topic 7: Republicans in News/Interviews','Topic 8: Policies (Border wall, Jobs)',
           'Topic 9: Trump & Megyn Kelly Feud')

## Topic Proportion
TopicMeans <- matrix(0,9,1)
for(i in 1:9){
  TopicMeans[i,] <- mean(fit9$theta[,i])
}
colnames(TopicMeans) <- 'Frequency'
rownames(TopicMeans) <- label
TopicMeans <- as.data.frame(TopicMeans)
RoundedTopicMeans=round(TopicMeans,2)
topicCorr(fit9)
plot(topicCorr(fit9))

par(mfrow=c(3,3))
for(i in 1:9) {
  cloud(fit9, topic = i, scale = c(4,.65), color='navy', max.words = 40)
}


