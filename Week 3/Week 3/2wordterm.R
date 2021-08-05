library(stringr)
processText <- function(df){
  # transform to lowercase
  df$Description <- tolower(df$Description)
  # remove punctuation
  df$Description <- str_replace_all(df$Description, pattern = "[[:punct:]]", " ")
  # remove numbers
  df$Description <- tm::removeNumbers(df$Description)
  # remove escape characters
  df$Description <- str_replace_all(df$Description,"[\r\n\t]", " ")
  # remove non ASCII characters
  Encoding(df$Description) <- 'UTF-8'
  df$Description <- iconv(df$Description, 'UTF-8', 'ASCII', sub=' ')
  # remove words of 2 or less characters
  df$Description <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", df$Description)
  df$Description <- gsub("u.s.", " america ", df$Description)
  df$Description <- gsub("&amp", " ", df$Description)
  df$Description <- gsub("@\\w+", " ", df$Description)
  df$Description <- gsub("http[^[:space:]]*", " ", df$Description)
  df$Description <- gsub("[[:punct:]]", " ", df$Description)
  
  # remove stopwords and own defined stopwords
  df$Description <- tm::removeWords(x = df$Description, stopwords('english'))
  df$Description <- tm::removeWords(x = df$Description, c('room', 'bedroom', 'viewing', 'house', 'home', 'rent', 'sale', 'call', 'unit', 'please', 'kindly'))
  # trim extra whitespaces
  df$Description <- gsub("\\s+", " ", df$Description)
  df$Description <- trimws(df$Description)
  # stemming document
  # use wordStem("words") to test root word
  df$Description <- stemDocument(df$Description, language= 'english')  
  
  return(df)
}

BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

df <- tweets
df$Description <- df$text
df <- processText(df)
tdm3  <- TermDocumentMatrix(VCorpus(VectorSource(df$Description)), control = list(tokenize = BigramTokenizer))
m3 <- as.matrix(tdm3)
v3 <- sort(rowSums(m3), decreasing = TRUE)

termFreq = rowSums(as.matrix(tdm3))
termFreq = subset(termFreq, termFreq>=50)
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
  ggtitle("Most Frequent 2 word term")