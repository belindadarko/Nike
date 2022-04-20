# Case 1 Text Analytics
# February 2020
# Belinda Darko

# To limit errors please run this code
Sys.setlocale('LC_ALL','C')

#### 
# set your working directory
setwd("/Users/belindadarko/Documents/Skole relatert/MSBAN/Text Analytics/Text-Mining-NLP copy/Case/Case I/Data")

# Importing Data
# Choosing these months due to all-star game & championship
Feb20 <-read.csv('E_Feb2020.csv', encoding = "UTF-8")


#Loading packages
library(ggplot2)
library(ggthemes)
library(stringi)
library(tm)
library(dplyr)
library(stringi)
library(wordcloud)
library(RColorBrewer)
library(qdap)
library(ggdendro)

# The most tweets in the whole month of Feb 20 was related to team Miami Heat,
# then Boston Celtics and LA Lakers. This number will change once I sample the data.
sort(table(Feb20$team), decreasing=TRUE)

# checking the frequency for lebron who was the team captain for west team in allstar game
lebron    <- grepl("lebron", Feb20$text, ignore.case=TRUE)

# He shows up 1.6 % of the tweets
sum(lebron) / nrow(Feb20)

# checking the frequency for giannis who was the team captain for east in allstar game
giannis    <- grepl("giannis", Feb20$text, ignore.case=TRUE)

# He shows up 1.2 % of the tweets
sum(giannis) / nrow(Feb20)

# lebron is therefore mentioned more than giannis

#text strings will not be factors of categories
options(stringsAsFactors = FALSE)

#making the tweets all lower case
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# this finds the correlation between the words
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# cleaning up the tweets by removing punctuations, numbers etc
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}


# creating stopwords to eliminate from analysis
stopwords <- c(stopwords("SMART"), "la lakers", "kobe bryant",
               "chigago", "tickets", "all star",
               "team", "best",
               "season", "finals", "announcement", "lebron",
               "giannis", "win", "cheat","rigged", "points", "foul", "th",
               "for", "and", "with", "lol")

# Samling the data for R to run
Feb20sample<- slice_sample(Feb20, prop = 0.004)

#Subset the tweets of the most popular teams for Feb20
topteams_feb <- subset(Feb20sample, team %in% c("Miami Heat", "Boston Celtics", "LA Lakers"))

#Clean and Organize the tweets
txtCorpus <- VCorpus(VectorSource(topteams_feb$text))
txtCorpus <- cleanCorpus(txtCorpus, stopwords)
fantweetsTDM  <- TermDocumentMatrix(txtCorpus)
fantweetsTDMs <- as.matrix(fantweetsTDM)

#to fix vector memory exhaustion, run this code
#Sys.setenv('R_MAX_VSIZE'=32000000000)

# Inspect word associations
associations_allstar <- findAssocs(fantweetsTDM, 'nike', 0.30)
associations_allstar

# Organize the word associations
assocDF <- data.frame(terms=names(associations_allstar[[1]]),
                      value=unlist(associations_allstar))

assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF


# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#FF0000') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="black",hjust="inward", vjust ="inward" , size=3) 


# Make a volatile corpus
txtCorpus <- VCorpus(DataframeSource(Feb20sample))

# Preprocess the corpus
Corpustxt <- cleanCorpus(txtCorpus, stopwords)

# Make bi-gram TDM according to the tokenize control & convert it to matrix
tweetsTDM  <- TermDocumentMatrix(Corpustxt, 
                                 control=list(tokenize=bigramTokens))
tweetsTDMm <- as.matrix(tweetsTDM)


# Get Row Sums & organize
fantweetsTDMt <- sort(rowSums(tweetsTDMm), decreasing = TRUE)
fantweets_DF   <- data.frame(word = names(fantweetsTDMt), freq = fantweetsTDMt)

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(fantweets_DF$word,
          fantweets_DF$freq,
          max.words    = 200,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

#Creating dendrogram
# Reduce TDM
reducedTDM <- removeSparseTerms(fantweetsTDM, sparse=0.985) #shoot for ~50 terms; 1.5% of cells in row have a value  
reducedTDM

# Organize the smaller TDM
reducedTDM <- as.data.frame(as.matrix(reducedTDM))

# Basic Hierarchical Clustering
hc <- hclust(dist(reducedTDM))
plot(hc,yaxt='n')

ggdendrogram(hc, rotate=FALSE) 

# End