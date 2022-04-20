# Case 1 Text Analytics
# September & October 2020
# Belinda Darko

# To limit errors please run this code
Sys.setlocale('LC_ALL','C')

#### 
# set your working directory
setwd("/Users/belindadarko/Documents/Skole relatert/MSBAN/Text Analytics/Text-Mining-NLP copy/Case/Case I/Data")

# Importing Data
# Choosing these months due to playoffs & championship
Sept20 <-read.csv('L_Sep2020.csv', encoding = "UTF-8")
Oct20 <- read.csv('M_Oct2020.csv', encoding = "UTF-8")

#making these two months into a dataframe so I can make merge both data into one
Sept20x <- data.frame(Sept20)
Oct20x <- data.frame(Oct20)

champs <- rbind(Sept20x, Oct20x)

#Loading packages
library(ggplot2)
library(ggthemes)
library(stringi)
library(tm)
library(dplyr)
library(stringi)
library(wordcloud)
library(wordcloud)
library(RColorBrewer)
library(qdap)


# The most tweets in Sept & Oct 20 was related to team Miami Heat,
# then LA Lakers, then Boston Celtics.
sort(table(champs$team), decreasing=TRUE)

# Deciding to narrow down to the month of September due to play offs
# trying to find the frequency for rookies in the tweets that played in playoffs
# checking for Jae'Sean Tate who plays for Houston Rockets
tate    <- grepl("Tate", Sept20$text, ignore.case=TRUE)

# He shows up 4 % of the tweets
sum(tate) / nrow(Sept20)

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
stopwords <- c(stopwords("SMART"), "la lakers", "miami heat", "boston celtics",
               "tickets", "championship", "disney", "bubble", "playoffs","team", "best",
               "season", "finals", "announcement", "jamal",
               "erik", "tyler", "lebron", "anthony", "win", "cheat","rigged", 
               "points", "foul", "th",
               "for", "and", "with", "lol")

# Samling the data for R to run
champs_sample<- slice_sample(champs, prop = 0.004)

#Subset the tweets of the most popular teams for Sept & Oct 20
topteams_champs <- subset(champs_sample, team %in% c("Miami Heat", "LA Lakers", "Boston Celtics"))


#Clean and Organize the tweets
txtCorpus <- VCorpus(VectorSource(topteams_champs$text))
txtCorpus <- cleanCorpus(txtCorpus, stopwords)
fantweetsTDM  <- TermDocumentMatrix(txtCorpus)
fantweetsTDMs <- as.matrix(fantweetsTDM)

#to fix vector memory exhaustion, run this code
#Sys.setenv('R_MAX_VSIZE'=32000000000)

# Inspect word associations
associations_champs <- findAssocs(fantweetsTDM, 'champ', 0.30)
associations_champs

# Organize the word associations
assocDF <- data.frame(terms=names(associations_champs[[1]]),
                      value=unlist(associations_champs))

assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF


# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#FFC0CB') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="black",hjust="inward", vjust ="inward" , size=3) 


# Make a volatile corpus
txtCorpus <- VCorpus(DataframeSource(champs_sample))

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
pal <- brewer.pal(8, "RdPu")
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