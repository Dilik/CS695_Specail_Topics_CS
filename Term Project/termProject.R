# R scripts for CS695 term project. 
# Every group member should contribute to this scripts
# I will track and grade student contribution through Git

# 1 - Load data (MD)

# Install necessary packages

install.packages('tm')
install.packages('RColorBrewer')
install.packages('wordcloud')
install.packages("readr")
install.packages("tm")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("plyr")
install.packages("stringr")
install.packages("googleVis")
install.packages("stringi")
install.packages("magrittr")
install.packages("dplyr")
install.packages("mclust")
install.packages("igraph")
install.packages("sna")
source("http://bioconductor.org/biocLite.R")
biocLite("RBGL")
biocLite("graph")


library('tm')
library('RColorBrewer')
library('wordcloud')
library("googleVis")
library("plyr")
library(readr)
library(mclust)
library(sna)
library(graph)
library(igraph)
library("stringr")
library("stringi")
library("magrittr")
library("dplyr")

# 2 - Word Cloud (MD)

# Read data in a R data object
UniDaysdata <- readRDS("Unidays.rds")
tweets <- UniDaysdata$MESSAGE_BODY


# Function to clean tweets
clean.text = function(x)
{
      # remove rt
      x = gsub("rt", "", x)
      # remove at
      x = gsub("@\\w+", "", x)
      # remove punctuation
      x = gsub("[[:punct:]]", "", x)
      # remove numbers
      x = gsub("[[:digit:]]", "", x)
      # remove links http
      x = gsub("http\\w+", "", x)
      # remove tabs
      x = gsub("[ |\t]{2,}", "", x)
      # remove blank spaces at the beginning
      x = gsub("^ ", "", x)
      # remove blank spaces at the end
      x = gsub(" $", "", x)
      return(x)

}

# clean tweets
tweets = clean.text(tweets)




#********************************************
#         Word Cloud
#********************************************
corpus = Corpus(VectorSource(tweets))
corpus = Corpus(VectorSource(cmail))
# create term-document matrix
tdm = TermDocumentMatrix(
  corpus,
    control = list(
        wordLengths=c(3,20),
	    removePunctuation = TRUE,
	        stopwords = c("the", "a", stopwords("english")),
		    removeNumbers = TRUE, tolower = FALSE) )

# convert as matrix
tdm = as.matrix(tdm)

# get word counts in decreasing order
word_freqs = sort(rowSums(tdm), decreasing=TRUE)

#check top 50 most mentioned words
head(word_freqs, 50)

#remove the top words which don’t generate insights such as "the", "a", "and", etc.
word_freqs = word_freqs[-(1)]  #Here “1” is 1st word in the list we want to remove

# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

#Plot corpus in a clored graph; need RColorBrewer package
wordcloud(head(dm$word, 50), head(dm$freq, 50), random.order=FALSE, colors=brewer.pal(8, "Dark2"))




# 3 - Topic Classification ( Ekhlasur) 
 


# 4 - Sentiment Analysis (Ephraim)



# 5 - User Profile (Monica & MD)



# 6 - Network Analysis (Satish)

