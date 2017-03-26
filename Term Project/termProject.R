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
library("RColorBrewer")
library("wordcloud")
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
  # remove unicode
  x = gsub("/[\ud800-\udfff]/g", "", x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove hashtag
  x = gsub("#\\w+", "", x)
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
  # tolower
  x = tolower(x)
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
 control = list(wordLengths=c(3,20),
 removePunctuation = TRUE,
 stopwords = c("the", "a", stopwords("english")),
 removeNumbers = TRUE, tolower = TRUE) )

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

#********************************************
#         Sentiment Analysis
#********************************************
#R's c() function (for "combine") allows us to add a few industry- and Twitter-specific terms to form our final pos.words and neg.words vectors:

pos.words = scan('positive-words.txt', what='character', comment.char=';')
neg.words = scan('negative-words.txt', what='character', comment.char=';')

neg.words = c(neg.words, 'wtf', 'fail')

#Implementing our sentiment scoring algorithm
require(plyr)
require(stringr)
require(stringi)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    #sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

sentiment.scores= score.sentiment(tweets, pos.words, neg.words, .progress='text')

score= sentiment.scores$score
hist(score)
score = subset(score,score!=0)
sentiscore = data.frame(score)

#topic.scores= score.topic(tweets, sports.words, .progress='text')
#topic.mentioned = subset(topic.scores, score !=0)

#N= nrow(topic.scores)
#Nmentioned = nrow(topic.mentioned)

#dftemp=data.frame(topic=c("Mentioned", "Not Mentioned"), 
#                 number=c(Nmentioned,N-Nmentioned))

Pos= nrow(sentiment.scores)
Neg= nrow(sentiscore)
Ntrl= nrow(NULL)

dftemp=data.frame(words=c("Positive", "Negative", "Neutral"),
                  number=c(Neg,Pos-Neg,!Pos||Neg))

library("googleVis")
Pie<- gvisPieChart(dftemp, options=list(
  legend="{ position: 'top', maxLines:2 }",
  colors="['#5C3292', '#1A8763', '#871B47']",
  width=400, height=360))
plot(Pie)



# 5 - User Profile (Monica & MD)

#********************************************
#         Create a Customer Profile
#********************************************

UniDays$gender <- tolower(UniDays$USER_GENDER)
genderSum = data.frame(table(UniDays$gender))

Bar1 <- gvisBarChart(genderSum,
  options = list(hAxes="[{title:'Popularity', titleTextStyle: {color: 'green'}}]"
    , vAxes="[{title:'Gender', titleTextStyle: {color: 'blue'}}]"))

plot(Bar1)



# 6 - Network Analysis (Satish)
#********************************************
#         Social Network Analysis
#********************************************

tweets = as.character(tweets)
screenname = UniDays$USER_SCREEN_NAME
screenname = as.character(screenname)
write.csv(cbind(screenname,tweets), "tweets.csv")

# Generate edge list from tweets
source("createList.R")
retweeterPoster <- createList("tweets.csv")

# Create graph
m <- ftM2adjM(ft = as.matrix(retweeterPoster[, 1:2]), W = retweeterPoster$weight, edgemode = "directed")
g1 <- as(m, "graphNEL")

# Plots initial graph
gplot(m, gmode = "graph", 
      label = nodes(g1),
      label.cex = 0,
      vertex.col = "#A63603",
      vertex.enclose = FALSE,
      edge.col = "#CCCCCC",
      vertex.cex = 2, 
      main = "Original graph",
      cex.main = 1)

# Prune graph 
# Original graph has too many small clusters, so we need to exclude small clusters
clust <- igraph::components(graph_from_adjacency_matrix(m), mode = "weak")
table(clust$csize)
# The largest cluster contains 891 nodes, so we create a graph of it
large_clusters <- which(clust$csize > 800)
selected_nodes <- names(clust$membership[clust$membership %in% large_clusters])
selected_nodes <- which(rownames(m) %in% selected_nodes)
m2 <- m[selected_nodes, selected_nodes]
gfrom2 <- graph_from_adjacency_matrix(m2)
g2 <- as(m2, "graphNEL")

# centrality measurements of new graph
require(sna) # to mask centrality functions
central <- data.frame(nodes(g2))
central$betweenness <-  sna::betweenness(m2)
central$degree <- sna::degree(m2)
sortlist <- central[order(-central$degree),]
head(sortlist, 10)


#**************************
# Plot Pruned Graph
# The largest cluster still has too many nodes, so we only label
# the most important ones based on centrality scores
#***************************

# Clasterize betweenness values to get groups of nodes 
central %<>%
  mutate(size = log(central$betweenness)) %>%
  mutate(size = ifelse(size == -Inf, 1, size))

# Number of groups for colors
N <- 9
# Colors for nodes
pal <- brewer.pal(N, "Oranges")

# Defines clusters for nodes in groups with different colors
central %<>%
  mutate(group = Mclust(size, G = N)$classification,
         color = pal[group])

# Removes labels for small nodes
# central$label = as.character(central$nodes.g2.)
# central$label[central$group < 7] = ''
central %<>%
  mutate(label = ifelse(group < 7, "", as.character(central$nodes.g2.)))

# Updates node sizes
central %<>%
  mutate(size = ifelse(group == N, 5, ifelse(group < 3, 1, 2)))

# Arranges vertexes by m2
indx <- plyr::laply(colnames(m2), function(i) {which(central$nodes.g2. == i)})
central <- central[indx, ]


# Plot function
PlotGraph <- function(m, colors, sizes, labels, filename, title = "") {
  m[m == Inf] <- 0
  png(filename = filename, width = 2048, height = 2048)
  gplot(m, gmode = "graph", 
        label = labels,
        label.cex = 2,
        vertex.col = colors,
        vertex.enclose = FALSE,
        edge.col = "#CCCCCC",
        vertex.cex = sizes, 
        main = title,
        cex.main = 4)
  dev.off()
}

# Plot graph with by centrality scores and save the image
set.seed(1)
PlotGraph(m2, 
          colors = central$color,
          sizes = central$size,
          labels = central$label,
          filename = "bybBetwenness.png",
          title = "Pruned Graph by Centrality")




