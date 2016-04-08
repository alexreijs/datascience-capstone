library(data.table)
library(quanteda)
#library(tm)

source("functions.R")


files <- data.frame(
  name = c("Twitter", "News", "Blogs"),
  location = c("en_US.twitter.txt", "en_US.news.txt", "en_US.blogs.txt"),
  randomSampleSize = c(50000, 50000, 50000),
  stringsAsFactors = F
)

nGramDataTables <- preProcessLines(files, maxGram = 5)
probabilities <- createProbabilities(nGramDataTables)
predictNextWords("i am", probabilities)
