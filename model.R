library(data.table)
library(quanteda)

setwd("D:/Development/Repos/datasciencecoursera/Capstone project")
set.seed(12345)

source("./functions.R")


files <- data.frame(
  name = c("Twitter", "News", "Blogs"),
  location = c("./final/en_US/en_US.twitter.txt", "./final/en_US/en_US.news.txt", "./final/en_US/en_US.blogs.txt"),
  randomSampleSize = c(100000, 100000, 100000),
  stringsAsFactors = F
)


files <- data.frame(
  name = c("Twitter"),
  location = c("./final/en_US/en_US.twitter.txt"),
  randomSampleSize = c(5000),
  stringsAsFactors = F
)

saveProbabilities(files, maxGram = 5)

accuracyTest(files, maxGram = 2, verboseLevel = 1)


#nGramDataTables <- saveProbabilities(files, maxGram = 4)
#probabilities <- createProbabilities(nGramDataTables)