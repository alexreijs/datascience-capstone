library(data.table)
library(quanteda)

source("functions.R")


files <- data.frame(
  name = c("Twitter", "News", "Blogs"),
  location = c("en_US.twitter.txt", "en_US.news.txt", "en_US.blogs.txt"),
  randomSampleSize = c(75000, 75000, 50000),
  stringsAsFactors = F
)


files <- data.frame(
  name = c("Twitter"),
  location = c("en_US.news.txt"),
  randomSampleSize = c(5000),
  stringsAsFactors = F
)

saveProbabilities(files, maxGram = 5)

accuracyTest(files, maxGram = 5)


#nGramDataTables <- saveProbabilities(files, maxGram = 4)
#probabilities <- createProbabilities(nGramDataTables)