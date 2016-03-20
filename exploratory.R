library(tm)
library(RWeka)
#library(graph)
#library(Rgraphviz)
library(wordcloud)

source("./functions.R")

lines <- suppressWarnings(loadLines("./final/en_US/en_US.twitter.txt", 5000))
#lines <- rbind(lines, loadLines("./final/en_US/en_US.blogs.txt", randomSampleSize = 5000))
#lines <- rbind(lines, loadLines("./final/en_US/en_US.news.txt", randomSampleSize = 5000))
corpus <- createCorpus(lines)

n <- 1

dataFrame <- createWekaNGramDataFrame(corpus, n)
createWordCloud(dataFrame, 50, c(3, 1))
#plot(wekaDataFrame[1:100, 2], type = "o")



wordsUsed <- mapply(wordsNeeded, 1:100 / 100, MoreArgs = list(dataFrame = dataFrame))
wordsUsed <- round(wordsUsed / nrow(dataFrame) * 100, 2)


plot(x = wordsUsed, y = 1:100, type = "l",
     main = "How many words does it take to cover the entire body of text?",
     xlab = "Percentage of words used", ylab = "Percentage of frequency covered")
#dtm <- createDtmNgram(corpus, n)
#matrix <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
#dtmDataFrame <- data.frame(word = names(matrix), freq = matrix)   
#createWordCloud(dtmDataFrame, 100)


#plot(dtm, term = findFreqTerms(dtm, lowfreq = 100), corThreshold = 0.6, weighting = T)
#findFreqTerms(dtm, lowfreq = 50)
