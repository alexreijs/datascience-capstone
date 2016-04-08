library(data.table)
library(quanteda)
#library(tm)




#dataTable1 <- data.table(tokens = c("meh", "kek"), frequency = c(1,2))
#dataTable2 <- data.table(tokens = c("fds", "kek"), frequency = c(1,2))
#dataTable <- merge(dataTable1, dataTable2, all.x = T, all.y = T, by = "tokens")

probabilities[[1]]["happy"][order(-probability), ]
probabilities[[2]]["case of"][order(-probability), ]
probabilities[[3]]["struggling but the"][order(-probability), ]


#Rprof("nGramDataFrame.out", memory.profiling=T)
Rprof("nGramDataFrame.out")
#probabilities <- createProbabilities(nGramDataTables)
#nGramDataTables <- createNgramDataTables(maxN)
c <- createNGramDataTable(corpus, 4)
Rprof(NULL)
#summaryRprof("nGramDataFrame.out", memory="both")
summaryRprof("nGramDataFrame.out")



#Rprof(tmp <- tempfile())
#probabilities <- createProbabilities(nGramDataTables)
#Rprof()
#plotProfileCallGraph(readProfileData(tmp), score = "total")


#x = profr(createProbabilities(nGramDataFrames))
#ggplot(x)

#l <- lineprof(probabilities <- createProbabilities(nGramDataFrames))


#system.time(corpus <- createCorpus(lines)) / randomSize
#system.time(nGramDataFrames <- createNgramDataFrames(maxN)) / randomSize
#system.time(probabilities <- createProbabilities(nGramDataFrames)) / randomSize