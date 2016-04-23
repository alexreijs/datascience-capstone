setwd("D:/Development/Repos/datasciencecoursera/Capstone project")
set.seed(12345)

loadLines <- function(location, randomSampleSize = 0, verboseLevel = 2) {
  
  if (verboseLevel >= 2)
    print(paste("Loading file:", location, "..."))
  
  # Create a connection to file and read lines
  con <- file(location, "r", blocking = FALSE)
  lines <- suppressWarnings(readLines(con))
  close(con)
  
  # Print some information
  if (verboseLevel >= 2)
    print(ifelse(randomSampleSize == 0,
                 paste("Total number of lines loaded:", length(lines)),
                 paste("Sample size loaded:", randomSampleSize)))
  
  # Get a random sample of percentage if specified
  if (randomSampleSize > 0) {
    linesLength <- length(lines)
    linesSample <- sample(1:linesLength, min(randomSampleSize, linesLength))
    lines <- lines[linesSample]
  }
  
  # Return lines
  lines
  
}

mergeSumDataTables <- function(dataTable1, dataTable2, n, verboseLevel = 2) {
  nDT1 <- nrow(dataTable1)
  nDT2 <- nrow(dataTable2)
  
  if (verboseLevel >= 2)
    print(paste("Merging ", n, "-Gram: ", round(nDT1 / nDT2, 2), " ( ", nDT1, " / ", nDT2, " )", sep = ""))
  
  dataTable <- merge(dataTable1, dataTable2, all.x = T, all.y = T, by = "tokens")
  dataTable$frequency <- rowSums(dataTable[, .(frequency.x, frequency.y)], na.rm = T)
  dataTable[, .(tokens, frequency)]
}


mergeToList <- function(list1, list2, n, verboseLevel = 2) {
  if (n > length(list1))
    list1[[n]] <- list2
  else
    list1[[n]] <- mergeSumDataTables(list1[[n]], list2, n, verboseLevel)
  
  list1
}



sliceToken <- function(token) {
  tokens <- unlist(strsplit(token, "\\s"))
  c(paste(tokens[1:(length(tokens)-1)], collapse = " "), tokens[length(tokens)])
}


getHeadAndTail <- function(nGramDataTables, maxGram, verboseLevel = 2) {
  
  ptm <- proc.time()
  
  for (n in 1:maxGram) {
    if (n > 1) {
      if (verboseLevel >= 2)
        print(paste("Getting head and tail for ", n, "-Gram ...", sep = ""))
      nGramDataTables[[n]] <- nGramDataTables[[n]][frequency > 1, ]
      
      slices <- mapply(sliceToken, nGramDataTables[[n]]$tokens)
      heads <- slices[1, ]
      tails <- slices[2, ]
      names(heads) <- c()
      names(tails) <- c()
      
      nGramDataTables[[n]]$head <- heads
      nGramDataTables[[n]]$tail <- tails
      setkeyv(nGramDataTables[[n]], c("tokens", "head"))
    }
  }
  if (verboseLevel >= 2)
    print(proc.time() - ptm)
  
  nGramDataTables
  
}


processLines <- function(lines, maxGram, chunkSize, verboseLevel = 2) {
  
  nGramDataTables <- list()  
  nrOfChunks <- ceiling(length(lines) / chunkSize)
  
  for (x in 1:nrOfChunks) {
    ptm <- proc.time()
    if (verboseLevel >= 2) {
      print(paste("Processing chunk:", x, "of", nrOfChunks))
      print("Creating corpus ...")
    }
    corpus <- createCorpus(lines[((x-1) * chunkSize):(x * chunkSize)])
    
    for (n in 1:maxGram) {
      if (verboseLevel >= 2)
        print(paste("Creating ", n, "-Gram ...", sep = ""))
      nGramDataTables <- mergeToList(nGramDataTables, createNGramDataTable(corpus, n), n)
    }
    if (verboseLevel >= 2)    
      print(proc.time() - ptm)
  }
  
  nGramDataTables
  
}




saveProbabilities <- function(files, maxGram = 4, chunkSize = 500000) {
  
  nGramDataTables <- list()  
  
  for (f in 1:nrow(files)) {
    file <- files[f, ]
    lines <- loadLines(file$location, randomSampleSize = file$randomSampleSize)
    
    print(paste("Processing file:", file$name, f, "of", nrow(files)))
    fileNGramDataTables <- processLines(lines, maxGram, chunkSize)
    
    if (length(nGramDataTables) > 0)
      print("Merging n-Gram data tables ...")
    
    ptm <- proc.time()
    for (n in 1:maxGram) {
      nGramDataTables <- mergeToList(nGramDataTables, fileNGramDataTables[[n]], n)
    }
    print(proc.time() - ptm)
    
  }
  
  nGramDataTables <- getHeadAndTail(nGramDataTables, maxGram)
  probabilities <- createProbabilities(nGramDataTables)
  saveRDS(probabilities, file = paste("probabilities.", paste(files$randomSampleSize / 1000, collapse = "k."), "k.dt", sep = ""))
  
  #nGramDataTables
  
}




accuracyTest <- function(files, maxGram = 4, backOff = T, chunkSize = 500000, verboseLevel = 2) {
  
  testNGramDataTables <- list()
  trainingNGramDataTables <- list()
  
  for (f in 1:nrow(files)) {
    file <- files[f, ]
    lines <- loadLines(file$location, randomSampleSize = file$randomSampleSize, verboseLevel = verboseLevel)
    
    if (verboseLevel >= 2)
      print("Splitting lines ...")
    randomLines <- sample(lines, length(lines))
    splitInt <- round(length(randomLines) * 0.8)
    trainingLines <- randomLines[1:splitInt]
    testLines <- randomLines[(splitInt + 1):length(randomLines)]
    
    if (verboseLevel >= 2)
      print(paste("Processing training lines:", file$name, f, "of", nrow(files)))
    trainingNGramDataTablesNew <- processLines(trainingLines, maxGram, chunkSize, verboseLevel = verboseLevel)
    if (verboseLevel >= 2)
      print(paste("Processing test lines:", file$name, f, "of", nrow(files)))
    testNGramDataTablesNew <- processLines(testLines, maxGram, chunkSize, verboseLevel = verboseLevel)
    
    ptm <- proc.time()
    if (verboseLevel >= 2)
      print("Merging n-Gram data tables ...")
    
    for (n in 1:maxGram) {
      trainingNGramDataTables <- mergeToList(trainingNGramDataTables, trainingNGramDataTablesNew[[n]], n, verboseLevel = verboseLevel)
      testNGramDataTables <- mergeToList(testNGramDataTables, testNGramDataTablesNew[[n]], n, verboseLevel = verboseLevel)
    }
    if (verboseLevel >= 2)
      print(proc.time() - ptm)
    
  }
  
  trainingNGramDataTables <- getHeadAndTail(trainingNGramDataTables, maxGram, verboseLevel = verboseLevel)
  testNGramDataTables <- getHeadAndTail(testNGramDataTables, maxGram, verboseLevel = verboseLevel)
  
  print("Creating probabilities ...")
  probabilities <- createProbabilities(trainingNGramDataTables)
  
  for (n in 2:maxGram) {
    if (verboseLevel >= 2)
      print(paste("Getting predictions for ", n, "-Gram ...", sep = ""))
    testNGramDataTables[[n]]$prediction <- mapply(function(tokens) {
      return(predictNextWords(tokens, probabilities, backOff = backOff)[1]$nextWord)
    }, testNGramDataTables[[n]]$head)
  }
  
  totalCorrect <- 0
  totalTokens <- 0
  
  for (n in 2:maxGram) {
    correct <- sum(testNGramDataTables[[n]]$tail == testNGramDataTables[[n]]$prediction)
    tokens <- nrow(testNGramDataTables[[n]])
    
    totalCorrect <- totalCorrect + correct
    totalTokens <- totalTokens + tokens
    
    print(paste(n, "-Gram accuracy: ", round(correct / tokens * 100, 2), sep = ""))
  }
  
  print(paste("Total accuracy: ", round(totalCorrect / totalTokens * 100, 2), sep = ""))
  
  #totalCorrect / totalTokens
}




createCorpus <- function(lines) {
  
  lines <- toLower(lines)
  
  Encoding(lines) <- "latin1"
  lines <- iconv(lines, "latin1", "ASCII", sub = "")
  
  lines <- gsub("([0-9:. -]{2,3})?[0-9]{1,2} ?(a|p)( |[.])?m[.]?", "", lines)
  lines <- gsub("[0-9]{1,} ?(g|kg|lbs|cc|sq|ft|mm|m)", "", lines)
  lines <- gsub("['\"]", "", lines)

  corpus(lines)
  
}


createNGramDataTable <- function (corpus, n) {
  
  tokens <- tokenize(corpus,
                     concatenator = " ",
                     verbose = FALSE,
                     removeNumbers = TRUE,
                     removeHyphens = TRUE,
                     removePunct = TRUE,
                     removeTwitter = TRUE)
  
  #tokens <- removeFeatures(tokens, stopwords("english"))
  #tokens <- removeFeatures(tokens, readLines("./profanity.txt"))

  tokens <- ngrams(tokens, n = n, concatenator = " ")
  
  dfm <- dfm(tokens, verbose = FALSE)
  dfreq <- docfreq(dfm)
  
  dataTable <- data.table(tokens = names(dfreq), frequency = dfreq)
  setkey(dataTable, tokens)
  dataTable
  
}




createProbabilities <- function(nGramDataTables) {
  
  probabilityDataTables <- list(NULL)
  
  for (n in 1:(length(nGramDataTables)-1)) {
    
    nGramDataTable <- nGramDataTables[[n]]
    nGramDataTableNext <- nGramDataTables[[n + 1]]
    
    dataTable <- suppressWarnings(merge(nGramDataTable, nGramDataTableNext, all.x = F, by.x = "tokens", by.y = "head"))
    dataTable$probability <- round(dataTable$frequency.y / dataTable$frequency.x, 4)
    
    if (n == 1)
      colnames(dataTable) <- c("tokens", "a", "b", "c", "nextWord", "probability")
    else 
      colnames(dataTable) <- c("tokens", "a", "b", "c", "e", "f", "nextWord", "probability")
    
    dataTable <- dataTable[, .(tokens, nextWord, probability)]
    probabilityDataTables[[n]] <- dataTable
    setkey(dataTable, tokens)
    
  }
  
  probabilityDataTables
}


predictNextWords <- function(input, probabilities, verbose = F, backOff = T) {
  
  inputTokens <- rev(unlist(strsplit(input, "\\s")))
  nGram <- min(4, length(inputTokens))
  originalNGram <- nGram
  nextWords <- c()
  
  
  while (nGram >= ifelse(backOff, 1, originalNGram)) {
    
    inputNGram <- paste(rev(inputTokens[1:nGram]), collapse = " ")
    
    if (nGram > length(probabilities)) {
      if (verbose)
        print(paste("Input to large, removing one word"))
      
      nGram <- nGram - 1
      originalNGram <- originalNGram - 1
      next
    }
    
    if (is.na(sum(probabilities[[nGram]][inputNGram]$probability))) {
      if (verbose)
        print(paste("No match found, going to n-1 gram"))
      
      nGram <- nGram - 1
      next
    }
    else {
      if (verbose)
        print(paste("Match found after", (originalNGram - nGram), "backoffs"))
      
      nextWords <- probabilities[[nGram]][inputNGram][order(-probability)]
      
      if (nGram < originalNGram)
        nextWords$probability <- nextWords$probability * (0.4 ^ (originalNGram - nGram))
      
      break
    }
    
  }
  
  nextWords
}


createWordCloud <- function(dataFrame, n = 10, scale = c(3, 1)) {
  
  # Create color palette
  pal <- colorRampPalette(brewer.pal(10, "RdBu"))(n)
  
  # Generate wordcloud
  wordcloud(words = dataFrame[1:n, 1],
            random.order = FALSE,
            freq = dataFrame[1:n, 2],
            scale = scale,
            colors = pal,
            ordered.colors = TRUE)
}

wordsNeeded <- function(goal = 0.6, data) {
  
  # Initialize variables
  wordCount <- 0
  currentPercentage <- 0.0
  totalFrequency <- sum(data)
  
  # Accumulate word coverage until goal is reached
  while (currentPercentage < goal & wordCount <= length(data)) {
    currentPercentage <- currentPercentage + (data[wordCount + 1] / totalFrequency)
    wordCount <- wordCount + 1
  }
  
  # Return number of words used
  wordCount 
  
}
