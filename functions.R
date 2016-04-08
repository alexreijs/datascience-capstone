setwd("D:/Development/Repos/datasciencecoursera/Capstone project")

loadLines <- function(location, randomSampleSize = 0) {
  
  # Create a connection to file and read lines
  con <- file(location, "r", blocking = FALSE)
  lines <- readLines(con)   
  close(con)
  
  # Print some information
  print(ifelse(randomSampleSize == 0, paste("Total number of lines loaded:", length(lines)), paste("Sample size loaded:", randomSampleSize)))
  
  # Get a random sample of percentage if specified
  if (randomSampleSize > 0) {
    linesLength <- length(lines)
    linesSample <- sample(1:linesLength, randomSampleSize)
    lines <- lines[linesSample]
  }
  
  # Return lines
  lines
  
}

mergeSumDataTables <- function(dataTable1, dataTable2) {
  dataTable <- merge(dataTable1, dataTable2, all.x = T, all.y = T, by = "tokens")
  dataTable$frequency <- rowSums(dataTable[, .(frequency.x, frequency.y)], na.rm = T)
  dataTable[, .(tokens, frequency)]
}


sliceToken <- function(token) {
  tokens <- unlist(strsplit(token, "\\s"))
  c(paste(tokens[1:(length(tokens)-1)], collapse = " "), tokens[length(tokens)])
}



preProcessLines <- function(files, maxGram = 4, chunkSize = 500000) {
  nGramDataTables <- list()  
  
  for (f in 1:nrow(files)) {
    
    file <- files[f]
    print(paste("Loading file:", file$location, "..."))
    lines <- loadLines(paste("./final/en_US/", file$location, sep = ""), randomSampleSize = file$randomSampleSize)

    nrOfChunks <- ceiling(length(lines) / chunkSize)
          
    for (x in 1:nrOfChunks) {
      ptm <- proc.time()
      print(paste("Preprocessing file:", file$name, f, "of", nrow(files)))
      print(paste("Preprocessing chunk:", x, "of", nrOfChunks))
      print("Creating corpus ...")
      corpus <- createCorpus(lines[((x-1) * chunkSize):(x*chunkSize)])
      
      for (n in 1:maxGram) {
        print(paste("Creating n-Gram", n, "..."))
        nGramDataTable <- createNGramDataTable(corpus, n)
        
        if (n > length(nGramDataTables))
          nGramDataTables[[n]] <- nGramDataTable
        else {
          nDT1 <- nrow(nGramDataTables[[n]])
          nDT2 <- nrow(nGramDataTable)
          print(paste("Merging n-Gram:", round(nDT1 / nDT2, 2), "(", nDT1, "/", nDT2, ")"))
          nGramDataTables[[n]] <- mergeSumDataTables(nGramDataTables[[n]], nGramDataTable)
        }
      }
      
      print(proc.time() - ptm)
    }
  }
    
  ptm <- proc.time()
  for (n in 1:maxGram) {
    if (n > 1) {
      print(paste("Getting head and tail", n, "..."))
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
  print(proc.time() - ptm)
  
  nGramDataTables
  
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

createCorpus__ <- function(lines) {
  
  corpus <- Corpus(VectorSource(lines))
  
  specialChars <- content_transformer(function(x, pattern, replacement) { gsub(pattern, replacement, x, perl = T) })
  
  corpus <- tm_map(corpus, specialChars, "\u00E2\u20AC\u0153", "'")  # convert â€œ
  corpus <- tm_map(corpus, specialChars, "\u00E2\u20AC\u02DC", "'")  # convert â€™
  corpus <- tm_map(corpus, specialChars, "\u00E2\u20AC\u2122", "'")  # convert â€˜
  corpus <- tm_map(corpus, specialChars, "\u00E2\u20AC\u201D", "–")  # convert â€”
  corpus <- tm_map(corpus, specialChars, "\u00E2\u20AC\u201C", "—")  # convert â€“
  corpus <- tm_map(corpus, specialChars, "\u00E2\u20AC\u00A2", "-")  # convert â€¢
  corpus <- tm_map(corpus, specialChars, "\u00E2\u20AC\u00A6", "…")  # convert â€¦
  corpus <- tm_map(corpus, specialChars, "\u00E2\u20AC", "'")  # convert â€
  
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, readLines("./profanity.txt")) # http://fffff.at/googles-official-list-of-bad-words/
  #corpus <- tm_map(corpus, removeWords, stopwords("english"))
  #corpus <- tm_map(corpus, stemDocument)
  
  #corpus <- tm_map(corpus, specialChars, "([0-9:. -]{2,3})?[0-9]{1,2} ?(a|p)( |[.])?m[.]?", "")  # remove numbers along with am / pm notation
  #corpus <- tm_map(corpus, specialChars, "[0-9]{1,} ?(g|kg|lbs|cc|sq|ft)", "")  # remove numbers along with common metrics
  
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  
  
  corpus
}

createCorpus_ <- function(lines) {
  
  # Create corpus from lines vector
  corpus <- Corpus(VectorSource(lines))
  
  # Apply cleaning of corpus
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  #corpus <- tm_map(corpus, stemDocument)

  
  # Create function to handle replacement using regular expressions
  specialChars <- content_transformer(function(x, pattern, replacement) { gsub(pattern, replacement, x, perl = T) })
  
  # Convert special characters
  corpus <- tm_map(corpus, specialChars, "\u00E2\u20AC\u0153", "'")  # convert â€œ
  corpus <- tm_map(corpus, specialChars, "\u00E2\u20AC\u02DC", "'")  # convert â€™
  corpus <- tm_map(corpus, specialChars, "\u00E2\u20AC\u2122", "'")  # convert â€˜
  corpus <- tm_map(corpus, specialChars, "\u00E2\u20AC\u201D", "–")  # convert â€”
  corpus <- tm_map(corpus, specialChars, "\u00E2\u20AC\u201C", "—")  # convert â€“
  corpus <- tm_map(corpus, specialChars, "\u00E2\u20AC\u00A2", "-")  # convert â€¢
  corpus <- tm_map(corpus, specialChars, "\u00E2\u20AC\u00A6", "…")  # convert â€¦
  corpus <- tm_map(corpus, specialChars, "\u00E2\u20AC", "'")  # convert â€
  
  # Remove retweets
  corpus <- tm_map(corpus, specialChars, "(rt|via)((?:\\b\\W*@\\w+)+)", "")
  
  # Remove @people mentions
  corpus <- tm_map(corpus, specialChars, "@\\w+", "")
  
  # Remove URLs
  corpus <- tm_map(corpus, specialChars, "http\\w+", "")
  
  # Remove stopwords
  corpus <- tm_map(corpus, removeWords, stopwords("en"))   
  #corpus <- tm_map(corpus, removeWords, readLines("./minimal-stop.txt"))   # http://www.text-analytics101.com/2014/10/all-about-stop-words-for-text-mining.html
  #corpus <- tm_map(corpus, removeWords, readLines("./terrier-stop.txt"))   # http://www.text-analytics101.com/2014/10/all-about-stop-words-for-text-mining.html

  
  # Remove numbers and any common metrics or time indication
  corpus <- tm_map(corpus, specialChars, "([0-9:. -]{2,3})?[0-9]{1,2} ?(a|p)( |[.])?m[.]?", "")  # remove numbers along with am / pm notation
  corpus <- tm_map(corpus, specialChars, "[0-9]{1,} ?(g|kg|lbs|cc|sq|ft)", "")  # remove numbers along with common metrics
  corpus <- tm_map(corpus, removeNumbers) 

  # Remove punctuation and special characters
  corpus <- tm_map(corpus, removePunctuation)
  #corpus <- tm_map(corpus, specialChars, "[\\\"'¦“”$*&+<>]", "")  # remove some characters
  #corpus <- tm_map(corpus, specialChars, "\\/", "")  # remove some characters
  #corpus <- tm_map(corpus, specialChars, "\\\\", "")  # remove some characters
  #corpus <- tm_map(corpus, specialChars, "\\-", "")  # remove some characters
  
  # Remove excess whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  
  # Remove profantiry
  corpus <- tm_map(corpus, removeWords, readLines("./profanity.txt")) # http://fffff.at/googles-official-list-of-bad-words/
  
  #corpus <- tm_map(corpus, specialChars, "( [a-z]*)\\1{1,}", "")  # (WIP) deduplicate reocurring ngrams like "no no no"

  # Return corpus
  corpus
  
}



createNGramDataTable <- function (corpus, n) {
  
  tokens <- tokenize(corpus,
                     concatenator = " ",
                     #ngram = n,
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


predictNextWords <- function(input, probabilities) {
  
  inputTokens <- rev(unlist(strsplit(input, "\\s")))
  nGram <- min(4, length(inputTokens))
  backTrack <- 0
  originalNGram <- nGram
  nextWords <- c()
  
  while (nGram >= 1) {

    inputNGram <- paste(rev(inputTokens[1:nGram]), collapse = " ")
        
    if (nGram > length(probabilities)) {
      print(paste("Input to large, removing one word"))
      nGram <- nGram - 1
      originalNGram <- originalNGram - 1
      next
    }
    
    if (is.na(sum(probabilities[[nGram]][inputNGram]$probability))) {
      print(paste("No match found, going to n-1 gram"))
      nGram <- nGram - 1
      next
    }
    else {
      print(paste("Match found after", (originalNGram - nGram), "backtracks"))
      
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
  wordcloud(words = dataFrame[1:n, 1], random.order = FALSE, freq = dataFrame[1:n, 2], scale = scale, colors = pal, ordered.colors = TRUE)
  
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





















createWekaNGramDataTable <- function(corpus, n) {
  
  # Convert corpus into readable data to RWeka tokenizer
  dataTable <- data.table(text = unlist(sapply(corpus, '[',"content")), stringsAsFactors = F)
  
  # Create n-gram tokenizer function
  tokens <- NGramTokenizer(dataTable, Weka_control(min = n, max = n))
  
  # Prepare data frame from tokens
  dataTable <- data.table(table(tokens))
  
  #dataTable <- dataTable[order(dataTable$N, decreasing = TRUE), ]  
  
  colnames(dataTable) <- c("tokens", "frequency")
  
  # Return data frame
  dataTable
  
}

