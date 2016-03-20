loadLines <- function(location, randomSampleSize = 0) {
  
  # Create a connection to file and read lines
  con <- file(location, "r", blocking = FALSE)
  lines <- readLines(con)   
  close(con)
  
  # Print some information
  print(paste("Total number of lines loaded:", length(lines), "| Sample size loaded:", randomSampleSize))
  
  # Get a random sample of percentage if specified
  if (randomSampleSize > 0) {
    linesLength <- length(lines)
    linesSample <- sample(1:linesLength, randomSampleSize)
    lines <- lines[linesSample]
  }
  
  # Return lines
  lines
  
}

createCorpus <- function(lines) {
  
  # Create corpus from lines vector
  corpus <- Corpus(VectorSource(lines))
  
  # Apply cleaning of corpus
  corpus <- tm_map(corpus, content_transformer(tolower))
  
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

  # Remove punctuation and special characters
  #corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, specialChars, "[\\\"'¦“”$*&+<>]", "")  # remove some characters
  corpus <- tm_map(corpus, specialChars, "\\/", "")  # remove some characters
  corpus <- tm_map(corpus, specialChars, "\\\\", "")  # remove some characters
  corpus <- tm_map(corpus, specialChars, "\\-", "")  # remove some characters
  
  
  # Remove numbers and any common metrics or time indication
  corpus <- tm_map(corpus, specialChars, "([0-9:. -]{2,3})?[0-9]{1,2} ?(a|p)( |[.])?m[.]?", "")  # remove numbers along with am / pm notation
  corpus <- tm_map(corpus, specialChars, "[0-9]{1,} ?(g|kg|lbs|cc|sq|ft)", "")  # remove numbers along with common metrics
  corpus <- tm_map(corpus, removeNumbers) 

  # Remove excess whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  
  # Remove profantiry
  corpus <- tm_map(corpus, removeWords, readLines("./profanity.txt")) # http://fffff.at/googles-official-list-of-bad-words/
  
  #corpus <- tm_map(corpus, specialChars, "( [a-z]*)\\1{1,}", "")  # (WIP) deduplicate reocurring ngrams like "no no no"

  # Return corpus
  corpus
  
}

createDtmNgram <- function (corpus, n) {
  
  # Create n-gram tokenizer function
  tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  
  # Create Term Document Matrix using tokenizer
  dtm <- DocumentTermMatrix(corpus, control = list(tokenize = tokenizer))
  
  # Return Term Document Matrix
  dtm  
  
}


createWekaNGramDataFrame <- function(corpus, n) {
  
  # Convert corpus into readable data to RWeka tokenizer
  dataFrame <- data.frame(text = unlist(sapply(corpus, '[',"content")), stringsAsFactors = F)
  
  # Create n-gram tokenizer function
  tokens <- NGramTokenizer(dataFrame, Weka_control(min = n, max = n))
  
  # Prepare data frame from tokens
  dataFrame <- data.frame(table(tokens))
  dataFrame <- dataFrame[order(dataFrame$Freq, decreasing = TRUE), ]  
  
  # Return data frame
  dataFrame
  
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