Data Science - Capstone Project
========================================================
author: Alexander Reijs
date: 2016-04-21
autosize: true
width: 1440

Introduction
========================================================




This project aims at providing a useful way of predicting the next word a user would most likely want to type, given an input of a string of text. This presentation explains the techniques used, but the code is available as well.

Chapters:

- Processing data
- Prediction algorythm
- Testing the algorythm
- Shiny App

Links:

- Github: https://github.com/sjakil/datascience-capstone
- Shiny App: https://sjakil.shinyapps.io/datascience-capstone

Processing data
========================================================

One of the first choices to make was which R libraries to use for text mining. I have explored two options, `tm` and `quanteda`. After testing, it seems `quanteda` is alot faster and easier to use.

Text mining starts by getting a corpus of text and cleaning it. The available training data consists of around 600MB of lines originating from twitter, blogs posts and news articles. I created a process capable of handling this and which is very easy to use. 

This process loads a random sample of lines, cleans them and creates a corpus of text. I determined the following steps in cleaning yielded the most promising results:

- Remove time notations (xx pm/am) and unit notations (xx kg/mm/etc)
- Remove special encoding (ä > a), Twitter (RT, @ etc), hyphens, numbers and punctuation.


```
[1] "Loading file: ../final/en_US/en_US.twitter.txt ..."
[1] "Sample size loaded: 10000"
```

```
[1] "Creating corpus ..."
```

Prediction algorythm
========================================================

After getting the corpus of text we can start creating n-grams. In case you are not familiar with them, n-grams are combinations of n amount of words. For example, a common bigram (2-gram) is "you are". In this project, I've created a maximum of 5 grams.

The next step is taking every n-gram except the unigram (1-gram) and splitting off the last word (let's call it "tail") from the words that came before ("head"). Some of these heads will appear more than once and have different tails. By simply counting the frequency of tails and heads, we can calculate probabilities.

The final step is to produce a table that contains these probabilities. Every head of n grams is essentially a n - 1 gram as well. This makes it so that the head of a quintgram (5-gram) could be matched to a quadgram (4-gram) input. So  a model with n-grams up to 5 n's, would be able to make predictions based on input of up to 4 grams.

Testing the algorythm
========================================================

In order to test the algorythm we are going to repeat the process described before, but this time we will split the lines of text into two parts. The one part is used to train the model (80% of data), the other part is used to test the model (20% of data). After creating n-grams and probabilities for both sets, we can calculate how many our model predicts correctly.

I've created a function that will make doing these tests very easy. In the example below I've used 50k lines of all three sources for up to quintgrams.



```r
accuracyTest(files, maxGram = 5, verboseLevel = 1)
```

```
[1] "Creating probabilities ..."
[1] "2-Gram accuracy: 7.68"
[1] "3-Gram accuracy: 27.27"
[1] "4-Gram accuracy: 46.61"
[1] "5-Gram accuracy: 54.37"
[1] "Total accuracy: 18.57"
```

Shiny App
========================================================

The Shiny App provides a straight-forward way of using the prediction algorythm. You enter a string of text and the app almost instantly gives you the prediction it found.

It also implements what is called stupid backoff. This means if the app could not predict a word given an n-gram as input, it will try to predict the next word using the n - 1 gram as input.

The predictions are presented in a data table, but you can also view a word cloud.

To make the probabilities works really fast, I've used the `data.table` package and it's `setkey` function. It creates an index on a specified column and this presumably works up to a 1000x times faster compared to normal data frames.

Link: https://sjakil.shinyapps.io/datascience-capstone



