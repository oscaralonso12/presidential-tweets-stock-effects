rm(list=ls())

options(scipen = 999, digits = 4)

# Function for replacing commas for close prices [Changes column from character to numeric]
replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
}



# importing data
obama <- read.csv('/Users/oscaralonso/Documents/School/San Jose State University/Spring 2021/engr_122/engr_122_final/Obama_SP500.csv', fileEncoding = 'UTF-8-BOM')
head(obama)

# getting dates while in office and converting Date class from character to Date
obama <- obama[obama['Date'] <= '2017-01-20',]
obama$Date <- as.Date(obama$Date)

# Replaces commas for close prices and rename column name from "Close." to "Close"
obama$Close. <- replaceCommas(obama$Close.)
names(obama)[names(obama) == 'Close.'] <- 'Close'


View(obama)



# importing data
trump <- read.csv('/Users/oscaralonso/Documents/School/San Jose State University/Spring 2021/engr_122/engr_122_final/Trump_SP500.csv', fileEncoding = 'UTF-8-BOM')
tail(trump)

# getting dates while in office and converting Date class from character to Date
trump <- trump[trump['Date'] >= '2017-01-20',]
trump$Date <- as.Date(trump$Date)

# Replaces commas for close prices
trump$Close. <- replaceCommas(trump$Close.)
names(trump)[names(trump) == 'Close.'] <- 'Close'


View(trump)

# checking the dimensions of the data
dim(obama)
dim(trump)




# loading packages
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")

# Obama tweets first
# Read the text file from local machine , choose file interactively
text <- obama$Tweet
# Load the data as a corpus
TextDoc <- Corpus(VectorSource(text))

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove your own stop word
custom_stop <- c("obama", "â€”presid", "pictwittercom", "presid", "ofabo", "http", "????")
# specify your custom stopwords as a character vector
TextDoc <- tm_map(TextDoc, removeWords, custom_stop) 
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)

# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 25)

# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods may have different scales
syuzhet_vector <- get_sentiment(text, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)

obama$sent_score <- syuzhet_vector
View(obama)

### Creating word cloud for obama
library(wordcloud2)
wordcloud2(data = dtm_d)

# Plotting Sentiment score and Close price
plot(obama$Date, obama$sent_score)
plot(obama$Date, obama$Close, type = 'l')




# Now Trump
# Read the text file from local machine , choose file interactively
text <- trump$Tweet
# Load the data as a corpus
TextDoc <- Corpus(VectorSource(text))

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove your own stop word
custom_stop <- c("will", "â€¦", "twittercom", "pictwittercom", "presid", "?????", "twittercom")
# specify your custom stopwords as a character vector
TextDoc <- tm_map(TextDoc, removeWords, custom_stop) 
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)

# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 25)

# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods may have different scales
syuzhet_vector_trump <- get_sentiment(text, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector_trump)
# see summary statistics of the vector
summary(syuzhet_vector_trump)

trump$sent_score <- syuzhet_vector_trump
View(trump)

### creating wordcloud for trump
library(wordcloud2)
wordcloud2(data = dtm_d)


# Plotting Sentiment score and Close price
plot(trump$Date, trump$sent_score, type='l')
plot(trump$Date, trump$Close, type = 'l')

# Visualizations
hist(obama$sent_score, main = 'Obama sentiment scores', xlab = 'Sentiment scores')

hist(trump$sent_score, main = 'Trump sentiment scores', xlan = 'Sentiment scores')


plot(trump$Date, trump$Close, type = 'l', main = 'Trump S&P 500', xlab = 'Date', ylab = 'Close')

plot(obama$Date, obama$Close, type = 'l', main = 'Obama S&P 500', xlab = 'Date', ylab = 'Close')

View(obama)
View(trump)


### t-test time
#### Have to aggregate data
agg_obama = aggregate(obama,
                by = list(obama$Date),
                FUN = mean)
View(agg_obama)

agg_trump = aggregate(trump,
                      by = list(trump$Date),
                      FUN = mean)
View(agg_trump)
#### Getting rate of change for the S&P column
library(dplyr)

agg_obama %>% 
  group_by(Date) %>% 
  arrange(Close, Date) %>% 
  mutate(rate = 100 * (count - lag(count))/lag(count)) %>%
  ungroup()
View(agg_obama)
agg_obama %>%
  mutate(rate = 100 * (Close - lag(close))/lag(close))

agg_obama <- agg_obama %>%
  mutate(rate = 100 * (agg_obama$Close - lag(agg_obama$Close))/lag(agg_obama$Close))
View(agg_obama)

agg_trump <- agg_trump %>%
  mutate(rate = 100 * (agg_trump$Close - lag(agg_trump$Close))/lag(agg_trump$Close))
View(agg_trump)


plot(agg_obama$Date,agg_obama$rate, type='l',
     main = 'Obama S&P Rate of Change',
     xlab = 'Date', ylab = 'Rate of Change')
plot(agg_trump$Date, agg_trump$rate, type='l',
     main = 'Trump S&P Rate of Change',
     xlab = 'Date', ylab = 'Rate of Change')

boxplot(agg_obama$sent_score, main = 'Obama Sentiment Score Boxplot',
        xlab = 'Mean = 0.5336', ylab = 'Sentiment Score')
boxplot(agg_trump$sent_score, main='Trump Sentiment Score Boxplot',
        ylab='Sentiment Score', xlab='Mean = 0.4439')


t.test(agg_obama$sent_score, agg_obama$rate)
# p-value is low. Less than 0.05, which is our significance level.
# This means that there is a difference between the means that is statistically significant.

t.test(agg_trump$sent_score, agg_trump$rate)
# P-value is also low here. It is lower than 0.05, our significance level.
# This means that there is a difference between the means that is statistically significant.

cor(agg_obama$sent_score, agg_obama$rate, use = 'pairwise')
cor(agg_trump$sent_score, agg_trump$rate, use = 'pairwise')


