###########################################################################
## Satya Sunkavalli 
## Created: may 10,2018
###########################################################################

#################### Setting Current Directory ############################
#setwd("C:/Users/bunny/Desktop/Satya ML")

######### Installing and loading required Libraries #######################
install.packages("tm")
library(tm)
install.packages('e1071', dependencies=TRUE)
library(e1071)
install.packages('dplyr', dependencies=TRUE)
library(dplyr)
install.packages('gradDescent', dependencies=TRUE)
library(gradDescent)
install.packages('ggplot2', dependencies=TRUE)
library(ggplot2)
install.packages('data.table', dependencies=TRUE)
library(data.table)
install.packages('wordcloud', dependencies=TRUE)
library(wordcloud)

######################### DATA COLLECTION #################################
######## Setting up keys and accesssecrets to create token ######################

consumerkey <- "glhuyzsOXwWKW3OsCo5ohSOz8"
consumersecret <- "ymTeXDy79QpsoCzWBAVOvrBcqcRIb1OdjXaZsfC8Ep6fWSvjeR"
accesstoken <- "817040547500163072-jfejOruhVZ3PnPoKzntj3taIlHGbIK1"
accesssecret <- "31ZKrkDmkhNhCW67kZ492iCKw7ov5XtTsLlmXhSs1vuqi"

token <- create_token(app = "rtweet_tokens_dic",consumer_key = consumerkey, consumer_secret = consumersecret, access_token = accesstoken, access_secret = accesssecret)

############ Creating a vector of terms to collect tweets ################# 

terms <- c("UB bookstore","#UB bookstore","UB Alert","UB Police","UB snowstorm","UB snow storm","UBuffalo","#UBuffalo","UBalumni","#UBalumni","Ubspectrum","#ubspectrum","UBNewsSource","#UBNewsSource","UBStudentExp","UBStudentExp","Ubathletics","Ubathletics","UBGreen","#UBGreen","UBCAS","#UBCAS","UB_sa","#UB_sa","UBAdmissions","#UBAdmissions","UBhornsUP","#UBhornsUP")

########### storing all the terms in a data frame #########################
tweets_ub_final <- data.frame()
for (term in terms)
{
  ub_tweets <- search_tweets(term,n=3000,retryonratelimit=TRUE, type = "recent", include_rts = F, until = "2019-05-08")
  if(length(ub_tweets)>0){
    tweets_ub_final <- rbind(tweets_ub_final,ub_tweets) 
  }}
dim(tweets_ub_final)

############## Writing the date into a csv #################################
write_as_csv(tweets_ub_final, "satya_may9.csv", prepend_ids = F, na = "", fileEncoding = "UTF-8")


# Reading the data from saved CSV File
#A CSV version of the original data can be downloaded from this link.
#We download the file to our working directory in R/RStudio and read it as a dataframe object.

tweet_data <- read.csv("C:/Users/bunny/Desktop/Satya ML/satya_may9.csv",stringsAsFactors = FALSE)
glimpse(tweet_data)

##### Randomizing the Dataset #####
tweet_data <- tweet_data[sample(nrow(tweet_data)), ]
tweet_data <- tweet_data[sample(nrow(tweet_data)), ]
glimpse(tweet_data)

# Convert the 'isThreat' variable from character to factor.
tweet_data$IsThreat <- as.factor(tweet_data$IsThreat)

####################### BAG OF WORDS TOKENISATION #######################
## We first prepare a corpus of all the documents in the dataframe
corpus <- Corpus(VectorSource(tweet_data$TweetText))
#corpus
#inspect(corpus[1:3])

##################### DATA CLEANING #####################################
## PERFORMING THE VARIOUS TRANSFORMATION on "corpus" DATASET #SUCH AS TRIM WHITESPACE, REMOVE PUNCTUATION, REMOVE STOPWORDS.
# Use dplyr's  %>% (pipe) utility to do this neatly.
corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)

myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                 "use", "see", "used", "via", "amp","can","take",'one','next')
corpus.clean <- tm_map(corpus.clean, removeWords, myStopwords)

######## MATRIX REPRESENTATION OF BAG OF WORDS: THE DOCUMENT MATRIX #######

#We represent the bag of words tokens with a document term matrix (DTM). 
#The rows of the DTM correspond to documents in the collection, columns correspond to terms, 
#and its elements are the term frequencies. 
#We use a built-in function from the ‘tm’ package to create the DTM.
dtm <- DocumentTermMatrix(corpus.clean)
#inspect(dtm[40:50, 10:15])

######### Finding TF-IDF of the model ########################
mat4 <- weightTfIdf(dtm)

splitedDataSet <- splitData(mat4)
splittarget <- splitData(data.frame(tweet_data$IsThreat))

train_fun <- cbind(data.frame(as.matrix(splitedDataSet$dataTrain)),data.frame(splittarget$dataTrain)) 
test_fun <- cbind(data.frame(as.matrix(splitedDataSet$dataTest)),data.frame(splittarget$dataTest))
####### Creating Gradient Descent Model using gradDescent Package ##
GDModel <- GD(train_fun)

####### Creating Gradient Descent Model using gradDescent Package ##
SGDModel <- SGD(train_fun)


############### VISUALIZATION ##################
colS <- colSums(as.matrix(dtm))
length(colS)
doc_features <- data.table(name = attributes(colS)$names, count = colS)


wordcloud(names(colS), colS, min.freq = 100, scale = c(6,.1), colors = brewer.pal(6, 'Dark2'))










