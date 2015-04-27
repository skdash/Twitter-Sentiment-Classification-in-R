#Include all Required Libraries

library(streamR)
library(stringr)
library(SnowballC)
library(ROAuth)
library(tm)
library(RMOA)
library(kernlab)

pause <- function() invisible(readline())

#Change twitter Keys appropriately
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "zAXKGDyOHE25DTU28CW3QltLQ"
consumerSecret <- "Esjy0exqpT8woU9YN7gKPthCzTM5OLn8Tc4jPVc1qHqqJ74gRh"
my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, 
                             requestURL = requestURL, accessURL = accessURL, authURL = authURL)

cat("Performing Twitter Handshake")
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(my_oauth, file = "my_oauth.Rdata")

pause <- function() invisible(readLine())

if(file.exists("tweets.json")== TRUE)
  file.remove("tweets.json")

#Capture Love and Hate tweets from twitter stream for 60 seconds each and store it in
#tweets.json

cat("Capturing  Tweets...........")

filterStream("tweets.json", track = c("love"), timeout = 30, language='en', 
             oauth = my_oauth)

filterStream("tweets.json", track = c("hate"), timeout = 30, language='en', 
             oauth = my_oauth)


#Parse The Tweets Received

cat("-----PARSING TWEETS------")
tweets.df <- parseTweets("tweets.json", simplify = TRUE)
tweets_text = tweets.df$text

#Pre process the tweets by removing unicode characters, punctuation, 
#Convert to lowercase, remove Numbers, stopwords

cat("-----PRE PROCESSING TWEETS")

usableText=str_replace_all(tweets_text,"[^[:graph:]]", " ") 
corpus <- Corpus(VectorSource(usableText))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
corpus <- tm_map(corpus, stemDocument, language = "english", lazy = TRUE)

#Corpus contains the pre-processed tweets
# check corpus

#Stem the preprocessed data
text_stem <- tm_map(corpus, stemDocument, lazy=TRUE)

#Convert corpus to Document Term Matrix which contains frequency of each word
a <- DocumentTermMatrix(text_stem)

cat("Converting Document Term Matrix to Matrix")
#Convert Document Term Matrix to Matrix (stored in td.mat)
td.mat <- as.matrix(a)



#Perform Feature Set Selection using TOP-K 

#Create a Dataframe which has all the columns from td.mat matrix
#and has row value equal to sum of each column in td.mat
#sum_Column has the count of the number of each word which has repeated

cat("---------Finding Top-K words--------- ")
sum_Column <- data.frame(v1=(colnames(td.mat)),v2=(colSums(td.mat)))

#Sort the dataframe based on frequency of occurence of each word in decreasing order
sum_Column <-sum_Column[order(sum_Column$v2,decreasing = TRUE),]

#Select the first 200 rows--> This indicates the Top-200 words 
sum_Column_new <- sum_Column[1:200,]

#Transpose the dataframe, so that each column indicates the word
# and the row indicates the frequency of each word
sum_Column_new <- t(sum_Column_new)

#If hate is not present in top 200 words, remove the 200th column
#and append the count of "hate" in the 200th column
if(!("hate" %in% colnames(sum_Column_new)))
{
  sum_Column_new$hate = su(td.mat[,"hate"])
  sum_Column_new[,200] <- sum_Column_new[,-200]
}

#Remove the counts from sum_Column_new as only the column names are required
temp2 <- sum_Column_new[-2,]

#Find the intersection between the top 200 words and Document Term matrix
cmn <- intersect( colnames(td.mat), temp2)

#Copy the Document Term matrix only for the top 200 words
cmn <- td.mat[,cmn]
count <- 0 

#Find Column no for Love
loveCol = which( colnames(cmn)=="love" )
#Find Column no for Hate
hateCol = which( colnames(cmn)=="hate" )

#Create a character vector which will store the class of each tweet(Love/Hate)
Verdict = vector("character")

#Function to determine the class of each top-200 word and store it in Verdict vector
createVerdict <- function(data)
{
  if(data[loveCol] >= 1)
    Verdict <<- c(Verdict,"Love")
  else if(data[hateCol] >= 1)
    Verdict <<- c(Verdict,"Hate")
  else
    Verdict <<- c(Verdict, "Hate")
  count <<- count + 1
}

cat("---Finding Out class of each Tweet-----")
#For each row in cmn(Document Term matrix for top 200 words), determine the class
apply(cmn,1,createVerdict)

#Convert the Document Term matrix to a data frame
OutputDF <- data.frame(cmn)
#Append the Verdict column to the data frame
OutputDF$verdict <- Verdict

#Factor the Verdict column
OutputDF$verdict <- as.factor(OutputDF$verdict )

#Create a Data Frame without Love and Hate column
DF_without <- OutputDF
DF_without <- subset(DF_without, select=- c(love,hate))

#tweetVars will contain all words in the top 200 except for love,hate and verdict
tweetVars <- setdiff(colnames(DF_without),list('verdict'))
#Create Formula to be used for Training the model
tweetFormula <- as.formula(paste('verdict',
                                 paste(tweetVars,collapse='+'),sep='~'))

#Create HoeffdingTree
hdt <- HoeffdingTree(numericEstimator = "GaussianNumericAttributeClassObserver")
#Factorise DF_without
DF_without <- factorise(DF_without)

#Create a datastream from the data frame
datastream <- datastream_dataframe(data=DF_without)

## Train the HoeffdingTree on the above datastream using the formula created above and
#trainMOA function

cat("-----Training the Model-----")

mymodel_classifier <- trainMOA(model = hdt, 
                               formula = tweetFormula , 
                               data = datastream)

cat("-----Training Over-------")

## Predict using the HoeffdingTree on the training dataset
#scores <- predict(mymodel_classifier, newdata=DF_without, type="response")
#str(scores)
#table(scores, DF_without$verdict)


#######TEST NEW DATA#####

while(TRUE)
{
  if(file.exists("tweets.json")== TRUE)
    file.remove("tweets.json")
  
  #Capture Love and Hate tweets from twitter stream for 60 seconds each and store it in
  #tweets.json
  
  cat("---------Capturing new Tweets for Test Data---------")
  
  filterStream("tweets.json", track = c("love"), timeout = 30, language='en', 
               oauth = my_oauth)
  filterStream("tweets.json", track = c("hate"), timeout = 30, language='en', 
               oauth = my_oauth)
  
  cat("------Pre Processing Tweets------------")
  #Parse Tweets  
  tweets_test.df <- parseTweets("tweets.json", simplify = TRUE)
  tweets_text_test = tweets_test.df$text
  
  #Pre process the tweets by removing unicode characters, punctuation, 
  #Convert to lowercase, remove Numbers, stopwords
  
  usableText_test=str_replace_all(tweets_text_test,"[^[:graph:]]", " ") 
  corpus_test <- Corpus(VectorSource(usableText_test))
  corpus_test <- tm_map(corpus_test, content_transformer(tolower))
  corpus_test <- tm_map(corpus_test, removePunctuation)
  corpus_test <- tm_map(corpus_test, removeNumbers)
  corpus_test <- tm_map(corpus_test, function(x) removeWords(x, stopwords("english")))
  corpus_test <- tm_map(corpus_test, stemDocument, language = "english", lazy = TRUE)
  corpus_test # check corpus
  
  #Stem the preprocessed data
  text_stem_test <- tm_map(corpus_test, stemDocument, lazy=TRUE)
  #Convert corpus to Document Term Matrix which contains frequency of each word
  a_test <- DocumentTermMatrix(text_stem_test)
  #Convert Document Term Matrix to Matrix (stored in td.mat)
  td.mat_test <- as.matrix(a_test)
  #td.mat_test
  
  cat("--------Finding Commong Top K words From Training Data------")
  
  #Find words in td.mat_test which are existent in the top-K words in the training data 
  commonWords <- intersect(colnames(OutputDF),colnames(td.mat_test))
  #Create a matrix which contains the frequency of each intersected word
  commonTopK <- td.mat_test[,commonWords]
  #Find Words which are present in the top-k Words in training data
  #but not present in testdata and set its value to 0
  extra_words <- setdiff(colnames(OutputDF), commonWords)
  extra_words <- extra_words[extra_words!="verdict"]
  EmptyMatrix <- matrix(0,nrow=nrow(commonTopK),ncol = length(extra_words))
  colnames(EmptyMatrix) <- extra_words
  #EmptyMatrix <- EmptyMatrix[,-which( colnames(EmptyMatrix)=="verdict" )]
  EmptyMatrix2 <- as.matrix(EmptyMatrix[,-which( colnames(EmptyMatrix)=="verdict" )])
  
  #Create a character vector which will store the class of each tweet(Love/Hate)
  Verdict_test = vector("character")
  #Merges the empty matrix(words in topk not present in test data ) and Common TopK words
  commonTopK <- cbind(commonTopK,EmptyMatrix)
  #Find Column no for Love
  loveCol = which( colnames(commonTopK)=="love" )
  #Find Column no for Hate
  hateCol = which( colnames(commonTopK)=="hate" )
  
  #Function to determine the class of each top-200 word and store it in Verdict_test vector
  createVerdict_test <- function(data)
  {
    if(data[loveCol] >= 1)
      Verdict_test <<- c(Verdict_test,"Love")
    else if(data[hateCol] >= 1)
      Verdict_test <<- c(Verdict_test,"Hate")
    else
      Verdict_test <<- c(Verdict_test, "Hate")
    count <<- count + 1
  }
  
  #For each row in commonTopK(Document Term matrix for common topK words), determine the class
  apply(commonTopK,1,createVerdict_test)
  #Convert this into a dataframe
  commonTopK_DF <- data.frame(commonTopK)
  #Add the verdict column to the data frame
  commonTopK_DF$verdict <- Verdict_test
  #Factor the verdict column
  commonTopK_DF$verdict <- as.factor(commonTopK_DF$verdict)
  
  commonTopK_DF <- factorise(commonTopK_DF)
  
  datastream_test <- datastream_dataframe(data=commonTopK_DF)
  
  cat("---Prediciting Class for current Test data using Trained Model----")
  
  #Predict Class for current test data using the trained model
  scores <- predict(mymodel_classifier, newdata=commonTopK_DF, type="response")
  str(scores)
  
  cat("----Prediction Done-----")
  
  cat("----Updating The existing Model--------")
  #Update the existing model, with the new test data, by keeping reset=FALSE
  mymodel_classifier <- trainMOA(model = mymodel_classifier$model, 
                                 formula = tweetFormula , 
                                 data = datastream_test,reset=FALSE)
  #Computes and Prints Accuracy table for love and hate classfication
  
  cat("-------Displaying the Accuracy Table----------")
  AccuractyTable <- table(scores, commonTopK_DF$verdict)
  AccuractyTable
  
  cat("-----Displaying the Accuracy Percentage------")
  if(nrow(AccuractyTable)==1){
    if(row.names(AccuractyTable)=="Love")
    {
      loveAccuracy <- AccuractyTable["Love","Love"]/(AccuractyTable["Love","Love"]+AccuractyTable["Love","Hate"])
      hateAccuracy <- 0
    }else
    {
      loveAccuracy <- 0
      hateAccuracy <- AccuractyTable["Hate","Hate"]/(AccuractyTable["Hate","hate"]+AccuractyTable["Hate","Love"])
    }  
  }else
  {
    loveAccuracy <- AccuractyTable["Love","Love"]/(AccuractyTable["Love","Love"] + AccuractyTable["Hate","Love"])
    hateAccuracy <- AccuractyTable["Hate","Hate"]/(AccuractyTable["Hate","Hate"] + AccuractyTable["Love","Hate"])
  }
  cat("Love Accuracy: " ,loveAccuracy*100,"%\n")
  cat("Hate Accuracy: ",hateAccuracy*100,"%\n")
  
  cat("-----Press Any Key to Capture Next Tweet----------")
  
  pause()
  
}