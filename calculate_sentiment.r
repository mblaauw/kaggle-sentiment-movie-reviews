library(plyr)
library("klaR")
library("caret")
library(e1071) 

testset <- read.delim("./data/test.tsv")
trainset <- read.delim("./data/train.tsv")

# -------------------------------
train$Phrase = tolower(train$Phrase)
NegTerms <- train$Phrase[train$Sentiment==0 | train$Sentiment==1]
NeutralTerms <- train$Phrase[train$Sentiment==2]
PosTerms <- train$Phrase[train$Sentiment==3 | train$Sentiment==4]

#function to calculate number of words in each category within a sentence
sentimentScore <- function(sentences, NegTerms, NeutralTerms, PosTerms){  
  
  final_scores <- matrix('', 0, 5)
  
  scores <- laply(sentences, function(sentence, NegTerms, NeutralTerms, PosTerms){
    initial_sentence <- sentence
    #remove unnecessary characters and split up by word 
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- tolower(sentence)
    wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    
    #build vector with matches between sentence and each category
    NegMatches <- match(words, NegTerms)
    NeutralMatches <- match(words, NeutralTerms)
    PosMatches <- match(words, PosTerms)
    
    #sum up number of words in each category
    NegMatches <- sum(!is.na(NegMatches))
    NeutralMatches <- sum(!is.na(NeutralMatches))
    PosMatches <- sum(!is.na(PosMatches))
      
    score <- c(NegMatches, NeutralMatches, PosMatches)
    
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, NegMatches, NeutralMatches, PosMatches)
  return(scores)
}    
# -------------------------------



train_sample = trainset[3:4]

x = train_sample$Phrase
y = train_sample$Sentiment

x_sample = x[1:20]
y_sample = y[1:20]

model = train(x,y, tuneLength=1)


#--e1071
classifier <- naiveBayes(x, y) 
predict(classifier, x)
confTable <- table(predict(classifier, results), results[,6], dnn=list('predicted','actual'))
confTable  


#build tables of positive and negative sentences with scores
posResult <- as.data.frame(sentimentScore(test$Phrase, NegTerms, NeutralTerms, PosTerms))
negResult <- as.data.frame(sentimentScore(negText, NegTerms, NeutralTerms, PosTerms))