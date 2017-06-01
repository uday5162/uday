# Build by Uday
CV_data <- read.csv("Resumedata.csv", stringsAsFactors = FALSE)
str(CV_data)
##------------------------------------------------------------------------------------------------------------------------------------##
# type can be any factorable data
CV_data$type <- factor(CV_data$type)

#Examining data
str(CV_data$type)
table(CV_data$type)

#division of train and test data
rawData_train <- CV_data$type[1:399,]
rawData_test  <- CV_data$type[400:500,]
##------------------------------------------------------------------------------------------------------------------------------------##
library(tm)

#text can be any column
#dataCorpus
CV_data$text <- Corpus(VectorSource(CV_data$text))

#jsf
str(CV_data$text)
inspect(CV_data$text)
##------------------------------------------------------------------------------------------------------------------------------------##
#cleaning the data
Cleaned_corpusdata <- tm_map(CV_data$text , tolower)
 # doubt the next line for checking the case of Experience# 
Cleaned_corpusdata <- tm_map(Cleaned_corpusdata , removeNumbers)
Cleaned_corpusdata <- tm_map(Cleaned_corpusdata , removeWords , stopwords())
Cleaned_corpusdata <- tm_map(Cleaned_corpusdata , removePunctuation)
Cleaned_corpusdata <- tm_map(Cleaned_corpusdata , stripWhitespace)

#division of train and test data
CV_data_Corpusclean_train <- Cleaned_corpusdata[1:399,]
CV_data_Corpusclean_test <- Cleaned_corpusdata[399:500,]
##------------------------------------------------------------------------------------------------------------------------------------##
#DocumentTermMatrix

CV_data$text_dtm <- DocumentTermMatrix(Cleaned_corpusdata)
CV_data$text_dtm

#division of train and test data
CVdata_dtm_train <- CV_data$text_dtm[1:399,]
CVdata_dtm_test <- CV_data$text_dtm[400:500,]
##------------------------------------------------------------------------------------------------------------------------------------##
var <- findFreqTerms(CVdata_dtm_train,5)
CVdata$text_train <- DocumentTermMatrix(CV_data_Corpusclean_train,list(dictionary = var))
CVdata$text_test <- DocumentTermMatrix(CV_data_Corpusclean_test,list(dictionary = var))

#convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}
CVdata$text_train <- apply(CVdata$text_train, MARGIN = 2 , convert_counts)
CVdata$text_test <- apply(CVdata$text_test, MARGIN = 2 , convert_counts)

##------------------------------------------------------------------------------------------------------------------------------------##


#Similar thing for all coldata


##------------------------------------------------------------------------------------------------------------------------------------##

##-------------------------------------------------Final Steps Applying Linear Regression-----------------------------------------------##
library(e1071)
#x1 is train data of class 1 and logit is for binomial
CV_Classifier <- glm(selected ~ x1 + x2 + ...... , family=binomial(link = "logit") , data = CV_data)
##------------------------------------------------------------------------------------------------------------------------------------##
#predicting the accuracy 
CV_data_predict <- predict(CV_Classifier,CVdata$text_test)
library(gmodels)
CrossTable(CV_data_predict, rawData_test,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))