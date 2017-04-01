library(tm)
emails = read.csv("emails.csv", stringsAsFactors =  FALSE)

corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)
spdtm = removeSparseTerms(dtm, 0.95)
dtm = as.data.frame(as.matrix(dtm))
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

emailsSparse$spam = as.factor(emails$spam)

library(caTools)
set.seed(123)
split = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, split == TRUE)
test = subset(emailsSparse, split == FALSE)

#building models
spamLog = glm(spam ~ ., data=train, family = "binomial")
library(rpart)
library(rpart.plot)
spamCart = rpart(spam ~ ., data=train, method = "class")
library(randomForest)
set.seed(123)
spamRf = randomForest(spam ~ ., data=train)

#evaluating models
probLog = predict(spamLog, newdata=test, type = "response")
probCart = predict(spamCart, newdata = test, type = "prob")
probRf = predict(spamRf, newdata = test, type = "prob")

summary(spamLog)
sum(diag(table(test$spam, probLog >= 0.5)))/nrow(test) #Accuracy of Logistic Regression

library(ROCR)
pred = prediction(probLog, test$spam)
pref = performance(pred, "auc")
as.numeric(pref@y.values)

sum(diag(table(test$spam, probCart[,2] >= 0.5)))/nrow(test)
as.numeric(performance(prediction(probCart[,2],test$spam),"auc")@y.values)

sum(diag(table(test$spam, probRf[,2] >= 0.5)))/nrow(test)
as.numeric(performance(prediction(probRf[,2],test$spam),"auc")@y.values)










