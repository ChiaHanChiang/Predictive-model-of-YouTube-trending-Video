library(rpart)
library(rpart.plot)
library(caret)

trainfreq$Largest_view <- as.integer(trainfreq$Largest_view)
trainfreq$Likes <- as.integer(trainfreq$Likes)
trainfreq$Dislikes <- as.integer(trainfreq$Dislikes)
trainfreq$Comment <- as.integer(trainfreq$Comment)

trainfreq$freq <- ifelse(trainfreq$freq>=5, 1,0)
testfreq$freq <- ifelse(testfreq$freq>=5, 1,0)

default.ct <- rpart(freq ~ Largest_view+Likes+Dislikes+Comment, data=trainfreq, method = "class")
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10)


youtube.ct <- rpart(freq ~ Largest_view+Likes+Dislikes+Comment, data=trainfreq, cp=0.0001, minsplit=5, method = "class")
prp(youtube.ct, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10)

deeper.ct <- rpart(freq ~ Largest_view+Likes+Dislikes+Comment, data=trainfreq, method = "class", cp=0, minsplit = 1)
prp(deeper.ct, type = 1, extra = 1, split.font = 1,varlen = -10)

pruned.youtube.ct <- prune(youtube.ct, cp = youtube.ct$cptable[which.min(youtube.ct$cptable[,"xerror"]),"CP"])
prp(pruned.youtube.ct, type = 1, extra = 1, split.font = 1,varlen = -10)

ct.pred.test <- predict(youtube.ct, testfreq, type = "class")
ct.pred.train <- predict(youtube.ct, trainfreq, type = "class")

dct.pred.test <- predict(deeper.ct, testfreq, type = "class")
dct.pred.train <- predict(deeper.ct, trainfreq, type = "class")

confusionMatrix(factor(ct.pred.test), factor(testfreq$freq))
confusionMatrix(factor(ct.pred.train), factor(trainfreq$freq))

confusionMatrix(factor(dct.pred.test), factor(testfreq$freq))
confusionMatrix(factor(dct.pred.train), factor(trainfreq$freq))

spotify.rt <- rpart(freq ~ Largest_view+Likes+Dislikes+Comment, data=trainfreq, control = rpart.control(minbucket = 1, maxdepth = 30, cp = 0.001), method = "anova")
prp(spotify.rt)


pruned.spotify.rt <- prune(spotify.rt, cp = spotify.rt$cptable[which.min(spotify.rt$cptable[,"xerror"]),"CP"])
pruned.spotify.rt <- prune(spotify.rt, cp = spotify.rt$cptable[5,"CP"])



prp(pruned.spotify.rt)

validfreq$Largest_view <- as.integer(validfreq$Largest_view)
validfreq$Likes <- as.integer(validfreq$Likes)
validfreq$Dislikes <- as.integer(validfreq$Dislikes)
validfreq$Comment <- as.integer(validfreq$Comment)

valid.pred <- predict(pruned.spotify.rt, validfreq[,c(5,8,9,10)])


sum((valid.pred - mean(validfreq[,2]))^2)
RMSE(valid.pred, validfreq[,2])


testfreq$Largest_view <- as.integer(testfreq$Largest_view)
testfreq$Likes <- as.integer(testfreq$Likes)
testfreq$Dislikes <- as.integer(testfreq$Dislikes)
testfreq$Comment <- as.integer(testfreq$Comment)

test.pred <- predict(pruned.spotify.rt, testfreq[,c(5,8,9,10)])
cat("The RMSE based on number of split 5:",RMSE(test.pred, testfreq[,2]))


RMSE <- data.frame("RMSE"=rep(0,10))
for (i in 1:10){
  pruned.spotify.rt <- prune(spotify.rt, cp = spotify.rt$cptable[i,"CP"])
  test.pred <- predict(pruned.spotify.rt, testfreq[,c(5,8,9,10)])
  RMSE$RMSE[i] <- RMSE(test.pred, testfreq[,2])
}
