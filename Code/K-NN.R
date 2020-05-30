library(caret)
library(FNN)
library(class)
library(taRifx)

#####Avr. Frequency#####
tmp <- data.frame(trainfreq$Category, as.integer(trainfreq$freq))
colnames(tmp) <- c("Category", "freq")
tmp <- tmp[order(tmp$Category),]
category_freq <- plyr::count(tmp$Category)

for (i in 1:nrow(category_freq)){
  tmp2 <- subset(tmp,tmp$Category==category_freq$x[i])
  avr <- round(sum(tmp2$freq)/nrow(tmp2),2)
  category_freq$Avr_Freq[i] <- avr
}
category_freq$Norm <- round((category_freq$Avr_Freq-mean(category_freq$Avr_Freq))/sd(category_freq$Avr_Freq),9)

#####dataframes#####
ind <- which(colnames(trainfreq)=="freq" | colnames(trainfreq)=="Largest_view" | colnames(trainfreq)=="Category" | colnames(trainfreq)=="Likes" | colnames(trainfreq)=="Dislikes" | colnames(trainfreq)=="Comment")
knn_train <- trainfreq[,ind]
knn_train <- cbind(knn_train[2],knn_train[4:6],knn_train[3],knn_train[1])
knn_train$Likes <- as.integer(knn_train$Likes)
knn_train$Dislikes <- as.integer(knn_train$Dislikes)
knn_train$Comment <- as.integer(knn_train$Comment)

knn_valid <- validfreq[,ind]
knn_valid <- cbind(knn_valid[2],knn_valid[4:6],knn_valid[3],knn_valid[1])
knn_valid$Likes <- as.integer(knn_valid$Likes)
knn_valid$Dislikes <- as.integer(knn_valid$Dislikes)
knn_valid$Comment <- as.integer(knn_valid$Comment)

knn_test <- testfreq[,ind]
knn_test <- cbind(knn_test[2],knn_test[4:6],knn_test[3],knn_test[1])
knn_test$Likes <- as.integer(knn_test$Likes)
knn_test$Dislikes <- as.integer(knn_test$Dislikes)
knn_test$Comment <- as.integer(knn_test$Comment)

for (i in 1:nrow(category_freq)){
  tmp2 <- which(knn_train$Category==category_freq$x[i])
  knn_train$Category[tmp2] <- category_freq$Avr_Freq[i]
  tmp2 <- which(knn_valid$Category==category_freq$x[i])
  knn_valid$Category[tmp2] <- category_freq$Avr_Freq[i]
  tmp2 <- which(knn_test$Category==category_freq$x[i])
  knn_test$Category[tmp2] <- category_freq$Avr_Freq[i]
}

#####KNN#####
train.norm.df <- knn_train
valid.norm.df <- knn_valid
new.norm.df <- knn_test
norm.values <- preProcess(knn_train,method=c("center","scale"))
train.norm.df <- predict(norm.values, knn_train)
valid.norm.df <- predict(norm.values, knn_valid)
new.norm.df <- predict(norm.values, knn_test)

train.norm.df[,6] <- ifelse(train.norm.df[,6]>=0,"1","0")
valid.norm.df[,6] <- ifelse(valid.norm.df[,6]>=0,"1","0")
new.norm.df[,6] <- ifelse(new.norm.df[,6]>=0,"1","0")

accuracy.df <- data.frame(k=seq(1,20),accuracy=rep(0,20))

valid.norm.df[,6] <- factor(valid.norm.df[,6])
for (i in 1:length(accuracy.df$k)){
  knn.pred <- class::knn(train.norm.df[,-6],valid.norm.df[,-6],cl=train.norm.df[,6],k=i)
  accuracy.df[i,2] <- confusionMatrix(knn.pred, valid.norm.df[,6])$overall[1]
}
accuracy.df

nn <- knn(train = train.norm.df[,-6], test = new.norm.df[,-6], cl = train.norm.df[,6], k=19)
table(new.norm.df[,6],nn)
new.norm.df[,6] <- factor(new.norm.df[,6])
confusionMatrix(nn, new.norm.df[,6])

#####Lift Chart & ROC#####
nn <- knn(train = train.norm.df[,-6], test = new.norm.df[,-6], cl = train.norm.df[,6], k=14, prob = TRUE)

pred.KNN <- as.data.frame(cbind(as.character(new.norm.df[,6]),as.character(nn),round(attr(nn,"prob"),5)))
colnames(pred.KNN) <- c("freq","predicted","probability")
pred.KNN <- remove.factors(pred.KNN)
pred.KNN$freq <- factor(pred.KNN$freq)
pred <- prediction(as.numeric(pred.KNN$probability), pred.KNN$freq)

perf2 <- performance(pred, "tpr", "rpp")
plot(perf2, main="Lift Chart", colorize=F)

pred.KNN$probability <- as.numeric(pred.KNN$probability)
lift.KNN <- lift(freq ~ probability, data = pred.KNN, cuts=10, class="1")
xyplot(lift.KNN, main="KNN - Lift Chart", type=c("l","g"), lwd=2
       , scales=list(x=list(alternating=FALSE,tick.number = 10)
                     ,y=list(alternating=FALSE,tick.number = 10)))

#####Normalized by themself#####
train.norm.df <- cbind(MLRnormtrain[2:6],train.norm.df[6])
valid.norm.df <- cbind(MLRnormvalid[2:6],valid.norm.df[6])
new.norm.df <- cbind(MLRnormtest[2:6],new.norm.df[6])

accuracy.df <- data.frame(k=seq(1,20),accuracy=rep(0,20))

valid.norm.df[,6] <- factor(valid.norm.df[,6])
for (i in 1:length(accuracy.df$k)){
  knn.pred <- class::knn(train.norm.df[,-6],valid.norm.df[,-6],cl=train.norm.df[,6],k=i)
  accuracy.df[i,2] <- confusionMatrix(knn.pred, valid.norm.df[,6])$overall[1]
}
accuracy.df

nn_7 <- knn(train = train.norm.df[,-6], test = new.norm.df[,-6], cl = train.norm.df[,6], k=7)
table(new.norm.df[,6],nn_7)
new.norm.df[,6] <- factor(new.norm.df[,6])
confusionMatrix(nn_7, new.norm.df[,6])$overall[1]

nn_16 <- knn(train = train.norm.df[,-6], test = new.norm.df[,-6], cl = train.norm.df[,6], k=16)
table(new.norm.df[,6],nn_16)
new.norm.df[,6] <- factor(new.norm.df[,6])
confusionMatrix(nn_7, new.norm.df[,6])$overall[1]

#####Avr. Channel#####
tmp <- data.frame(trainfreq$Channel_title, as.integer(trainfreq$freq))
colnames(tmp) <- c("Channel", "freq")
tmp <- tmp[order(tmp$Channel),]
channel_freq <- plyr::count(tmp$Channel)

for (i in 1:nrow(channel_freq)){
  tmp2 <- subset(tmp,tmp$Channel==channel_freq$x[i])
  avr <- round(sum(tmp2$freq)/nrow(tmp2),2)
  channel_freq$Avr_Freq[i] <- avr
}
channel_freq$Norm <- round((channel_freq$Avr_Freq-mean(channel_freq$Avr_Freq))/sd(channel_freq$Avr_Freq),9)

knn_normalize_train <- cbind(MLRnormtrain,"Category" = trainfreq$Category,"Channel" = trainfreq$Channel_title)
for (i in 1:nrow(category_freq)){
  tmp2 <- which(knn_normalize_train$Category==category_freq$x[i])
  knn_normalize_train$Category[tmp2] <- category_freq$Norm[i]
}
knn_normalize_train <- remove.factors(knn_normalize_train)
for (i in 1:nrow(channel_freq)){
  tmp2 <- which(knn_normalize_train$Channel==channel_freq$x[i])
  knn_normalize_train$Channel[tmp2] <- channel_freq$Norm[i]
}
