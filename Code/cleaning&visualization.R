load("/Users/CCH/Desktop/---/19Fall_DataMining/Project/USvideos.Rda")
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(GGally)
library(tidyverse)
library(hrbrthemes)
library(viridis)
###########################################################################################################################################################################
summary(df$views,df$likes,df$dislikes,df$comment_count)
par(mar=c(1.8,1.8,1.8,1.8))
par(mfrow=c(2,2))
hist(df$views,main="Histogram of views", col = "darkgrey", cex.main=1, breaks = 100)
hist(df$likes,main="Histogram of likes", col = "darkgrey", cex.main=1, breaks = 100)
hist(df$dislikes,main="Histogram of dislikes", col = "darkgrey", cex.main=1, breaks = 100)
hist(df$comment_count,main="Histogram of comment count", col = "darkgrey", cex.main=1, breaks = 100)

cateid<-unique(df$category_id)
q1<-data.frame(cateid,stringsAsFactors = FALSE)
q1$Count<-0
q1<-q1[order(q1$cateid),]

for(i in 1:length(df$category_id)){
  ind<-which(q1$cateid==df$category_id[i])
  q1$Count[ind]=q1$Count[ind]+1
}

length(unique(df$trending_date))
par(mfrow=c(1,1))
pairs(~temp+RH+DC+DMC,data=fire, main="Scatter Matrix", lower.panel = NULL, upper.panel = upper.panel)

upper.panel<-function(x, y){ points(x,y, pch=19,  col=("steelblue"))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}
pairs(~views+likes+dislikes+comment_count,data=df, main="Scatter Matrix", lower.panel = NULL, upper.panel = upper.panel)

######Set up#####
ind <- which(is.na(df))

ind <- which(colnames(df)=="thumbnail_link" | colnames(df)=="description" | colnames(df)=="video_error_or_removed")
df <- df[-ind]

df$trending_date <- as.Date(df$trending_date,format='%y.%d.%m')

tmp <- data.frame(strsplit(as.character(df$publish_time), "T"))
colnames(tmp) <- c(1:length(tmp))
tmp <- data.frame(t(tmp))
colnames(tmp) <- c("publish_date","publish_time")
tmp$publish_time <- gsub(pattern = ".000Z", replacement = "", x = tmp$publish_time)
ind <- which(colnames(df)=="publish_time")
df <- df[-ind]
df <- cbind(df[1:ind-1],tmp,df[ind:length(df)])
df <- df[1:36000,]

#####Weekdays:Not use#####
df <- df[1:12000,]
tmp <- data.frame("Weekdays"=rep("",length(df$publish_date)))#,"Time_of_Day"=rep("",length(df$publish_date))
tmp$Weekdays <- as.character(tmp$Weekdays)
#tmp$Time_of_Day <- as.character(tmp$Time_of_Day)
for (i in 1:length(df$publish_date)){
  if (weekdays(as.Date(df$publish_date[i]))=="Sunday" | weekdays(as.Date(df$publish_date[i]))=="Saturday"){
    tmp$Weekdays[i] <- c("Weekend")

    
  }else{
    tmp$Weekdays[i] <- c("Weekday")
  }
}
ind <- which(colnames(df)=="publish_date")
df <- cbind(df, tmp)
df <- cbind(df[1:ind],tmp[1],df[ind+1],tmp[2],df[(ind+2):length(df)])

#####Get train, valid, test data#####
train <- df[1:12000,]
valid <- df[12001:24000,]
test <- df[24001:36000,]


#####Keyword freq: not use#####
keyword <- data.frame(str_split(as.character(train$tags), "\\|", simplify = TRUE),stringsAsFactors= FALSE)
keyword <- stack(keyword)
keyword <- plyr::count(keyword$values)
keyword <- keyword[order(keyword$freq, decreasing = T),]

ind <- which(keyword$x=="" | keyword$x=="[none]")
keyword <- keyword[-ind,]
colnames(keyword) <- c("Keyword","Freq")

#####New df: trainfreq#####
freq <- plyr::count(train$video_id)
freq <- freq[order(freq$freq, decreasing = T),]
trainfreq <- freq
colnames(trainfreq) <- c("Video_id","freq")

tmp <- data.frame("Video_id"=c(as.character(trainfreq$Video_id)),"CE"=rep(0,length(trainfreq$Video_id)),"RE"=rep(0,length(trainfreq$Video_id)),"Largest_view"=rep(0,length(trainfreq$Video_id)),"Category"=rep(0,length(trainfreq$Video_id)),"Channel_title"=rep(0,length(trainfreq$Video_id)))
for (i in 1:length(tmp$Video_id)){
  tmp2 <- subset(train, train$video_id==as.character(tmp$Video_id[i]))
  tmp$Largest_view[i] <- tmp2$views[length(tmp2$views)]
  tmp$Category[i] <- tmp2$category_id[1]
  tmp$Channel_title[i] <- as.character(tmp2$channel_title[1])
  tmp$Likes[i] <- as.character(tmp2$likes[length(tmp2$likes)])
  tmp$Dislikes[i] <- as.character(tmp2$dislikes[length(tmp2$dislikes)])
  tmp$Comment[i] <- as.character(tmp2$comment_count[length(tmp2$comment_count)])
  if (tmp2$comments_disabled[1]=="False"){
    tmp$CE[i] <- as.character("True")
  }else{
    tmp$CE[i] <- as.character("False")
  }
  if (tmp2$ratings_disabled[1]=="False"){
    tmp$RE[i] <- as.character("True")
  }else{
    tmp$RE[i] <- as.character("False")
  }
}
trainfreq <- merge(trainfreq,tmp, by="Video_id")
trainfreq <- trainfreq[order(trainfreq$freq, decreasing = T),]

ind <- which(trainfreq$RE=="False" & trainfreq$Likes!=0)
trainfreq$RE[ind] <- "True"
ind <- which(trainfreq$CE=="False" & trainfreq$Comment!=0)
trainfreq$CE[ind] <- "True"

ind <- which(trainfreq$RE=="False" | trainfreq$CE=="False")
trainfreq <- trainfreq[-ind,]

#####New df: validfreq#####
freq <- plyr::count(valid$video_id)
freq <- freq[order(freq$freq, decreasing = T),]
validfreq <- freq
colnames(validfreq) <- c("Video_id","freq")

tmp <- data.frame("Video_id"=c(as.character(validfreq$Video_id)),"CE"=rep(0,length(validfreq$Video_id)),"RE"=rep(0,length(validfreq$Video_id)),"Largest_view"=rep(0,length(validfreq$Video_id)),"Category"=rep(0,length(validfreq$Video_id)),"Channel_title"=rep(0,length(validfreq$Video_id)))
for (i in 1:length(tmp$Video_id)){
  tmp2 <- subset(valid, valid$video_id==as.character(tmp$Video_id[i]))
  tmp$Largest_view[i] <- tmp2$views[length(tmp2$views)]
  tmp$Category[i] <- tmp2$category_id[1]
  tmp$Channel_title[i] <- as.character(tmp2$channel_title[1])
  tmp$Likes[i] <- as.character(tmp2$likes[length(tmp2$likes)])
  tmp$Dislikes[i] <- as.character(tmp2$dislikes[length(tmp2$dislikes)])
  tmp$Comment[i] <- as.character(tmp2$comment_count[length(tmp2$comment_count)])
  if (tmp2$comments_disabled[1]=="False"){
    tmp$CE[i] <- as.character("True")
  }else{
    tmp$CE[i] <- as.character("False")
  }
  if (tmp2$ratings_disabled[1]=="False"){
    tmp$RE[i] <- as.character("True")
  }else{
    tmp$RE[i] <- as.character("False")
  }
}
validfreq <- merge(validfreq,tmp, by="Video_id")
validfreq <- validfreq[order(validfreq$freq, decreasing = T),]

ind <- which(validfreq$RE=="False" & validfreq$Likes!=0)
validfreq$RE[ind] <- "True"
ind <- which(validfreq$CE=="False" & validfreq$Comment!=0)
validfreq$CE[ind] <- "True"

ind <- which(validfreq$RE=="False" | validfreq$CE=="False")
validfreq <- validfreq[-ind,]


#####New df: testfreq#####
freq <- plyr::count(test$video_id)
freq <- freq[order(freq$freq, decreasing = T),]
testfreq <- freq
colnames(testfreq) <- c("Video_id","freq")

#tmp <- data.frame("Video_id"=c(as.character(trainfreq$Video_id)),"Largest_view"=rep(0,length(trainfreq$Video_id)),"Category"=rep(0,length(trainfreq$Video_id)),"Channel_title"=rep(0,length(trainfreq$Video_id)),"Weekdays"=rep(0,length(trainfreq$Video_id)),"Time"=rep(0,length(trainfreq$Video_id)),"Keyword1"=rep(0,length(trainfreq$Video_id)),"Keyword2"=rep(0,length(trainfreq$Video_id)),"Keyword3"=rep(0,length(trainfreq$Video_id)),"Keyword4"=rep(0,length(trainfreq$Video_id)),"Keyword5"=rep(0,length(trainfreq$Video_id)))
tmp <- data.frame("Video_id"=c(as.character(testfreq$Video_id)),"CE"=rep(0,length(testfreq$Video_id)),"RE"=rep(0,length(testfreq$Video_id)),"Largest_view"=rep(0,length(testfreq$Video_id)),"Category"=rep(0,length(testfreq$Video_id)),"Channel_title"=rep(0,length(testfreq$Video_id)))
for (i in 1:length(tmp$Video_id)){
  tmp2 <- subset(test, test$video_id==as.character(tmp$Video_id[i]))
  tmp$Largest_view[i] <- tmp2$views[length(tmp2$views)]
  tmp$Category[i] <- tmp2$category_id[1]
  tmp$Channel_title[i] <- as.character(tmp2$channel_title[1])
  tmp$Likes[i] <- as.character(tmp2$likes[length(tmp2$likes)])
  tmp$Dislikes[i] <- as.character(tmp2$dislikes[length(tmp2$dislikes)])
  tmp$Comment[i] <- as.character(tmp2$comment_count[length(tmp2$comment_count)])
  if (tmp2$comments_disabled[1]=="False"){
    tmp$CE[i] <- as.character("True")
  }else{
    tmp$CE[i] <- as.character("False")
  }
  if (tmp2$ratings_disabled[1]=="False"){
    tmp$RE[i] <- as.character("True")
  }else{
    tmp$RE[i] <- as.character("False")
  }
}
testfreq <- merge(testfreq,tmp, by="Video_id")
testfreq <- testfreq[order(testfreq$freq, decreasing = T),]

ind <- which(testfreq$RE=="False" & testfreq$Likes!=0)
testfreq$RE[ind] <- "True"
ind <- which(testfreq$CE=="False" & testfreq$Comment!=0)
testfreq$CE[ind] <- "True"

ind <- which(testfreq$RE=="False" | testfreq$CE=="False")
testfreq <- testfreq[-ind,]

#####For enable:not sure usage#####

tmp2 <- subset(trainfreq, trainfreq$RE == "True" & trainfreq$CE == "True")
tmp <- data.frame(Class=c(rep(0,length(trainfreq$Video_id))), freq=c(rep(0,length(trainfreq$Video_id))))
for (i in 1:length(tmp$Class)){
  if (trainfreq$RE[i] == "True" & trainfreq$CE[i] == "True"){
    tmp$Class[i] <- "RC"
    tmp$freq[i] <- trainfreq$freq[i]
  }else if (trainfreq$RE[i] == "True" & trainfreq$CE[i] == "False"){
    tmp$Class[i] <- "R"
    tmp$freq[i] <- trainfreq$freq[i]
  }else if (trainfreq$RE[i] == "False" & trainfreq$CE[i] == "True"){
    tmp$Class[i] <- "C"
    tmp$freq[i] <- trainfreq$freq[i]
  }else {
    tmp$Class[i] <- "None"
    tmp$freq[i] <- trainfreq$freq[i]
  }
}

tmp$Class <- factor(tmp$Class,levels=c('RC','R', 'C', 'None'),ordered = TRUE)
tmp <- tmp[order(tmp$Class),]

tmp %>%
  ggplot( aes( x = Class, y = freq, fill = Class)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  theme_ipsum() +
  theme(
    legend.position = "none",
  ) +
  ylim(0,15) +
  xlab("Enabled")



#####Visualization#####
#bar chart - top10 freq
table <- trainfreq[1:20,1:2]
ggplot(table, aes(x=reorder(Video_id,freq), y=freq))+geom_bar(stat = "identity",fill="#FF6347")+ylim(c(0,15))+theme_minimal() +theme(axis.text.x = element_text(size=8,angle = 90))+xlab("Video_id")+ylab("freq")

trainfreq <- trainfreq[,-(3:4)]
table <- trainfreq[1:20,(1:3)]
ggplot(table, aes(x=Video_id, y=Largest_view))+geom_bar(stat = "identity",fill="#FF6347")+ 
  scale_y_continuous(labels = c("0","1m","2m","3m","4m","5m"))+theme_minimal() +theme(axis.text.x = element_text(size=8,angle = 90))

#bar chart - top10 views
trainfreq <- trainfreq[order(trainfreq$Largest_view,decreasing = T),]
table <- trainfreq[1:10,1:3]
ggplot(table, aes(x=reorder(Video_id,Largest_view), y=Largest_view))+geom_bar(stat = "identity",fill="darkgreen")+scale_y_continuous(labels = c("0","5m","10m","15m"))+theme_minimal()+theme(axis.text.x = element_text(size=8,angle = 90))+xlab("Video_id")

ggplot(table, aes(x=reorder(Video_id,Largest_view), y=freq))+geom_bar(stat = "identity", fill="darkgreen")+ylim(c(0,15))+theme_minimal()+theme(axis.text.x = element_text(size=8,angle = 90))+xlab("Video_id")

#bar chart - top20 freq
trainfreq <- trainfreq[order(trainfreq$freq, decreasing = T),]
table <- trainfreq[1:20,]
table$Video_id <- factor(table$Video_id, levels=rev(as.character(table$Video_id)))
ggplot(table, aes(x=reorder(Video_id,freq), y=freq))+geom_bar(stat = "identity",fill="#FF6347")+ylim(c(0,15))+theme_minimal() +theme(axis.text.x = element_text(size=8,angle = 90))+xlab("Video_id")

#bar chart - top20 views
ggplot(table, aes(x=reorder(Video_id,Largest_view), y=Largest_view))+geom_bar(stat = "identity",fill="darkgreen")+scale_y_continuous(labels = c("0","5m","10m","15m"))+theme_minimal()+theme(axis.text.x = element_text(size=8,angle = 90))+xlab("Video_id")

ggplot(table, aes(x=reorder(Video_id,Largest_view), y=freq))+geom_bar(stat = "identity", fill="darkgreen")+ylim(c(0,15))+theme_minimal()+theme(axis.text.x = element_text(size=8,angle = 90))+xlab("Video_id")

ggplot(table, aes(x=Video_id, y=Largest_view))+geom_bar(stat = "identity",fill="#FF6347")+ 
  scale_y_continuous(labels = c("0","1m","2m","3m","4m","5m"))+theme_minimal() +theme(axis.text.x = element_text(size=8,angle = 90))

#matrix
table <- cbind(as.integer(trainfreq$freq),as.integer(trainfreq$Largest_view),as.integer(trainfreq$Likes),as.integer(trainfreq$Dislikes),as.integer(trainfreq$Comment))
colnames(table) <- c("Freq","Views","Likes","Dislikes","Comments")

table <- data.frame(table)
ind <- which((table$Likes==0 & table$Dislikes==0)|table$Comments==0)
table <- table[-ind,]
#Outlier
ind <- which(table$Views>140000000 | table$Dislikes>1500000 | table$Comments>1250000)

table <- table[-ind,]

ggpairs(table)


cate_views <- plyr::count(trainfreq$Category)
colnames(cate_views) <- c("Category","freq1") 
ind <- which(cate_views$Category>31)
cate_views <- cate_views[-ind,]
for(i in 1:length(cate_views$Category)){
  tmp <- subset(trainfreq, trainfreq$Category==cate_views$Category[i])
  cate_views$freq2[i] <- sum(tmp$freq)/cate_views$freq1[i]
}

ggplot(cate_views, aes(x=reorder(as.character(Category),freq2), y=freq2))+
  geom_bar(stat = "identity", fill="#FF6347") +
  xlab("Category")+ ylab("Avg.Freq") +
  theme_minimal() 

#Category#
cate_views <- plyr::count(trainfreq$Category)
colnames(cate_views) <- c("Category","freq") 
tmp <- data.frame("Category"=c(cate_views$Category),"Avg.Views"=c(rep(0,length(cate_views$Category))))
for (i in 1:length(tmp$Category)){
  tmp2 <- subset(trainfreq, trainfreq$Category==as.character(tmp$Category[i]))
  tmp$Avg.Views[i] <- floor(sum(tmp2$Largest_view)/length(tmp2$Video_id))
}
cate_views <- merge(cate_views,tmp, by="Category")
ggplot(cate_views, aes(x=reorder(as.character(Category),Avg.Views), y=Avg.Views))+
  geom_bar(stat = "identity", fill="darkgreen") +
  xlab("Category") + 
  scale_y_continuous(labels = c("0","1m","2m","3m")) +
  theme_minimal()
cate_views <- cate_views[order(cate_views$Avg.Views,decreasing = T),]

cate_views_hist <- subset(trainfreq, trainfreq$Category==as.character(cate_views$Category[1]))
ggplot(cate_views_hist, aes(x=Largest_view))+geom_histogram(fill="darkgreen") +scale_x_continuous(limits = c(0,5000000), labels = c("0","1m","2m","3m","4m","5m"))+theme_minimal() 

#Cate_views_Outliers#
table <- subset(trainfreq, trainfreq$Video_id=="l_lblj8Cq0o" | trainfreq$Video_id=="_9YMpuLDnwo" | trainfreq$Video_id=="LsoLEjrDogU"| trainfreq$Video_id=="6ZfuNTqbHE8"| trainfreq$Video_id=="TyHvyGVs42U"| trainfreq$Video_id=="FlsCjmMhFmw")

table <- subset(trainfreq, trainfreq$Video_id=="sXP6vliZIHI")
table <- table[,-(8:12)]
tmp <- subset(train, train$video_id=="sXP6vliZIHI")
table$Comment_Enable
sd(as.integer(trainfreq$Comment))

#Disable#
tmp <- data.frame("Video_id"=as.character(trainfreq$Video_id),"Rating_Enabled"=c(rep(0,length(trainfreq$Video_id))),"Comment_Enabled"=c(rep(0,length(trainfreq$Video_id))))
for (i in 1:length(tmp$Video_id)){
  tmp2 <- subset(train, train$video_id==as.character(tmp$Video_id[i]))
  if (tmp2$comments_disabled[1]=="True"){
    tmp$Comment_Enabled[i] <- c("False")
  }else{
    tmp$Comment_Enabled[i] <- c("True")
  }
  if (tmp2$ratings_disabled[1]=="True"){
    tmp$Rating_Enabled[i] <- c("False")
  }else{
    tmp$Rating_Enabled[i] <- c("True")
  }
}
trainfreq <- merge(trainfreq,tmp, by="Video_id")

enable <- data.frame(cbind(trainfreq$freq,trainfreq$RE,trainfreq$CE))
colnames(enable) <- c("Freq","Rating_E","Comment_E")
enable$WE <- 0

for (i in 1:length(enable$Freq)){
  if (enable$Rating_E[i]=="True" & enable$Comment_E[i]=="True"){
    enable$WE[i] <- c("RC")
  }else if (enable$Rating_E[i]=="True" & enable$Comment_E[i]=="False"){
    enable$WE[i] <- c("R")
  }else if (enable$Rating_E[i]=="False" & enable$Comment_E[i]=="True"){
    enable$WE[i] <- c("C")
  }else if (enable$Rating_E[i]=="False" & enable$Comment_E[i]=="False"){
    enable$WE[i] <- c("NA")
  }
}

enable_set <- data.frame("Enable"=c(unique(enable$WE)),"Avg.Freq"=c(rep(0,length(4))))
for (i in 1:length(enable_set$Enable)){
  tmp <- subset(enable, enable$WE==enable_set$Enable[i])
  enable_set$Avg.Freq[i] <- sum(as.integer(tmp$Freq))/length(tmp$Freq)
  enable_set$times[i] <- length(tmp$Freq)
}

ggplot(enable_set, aes(x=reorder(as.character(Enable),Avg.Freq), y=Avg.Freq))+
  geom_bar(stat = "identity", fill="#FF6347") +
  xlab("Type") +
  theme_minimal()


##Channel_views##
channel_views <- plyr::count(trainfreq$Channel_title)
colnames(channel_views) <- c("Channel","freq") 
tmp <- data.frame("Channel"=as.character(channel_views$Channel),"Avg.Views"=c(rep(0,length(channel_views$Channel))))
for (i in 1:length(tmp$Channel)){
  tmp2 <- subset(trainfreq, trainfreq$Channel_title==as.character(tmp$Channel[i]))
  tmp$Avg.Views[i] <- floor(sum(tmp2$Largest_view)/length(tmp2$Video_id))
}
channel_views <- merge(channel_views,tmp, by="Channel")
channel_views <- channel_views[order(channel_views$Avg.Views,decreasing = T),]
channel_views <- channel_views[1:10,]
ggplot(channel_views, aes(x=reorder(as.character(Channel),Avg.Views), y=Avg.Views))+
  geom_bar(stat = "identity", fill="#FF6347") +
  xlab("Channel") +
  scale_y_continuous(labels = c("0","2.5m","5m","7.5m","10m")) +
  theme_minimal()+ theme(axis.text.x = element_text(size=8,angle = 90)) 


channel_views_hist <- subset(trainfreq, trainfreq$Channel_title==as.character(channel_views$Channel[1]))

##time_of_day##
timeofday <- data.frame("Time"=c(unique(trainfreq$Time)),"Avg.Views"=c(rep(0,4)))
for (i in 1:length(timeofday$Time)){
  tmp <- subset(trainfreq,trainfreq$Time==timeofday$Time[i])
  timeofday$Avg.Views[i] <- floor(sum(tmp$Largest_view)/length(tmp$Video_id))
}
ggplot(timeofday, aes(x=reorder(as.character(Time),Avg.Views), y=Avg.Views))+
  geom_bar(stat = "identity", fill="darkgreen") +
  xlab("Time of Day") +
  scale_y_continuous(limits = c(0,1750000)) +
  theme_minimal()

##Weekdays##
weekday <- data.frame("Weekdays"=c(unique(trainfreq$Weekdays)),"Avg.Freq"=c(rep(0,2)))
for (i in 1:length(weekday$Weekdays)){
  tmp <- subset(trainfreq,trainfreq$Weekdays==weekday$Weekdays[i])
  weekday$Avg.Freq[i] <- sum(tmp$freq)/length(tmp$Video_id)
}
ggplot(weekday, aes(x=as.character(Weekdays), y=Avg.Freq))+
  geom_bar(stat = "identity", fill="#FF6347") +
  xlab("Weekdays") + ylim(0,5) +
  theme_minimal()

#Percentile
vp <- trainfreq$Largest_view
vp <- quantile(vp, probs = seq(0, 1, by= 0.01))
vp <- data.frame(vp)

lp <- as.integer(trainfreq$Likes)
lp <- quantile(lp, probs = seq(0, 1, by= 0.01))
lp <- data.frame(lp)

dp <- as.integer(trainfreq$Dislikes)
dp <- quantile(dp, probs = seq(0, 1, by= 0.01))
dp <- data.frame(dp)

cp <- as.integer(trainfreq$Comment)
cp <- quantile(cp, probs = seq(0, 1, by= 0.01))
cp <- data.frame(cp)

percent <- data.frame(cbind(as.integer(trainfreq$freq),as.integer(trainfreq$Largest_view),as.integer(trainfreq$Likes),as.integer(trainfreq$Dislikes),as.integer(trainfreq$Comment)))
colnames(percent) <- c("Freq","View","Likes","Dislikes","Comments")
percent$View_P <- 0
percent$Likes_P <- 0
percent$Dislikes_P <- 0
percent$Comments_P <- 0
i<-1

for (i in 1:length(percent$Freq)){
  for (j in 1:101){
    if(percent$View[i]<vp$vp[j]){
      print("V")
      percent$View_P[i] <- ((j-1)/100)
      break
    }
    if(percent$Likes[i]<lp$lp[j]){
      print("L")
      percent$Likes_P[i] <- ((j-1)/100)
      break
    }
    if(percent$Dislikes[i]<dp$dp[j]){
      print("D")
      percent$Dislikes_P[i] <- ((j-1)/100)
      break
    }
    if(percent$Comments[i]<cp$cp[j]){
      print("C")
      percent$Comments_P[i] <- ((j-1)/100)
      break
    }
  }
}


##Cutoff & ##
trainfreq <- trainfreq[order(trainfreq$freq,decreasing = T),]
cutoff <- trainfreq[,1:2]
for (i in 1:length(cutoff$Video_id)){
  if (cutoff$freq[i]>=10){
    cutoff$Frequency[i] <- c("1")
  }else{
    cutoff$Frequency[i] <- c("0")
  }
  
  if (trainfreq$Largest_view[i]>1250000){
    cutoff$CaView[i] <- c("1")
  }else{
    cutoff$CaView[i] <- c("0")
  }
}


#video_id all freq
tmp <- plyr::count(df$video_id)
tmp <- tmp[order(tmp$freq, decreasing = T),]
colnames(tmp) <- c("Video_id","freq")
allfreq <- tmp





