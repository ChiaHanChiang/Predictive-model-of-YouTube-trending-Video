tv <- df[1:24000,]
#####New df: tvfreq#####
freq <- plyr::count(tv$video_id)
freq <- freq[order(freq$freq, decreasing = T),]
tvfreq <- freq
colnames(tvfreq) <- c("Video_id","freq")

tmp <- data.frame("Video_id"=c(as.character(tvfreq$Video_id)),"CE"=rep(0,length(tvfreq$Video_id)),"RE"=rep(0,length(tvfreq$Video_id)),"Largest_view"=rep(0,length(tvfreq$Video_id)),"Category"=rep(0,length(tvfreq$Video_id)),"Channel_title"=rep(0,length(tvfreq$Video_id)))
for (i in 1:length(tmp$Video_id)){
  tmp2 <- subset(tv, tv$video_id==as.character(tmp$Video_id[i]))
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
tvfreq <- merge(tvfreq,tmp, by="Video_id")
tvfreq <- tvfreq[order(tvfreq$freq, decreasing = T),]

ind <- which(tvfreq$RE=="False" & tvfreq$Likes!=0)
tvfreq$RE[ind] <- "True"
ind <- which(tvfreq$CE=="False" & tvfreq$Comment!=0)
tvfreq$CE[ind] <- "True"

ind <- which(tvfreq$RE=="False" | tvfreq$CE=="False")
tvfreq <- tvfreq[-ind,]


#####MLR#####
ind <- which(colnames(tvfreq)=="Likes"|colnames(tvfreq)=="Dislikes"|colnames(tvfreq)=="Largest_view"|colnames(tvfreq)=="Comment"|colnames(tvfreq)=="Video_id"|colnames(tvfreq)=="freq")
MLR_tv <- data.frame(tvfreq[,ind])
#####MLRnormtv:Normalized tv data#####
MLRnormtv <- MLR_tv
MLRnormtv$Largest_view <- (MLR_tv$Largest_view-mean(MLR_tv$Largest_view))/sd(MLR_tv$Largest_view)
MLR_tv$Likes <- as.integer(MLR_tv$Likes)
MLRnormtv$Likes <- (MLR_tv$Likes-mean(MLR_tv$Likes))/sd(MLR_tv$Likes)
MLR_tv$Dislikes<- as.integer(MLR_tv$Dislikes)
MLRnormtv$Dislikes <- (MLR_tv$Dislikes-mean(MLR_tv$Dislikes))/sd(MLR_tv$Dislikes)
MLR_tv$Comment <- as.integer(MLR_tv$Comment)
MLRnormtv$Comment <- (MLR_tv$Comment-mean(MLR_tv$Comment))/sd(MLR_tv$Comment)
MLRnormtv$freq <- (MLR_tv$freq-mean(MLR_tv$freq))/sd(MLR_tv$freq)

#####MLR#####
fit2 <- lm(freq ~ Largest_view + Likes + Dislikes + Comment, data = MLRnormtv)
summary(fit2)
stepAIC(fit2, direction ="backward")
step <- stepAIC(fit2, direction ="backward")

for (i in 1:nrow(MLRnormtv)){
  MLRnormtv$py[i] <- step$coefficients[1] + step$coefficients[2]* MLRnormtv$Largest_view[i] + step$coefficients[3]* MLRnormtv$Dislikes[i] + step$coefficients[4]* MLRnormtv$Comment[i]
}

RMSE_tv <- sqrt(sum((MLRnormtv$freq-MLRnormtv$py)^2)/nrow(MLRnormtv))
