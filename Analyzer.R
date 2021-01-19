setwd("")

library(ggplot2)

media_list <- read.csv("media_list.csv", stringsAsFactors = FALSE)
media_list <- media_list$Media

breakpoint <- as.Date("2020-08-06")

###Paired dot chart (outliers removed)

paired <- data.frame(type="fake", media="fake", count=0, metrics="fake")
temp <- data.frame(type="", media="", count=0, metrics=c("retweets", "likes"))

for (media in media_list){
  file_name <- paste0(media, ".csv")
  file <- read.csv(file_name, stringsAsFactors = FALSE)[,1:6]
  for (i in 1:nrow(file)){
    file[i,1] <- strsplit(file[i,1], " ")[[1]][1]
  }
  file$date <- as.Date(file$date)
  
  file$type <- ifelse(file$date<breakpoint,"1before","2after")
  
  for (i in c("1before","2after")){
    sub <- file[which(file$type==i), ]
    
    retweets_outlier <- boxplot.stats(sub$retweets, coef = 3)$out
    likes_outlier <- boxplot.stats(sub$likes, coef = 3)$out
    
    retweets <- sub$retweets
    likes <- sub$likes
    
    temp[, 1] <- i
    temp[, 2] <- media
    temp[1, 3] <- (sum(retweets)-sum(retweets_outlier))/(length(retweets)-length(retweets_outlier))
    temp[2, 3] <- (sum(likes)-sum(likes_outlier))/(length(likes)-length(likes_outlier))
    
    paired <- rbind(paired, temp)
    
  }
}

paired <- paired[-1,]

### merge
merged <- data.frame(media=media_list)
merged$beforeLikes <- 0
merged$afterLikes <- 0
merged$beforeRetweets <- 0
merged$afterRetweets <- 0

for (i in 1:nrow(merged)){
  media <- as.character(merged[i,1])
  merged[i,2] <- paired[which(paired$type=="1before" & paired$media==media & paired$metrics=="likes"), 3]
  merged[i,3] <- paired[which(paired$type=="2after" & paired$media==media & paired$metrics=="likes"), 3]
  merged[i,4] <- paired[which(paired$type=="1before" & paired$media==media & paired$metrics=="retweets"), 3]
  merged[i,5] <- paired[which(paired$type=="2after" & paired$media==media & paired$metrics=="retweets"), 3]
}

merged$diffLikes <- merged$afterLikes - merged$beforeLikes
merged$diffRetweets <- merged$afterRetweets - merged$beforeRetweets

write.csv(merged, "results.csv")

###whether or not allowed to advertise added manually
###Visualization
results <- read.csv("results.csv", stringsAsFactors = FALSE)
results$rateLikes <- (results$afterLikes-results$beforeLikes)/results$beforeLikes*100
results$rateRetweets <- (results$afterRetweets-results$beforeRetweets)/results$beforeRetweets*100

ggplot(results, aes(x=beforeLikes, y=afterLikes, group=advertise, color=advertise))+
  geom_point()+
  geom_text(aes(label=media), vjust = 0, nudge_y =2)

ggplot(results[which(results$media!="XinhuaTravel"&results$media!="ipandacom"),], 
       aes(x=beforeLikes, y=afterLikes, group=advertise, color=advertise))+
  geom_point()+
  geom_text(aes(label=media), vjust = 0, nudge_y =2)

ggplot(results, aes(x=beforeRetweets, y=afterRetweets, group=advertise, color=advertise))+
  geom_point()+
  geom_text(aes(label=media), vjust = 0, nudge_y =2)

ggplot(results[which(results$media!="XinhuaTravel"&results$media!="ipandacom"&results$media!="PDChina"&results$media!="PDChinaLife"),], 
       aes(x=beforeRetweets, y=afterRetweets, group=advertise, color=advertise))+
  geom_point()+
  geom_text(aes(label=media), vjust = 0, nudge_y =2)

ggplot(results, aes(x=log10(beforeLikes), y=rateLikes, group=advertise, color=advertise))+
  geom_point()+
  geom_text(aes(label=media), vjust = 0, nudge_y =2)
ggplot(results[which(results$media!="PDChinaSports"),], aes(x=log10(beforeLikes), y=rateLikes, group=advertise, color=advertise))+
  geom_point()+
  geom_text(aes(label=media), vjust = 0, nudge_y =2)


ggplot(results, aes(x=log10(beforeRetweets), y=rateRetweets, group=advertise, color=advertise))+
  geom_point()+
  geom_text(aes(label=media), vjust = 0, nudge_y =2)
ggplot(results[which(results$media!="PDChinaSports"),], aes(x=log10(beforeRetweets), y=rateRetweets, group=advertise, color=advertise))+
  geom_point()+
  geom_text(aes(label=media), vjust = 0, nudge_y =2)