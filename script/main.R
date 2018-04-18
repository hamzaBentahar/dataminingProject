library(ggplot2)
library(data.table)
library(tidytext)
library(corrplot)
library(wordcloud)
library(ggdendro)

us_data <- as.data.frame.matrix(fread("datamining project/data/USvideos.csv", nrows = 5000)[,"Location":="US"])
merged_us <- aggregate( .~ title, us_data, function(x) toString(unique(x)))
merged_us$trending_days <- strsplit(merged_us$trending_date, ",")
for(row in 1:nrow(merged_us)){
  merged_us[row, "trending_days"] <- length(unlist(merged_us[row, "trending_days"]))
}

fr_data <- as.data.frame.matrix(fread("datamining project/data/FRvideos.csv", nrows = 5000)[,"Location":="FR"])
merged_fr <- aggregate( .~ title, fr_data, function(x) toString(unique(x)))
merged_fr$trending_days <- strsplit(merged_fr$trending_date, ",")
for(row in 1:nrow(merged_fr)){
  merged_fr[row, "trending_days"] <- length(unlist(merged_fr[row, "trending_days"]))
}

gb_data <- as.data.frame.matrix(fread("datamining project/data/GBvideos.csv", nrows = 5000)[,"Location":="GB"])
merged_gb <- aggregate( .~ title, gb_data, function(x) toString(unique(x)))
merged_gb$trending_days <- strsplit(merged_gb$trending_date, ",")
for(row in 1:nrow(merged_gb)){
  merged_gb[row, "trending_days"] <- length(unlist(merged_gb[row, "trending_days"]))
}

merged_all <- as.data.table(rbind(merged_gb,merged_fr,merged_us))
videos <- as.data.table(rbind(gb_data,fr_data,us_data))

View(merged_us)
videos <- as.data.table(merged_us)
ggplot(videos[,.N,by=category_id][order(-N)],aes(reorder(category_id,-N),N,fill=as.factor(category_id)))+geom_bar(stat="identity")+guides(fill="none")+labs(caption="Donyoe",title=" Top Category ID")+
  xlab(NULL)+ylab(NULL)
#ggplot(videos[,.N,by=trending_days][order(-N)],aes(reorder(trending_days,-N),N,fill=as.factor(trending_days)))+geom_bar(stat="identity")+guides(fill="none")+labs(caption="Donyoe",title=" Top Category ID")+
#  xlab(NULL)+ylab(NULL)
ggplot(videos[,.N,by=channel_title][order(-N)][1:10],aes(reorder(channel_title,-N),N,fill=channel_title))+geom_bar(stat="identity")+geom_label(aes(label=N))+guides(fill="none")+theme(axis.text.x = element_text(angle = 45,hjust = 1))+  labs(caption="Donyoe",title=" Top trending channel titles in all countries")+
  xlab(NULL)+ylab(NULL)+coord_flip()
videos <- as.data.table(merged_us)
#corrplot.mixed(corr = cor(videos[,c("category_id","views","likes","dislikes","comment_count"),with=F]))

videos[,"Percentage_Likes":=round(100*(likes)/sum(as.numeric(views),na.rm = T),digits = 4)]
videos[,"Percentage_Disikes":=round(100*(dislikes)/sum(as.numeric(views),na.rm = T),digits = 4)]
videos[,"Percentage_comments":=round(100*(comment_count)/sum(as.numeric(views),na.rm = T),digits = 4)]
dista <- dist(x = videos)
cluster <- hclust(dista,method = "ward.D")
ggdendrogram(cluster)
