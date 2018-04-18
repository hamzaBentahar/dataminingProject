library(ggplot2)
library(data.table)
library(tidytext)
library(corrplot)
library(wordcloud)
library(ggdendro)

us_data <- as.data.frame.matrix(fread("datamining project/data/USvideos.csv", nrows = 5000)[,"Location":="US"])
merged_us <- as.data.table(aggregate( .~ title, us_data, function(x) toString(unique(x))))
merged_us$trending_days <- sapply(merged_us$trending_date, function(x) length(unlist(strsplit(as.character(x), ","))))

fr_data <- as.data.frame.matrix(fread("datamining project/data/FRvideos.csv", nrows = 5000)[,"Location":="FR"])
merged_fr <- as.data.table(aggregate( .~ title, fr_data, function(x) toString(unique(x))))
merged_fr$trending_days <- sapply(merged_fr$trending_date, function(x) length(unlist(strsplit(as.character(x), ","))))

gb_data <- as.data.frame.matrix(fread("datamining project/data/GBvideos.csv", nrows = 5000)[,"Location":="GB"])
merged_gb <- as.data.table(aggregate( .~ title, gb_data, function(x) toString(unique(x))))
merged_gb$trending_days <- sapply(merged_gb$trending_date, function(x) length(unlist(strsplit(as.character(x), ","))))

merged_all <- as.data.table(rbind(merged_gb,merged_fr,merged_us))
videos <- as.data.table(rbind(gb_data,fr_data,us_data))

ggplot(videos[,.N,by=category_id][order(-N)],aes(reorder(category_id,-N),N,fill=as.factor(category_id)))+geom_bar(stat="identity")+guides(fill="none")+labs(caption="Wiam Benhammou, Hamza Bentahar, Yousra Gaimes",title=" Top Category ID")+
  xlab(NULL)+ylab(NULL)
ggplot(merged_us[,.N,by=trending_days][order(-N)],aes(reorder(trending_days,-N),N,fill=as.factor(trending_days)))+geom_bar(stat="identity")+guides(fill="none")+labs(caption="Wiam Benhammou, Hamza Bentahar, Yousra Gaimes",title="Number of trending days in United States")+
  xlab(NULL)+ylab(NULL)
ggplot(merged_fr[,.N,by=trending_days][order(-N)],aes(reorder(trending_days,-N),N,fill=as.factor(trending_days)))+geom_bar(stat="identity")+guides(fill="none")+labs(caption="Wiam Benhammou, Hamza Bentahar, Yousra Gaimes",title="Number of trending days in France")+
  xlab(NULL)+ylab(NULL)
ggplot(merged_gb[,.N,by=trending_days][order(-N)],aes(reorder(trending_days,-N),N,fill=as.factor(trending_days)))+geom_bar(stat="identity")+guides(fill="none")+labs(caption="Wiam Benhammou, Hamza Bentahar, Yousra Gaimes",title="Number of trending days in Great Britain")+
  xlab(NULL)+ylab(NULL)
ggplot(merged_all[,.N,by=trending_days][order(-N)],aes(reorder(trending_days,-N),N,fill=as.factor(trending_days)))+geom_bar(stat="identity")+guides(fill="none")+labs(caption="Wiam Benhammou, Hamza Bentahar, Yousra Gaimes",title="Number of trending days in FR, US and GB")+
  xlab(NULL)+ylab(NULL)
ggplot(videos[,.N,by=channel_title][order(-N)][1:10],aes(reorder(channel_title,-N),N,fill=channel_title))+geom_bar(stat="identity")+geom_label(aes(label=N))+guides(fill="none")+theme(axis.text.x = element_text(angle = 45,hjust = 1))+  labs(caption="Wiam Benhammou, Hamza Bentahar, Yousra Gaimes",title=" Top trending channel titles in all countries")+
  xlab(NULL)+ylab(NULL)+coord_flip()
videos <- as.data.table(merged_us)
#corrplot.mixed(corr = cor(videos[,c("category_id","views","likes","dislikes","comment_count"),with=F]))

videos[,"Percentage_Likes":=round(100*(likes)/sum(as.numeric(views),na.rm = T),digits = 4)]
videos[,"Percentage_Disikes":=round(100*(dislikes)/sum(as.numeric(views),na.rm = T),digits = 4)]
videos[,"Percentage_comments":=round(100*(comment_count)/sum(as.numeric(views),na.rm = T),digits = 4)]
distMatrixComp <- dist(x = videos)
cluster <- hclust(distMatrixComp,method = "ward.D")
ggdendrogram(cluster)

merged_all$isMultiple <- cut(merged_all$trending_days, 
                breaks = c(-Inf, 2, Inf), 
                labels = c("one day", 'more than one day'), 
                right = FALSE)
ggplot(merged_all[,.N,by=isMultiple][order(-N)],aes(reorder(isMultiple,-N),N,fill=as.factor(isMultiple)))+geom_bar(stat="identity")+guides(fill="none")+labs(caption="Wiam Benhammou, Hamza Bentahar, Yousra Gaimes",title="Number of trending days in FR, US and GB")+
  xlab(NULL)+ylab(NULL)

merged_us$isMultiple <- cut(merged_us$trending_days, 
                             breaks = c(-Inf, 2, Inf), 
                             labels = c("one day", 'more than one day'), 
                             right = FALSE)
ggplot(merged_us[,.N,by=isMultiple][order(-N)],aes(reorder(isMultiple,-N),N,fill=as.factor(isMultiple)))+geom_bar(stat="identity")+guides(fill="none")+labs(caption="Wiam Benhammou, Hamza Bentahar, Yousra Gaimes",title="Number of trending days in FR, US and GB")+
  xlab(NULL)+ylab(NULL)

merged_fr$isMultiple <- cut(merged_fr$trending_days, 
                             breaks = c(-Inf, 2, Inf), 
                             labels = c("one day", 'more than one day'), 
                             right = FALSE)
ggplot(merged_fr[,.N,by=isMultiple][order(-N)],aes(reorder(isMultiple,-N),N,fill=as.factor(isMultiple)))+geom_bar(stat="identity")+guides(fill="none")+labs(caption="Wiam Benhammou, Hamza Bentahar, Yousra Gaimes",title="Number of trending days in FR, US and GB")+
  xlab(NULL)+ylab(NULL)

merged_gb$isMultiple <- cut(merged_gb$trending_days, 
                             breaks = c(-Inf, 2, Inf), 
                             labels = c("one day", 'more than one day'), 
                             right = FALSE)
ggplot(merged_all[,.N,by=isMultiple][order(-N)],aes(reorder(isMultiple,-N),N,fill=as.factor(isMultiple)))+geom_bar(stat="identity")+guides(fill="none")+labs(caption="Wiam Benhammou, Hamza Bentahar, Yousra Gaimes",title="Number of trending days in FR, US and GB")+
  xlab(NULL)+ylab(NULL)
View(merged_all)
summary(data)
