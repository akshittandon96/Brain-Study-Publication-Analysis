library(ggfortify)
library(dplyr)
library(igraph)
library(wordcloud)
library(tm)
library(sqldf)
library(ggplot2)
require(data.table)
require(gridExtra)
setwd("C:\\Users\\Nirupam\\Desktop\\STATS\\Project")
#1
author_data <- read.csv("brain_author.csv")
pub_year <- author_data %>% filter( min_pub_year > 1960) %>% select(min_pub_year)

g <- ggplot( pub_year,aes(min_pub_year)) + ggtitle("PDF for First Publication year of Faculty") + labs(y = "PDF",x = "Year" ) + geom_histogram(aes(y = ..density..),binwidth = 2, colour = "black", fill = "white") + geom_density(alpha=.2, fill="#FF6666")
g

#2
total_citations <- author_data %>% filter(min_pub_year > 1960 && citations != 0) %>% select(citations)
transform <- log(1+total_citations)
g <- ggplot(transform,aes(citations)) + ggtitle("PDF for Total Citations of Faculty") + labs(y = "PDF",x = "Total Citations" )+ geom_histogram(aes(y = ..density..), binwidth = 0.5, colour = "black", fill = "white") + geom_density(alpha=.2, fill="#FF6666")
g
#3
publication_cat <- read.csv("brain_publication_areas.csv")
publication_cat <- unique(publication_cat)

#forming the edge list for the graph
combs <- inner_join(publication_cat,publication_cat, by="eid")
combs_alt <- combs %>% filter(!area.x == area.y)

#creatnig a graph from the edge list
g <- graph_from_data_frame(d = combs %>% select(area.x,area.y), directed = FALSE)

#clustering
clusters <- cluster_louvain(g)

area_to_cluster_map <- cbind(V(g)$name,clusters$membership)
colnames(area_to_cluster_map) <- c("area", "cluster")
area_to_cluster_map <- as.data.frame(area_to_cluster_map)
allclusters <- area_to_cluster_map
area_to_freq_map <- as.data.frame(table(publication_cat$area))
colnames(area_to_freq_map) <- c("area", "freq")

# selecting top 5 clusters 
y <- clusterdata %>% group_by(cluster) %>% add_tally(sort=TRUE) %>% select(cluster,n)
y <- unique(y)
top5 <- c(7,3,1,2,5)

merged_areas <- merge(area_to_freq_map, area_to_cluster_map, by = "area")
merged_areas <- merged_areas %>% filter(merged_areas$cluster %in% top5)
write.csv(merged_areas, "final_clusters_w_freq.csv")

# to make a word cloud -->
# you need to change these 3 variables for every cluster based on your personal choice
cluster_num = 5
min_freq = 1
max_size = 1
clrs = "black"
particular_cluster <- filter(merged_areas, cluster == cluster_num)
wordcloud(words = particular_cluster$area,
          freq = particular_cluster$freq,
          scale = c(max_size,0.5),
          min.freq = min_freq,
          max.words = Inf,
          random.order = FALSE,
          rot.per = .0,
          ordered.colors = TRUE,
          
          use.r.layout = FALSE)




#4
clusterdata <- read.csv("final_clusters_w_freq.csv")
author <- read.csv("brain_author.csv")
pub_details <- read.csv("brain_publication_details.csv")
publication_cat <- read.csv("brain_publication_areas.csv")

combined_frame_1 <- merge(author, pub_details, by="scopus_id")
combined_frame_2 <- merge(combined_frame_1, publication_cat, by.x="eids",by.y="eid")
mergedData <- merge(combined_frame_2, clusterdata, by.x = "area",by.y ="area")
clusterHead <- sqldf("select area,cluster,max(Freq)from clusterdata group by cluster")
clusterHead
finalDataset <- merge(mergedData,clusterHead,by="cluster")
#this is a 2.6 GB file 
write.csv(finalDataset,"finalset.csv")

# Final Dataset to use after all joins

finalClusterSet <- read.csv("finalset.csv")
finalClusterSet <- finalClusterSet %>% select(eids,area.y,cip_title,scopus_id,pub_year,region,)

after_2014_q4 <- filter(finalClusterSet, pub_year > 2014)
before_2014_q4 <- filter(finalClusterSet, pub_year <= 2014)


#filtering values before and after 2014 and grouping based on subject area and region 
values_after_2014 <- after_2014_q4 %>% group_by(after_2014_q4$area.y, region) %>% filter(!is.na(region))  %>% add_tally()

values_before_2014 <- before_2014_q4 %>% group_by(before_2014_q4$area.y, region) %>% filter(!is.na(region)) %>% add_tally()


#plotting after 2014
g1 <- ggplot(values_after_2014, aes(x = region, y = n, fill =values_after_2014$area.y ))+
   geom_bar(stat = "identity",position=position_dodge())+ coord_flip() + ggtitle("Number of publications per subject area per continent post-2014") +
   labs(x = "region", y= "publications", fill="Subject Area")

#plotting beofre 2014
g2 <- ggplot(values_before_2014, aes(x = region, y = n, fill =values_before_2014$area.y ))+
   geom_bar(stat = "identity",position=position_dodge())+ coord_flip() + ggtitle("Number of publications per subject area per continent pre-2014") +
   labs(x = "region", y= "publications", fill="Subject Area")


grid.arrange(g1,g2)
comb_data <- merge(values_after_2014,values_before_2014, by.x = "region", by.y = "area.y")
comb_data$n <- comb_data$n.x - comb_data$n.y

ggplot() + ggtitle("Number of publications per subject area per continent pre-2014") +

#5
author_data <- read.csv("brain_author.csv")
pub_details <- read.csv("brain_publication_details.csv")

combined_frame <- merge(author_data, pub_details, by="scopus_id")
freq_cip <- as.data.frame(table(combined_frame$cip_title))
colnames(freq_cip) <- c("Cip_Title","Freq")
freq_cip_top6 <- sqldf("select * from freq_cip order by Freq Desc limit 6")
dd <- merge(combined_frame,freq_cip_top6 , by.x = "cip_title",by.y = "Cip_Title")
new_dd <- sqldf("select * from dd where region <> 'NA' ")

after_2014 <- filter(new_dd, pub_year > 2014) %>% filter(!is.na(region)) %>% filter(!is.na(cip_title))
grouped_after_2014 <- after_2014%>% group_by(after_2014$region, after_2014$cip_title) %>% add_tally()

before_2014 <-  filter(new_dd, pub_year < 2014) %>% filter(!is.na(region)) %>% filter(!is.na(cip_title))
grouped_before_2014 <- before_2014%>% group_by(before_2014$region, before_2014$cip_title) %>% add_tally()

comb_data <- merge(grouped_after_2014,grouped_before_2014, by.x="cip_title" , by.y="region")


# plot for after 2014 
g1 <- ggplot(grouped_after_2014, aes(x = region, y = n, fill = cip_title))+
  geom_bar(stat = "identity",position=position_dodge())+ coord_flip() +
    ggtitle("Number of publications per CIP per continent post-2014") + labs(x = "Region", y = "Publications", fill ="CIP")

#plot for before 2014
g2 <- ggplot(grouped_before_2014, aes(x = region, y = n, fill = cip_title))+
   geom_bar(stat = "identity",position=position_dodge())+ coord_flip() +
   ggtitle("Number of publications per CIP per continent pre-2014")  + labs(x = "Region", y = "Publications", fill ="CIP")

grid.arrange(g1,g2)

