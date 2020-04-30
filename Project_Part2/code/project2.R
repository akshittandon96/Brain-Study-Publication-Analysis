setwd("C:\\Users\\Nirupam\\Desktop\\stats")
library(dplyr)
library(plyr)
library(data.table)

brainAuthor <- read.csv("brain_author.csv")
brainAuthor$scopus_id <- as.factor(brainAuthor$scopus_id)
brainPubAuthors <- read.csv("brain_publication_authors.csv")
brainPubAuthors$scopus_id <- as.factor(brainPubAuthors$scopus_id)
brainPubAuthors$co_author_scopus_id <- as.factor(brainPubAuthors$co_author_scopus_id)
#brainPubDetails <- read.csv("brain_publication_details.csv")
cipCatFile <- read.csv("CIP_category.csv")
colnames(cipCatFile) <- c("cip_title", "cip_category")

nodeListInit <- distinct(inner_join(brainAuthor,cipCatFile,by="cip_title")) %>% filter(min_pub_year > 1960) %>% filter(num_publications > 10) %>% select(scopus_id,region,min_pub_year,cip_category,total_deflated_dollar_2010)

#USA STUFF START
usaNode <- unique(nodeListInit %>% filter(region == "US/Canada")) 
setDT(usaNode,keep.rownames = TRUE)[]
names(usaNode)[names(usaNode) == 'rn'] <- 'Source'
write.csv(usaNode,"usanodelist2.csv",row.names = FALSE)

edgeListInit <- inner_join(usaNode,brainPubAuthors,by="scopus_id") %>% filter(co_author_scopus_id %in% usaNode$scopus_id) %>% filter(scopus_id %in% usaNode$scopus_id) %>% filter(scopus_id != co_author_scopus_id)
edgeListInit <- edgeListInit %>% select(scopus_id,region,co_author_scopus_id,Source)
edgeListInit <- unique(edgeListInit)

names(usaNode)[names(usaNode) == 'scopus_id'] <- 'co_author_scopus_id'
edgeListInit <- inner_join(usaNode,edgeListInit,by="co_author_scopus_id")

names(edgeListInit)[names(edgeListInit) == 'Source.y'] <- 'Target'
names(edgeListInit)[names(edgeListInit) == 'Source.x'] <- 'Source'

usaEdgeList <- edgeListInit  %>% select(Source,Target) 
write.csv(usaEdgeList,"usaedgelist2.csv",row.names = FALSE)
#USA STUFF END

#europe start
europeNode <- nodeListInit %>% filter(region == "Europe") 
setDT(europeNode,keep.rownames = TRUE)[]
names(europeNode)[names(europeNode) == 'rn'] <- 'Source'
write.csv(europeNode,"europeNodelist.csv",row.names = FALSE)

europeEdgeList <- inner_join(europeNode,brainPubAuthors,by="scopus_id") %>% filter(co_author_scopus_id %in% europeNode$scopus_id) %>% filter(scopus_id %in% europeNode$scopus_id) %>% filter(scopus_id != co_author_scopus_id)
europeEdgeList <- europeEdgeList %>% select(scopus_id,region,co_author_scopus_id,Source)
europeEdgeList <- unique(europeEdgeList)

names(europeNode)[names(europeNode) == 'scopus_id'] <- 'co_author_scopus_id'
europeEdgeList <- inner_join(europeNode,europeEdgeList,by="co_author_scopus_id")

names(europeEdgeList)[names(europeEdgeList) == 'Source.y'] <- 'Target'
names(europeEdgeList)[names(europeEdgeList) == 'Source.x'] <- 'Source'
europeEdgeList <- europeEdgeList %>% select(Source,Target)
write.csv(europeEdgeList,"europeEdgeList.csv",row.names = F)
#europe end

#autralasia start
australasiaNodeList <- nodeListInit %>% filter(region == "Australasia")
setDT(australasiaNodeList,keep.rownames = TRUE)[]
names(australasiaNodeList)[names(australasiaNodeList) == 'rn'] <- 'Source'
write.csv(australasiaNodeList,"australasiaNodelist.csv",row.names = FALSE)

australasiaEdgeList <- inner_join(australasiaNodeList,brainPubAuthors,by="scopus_id") %>% filter(co_author_scopus_id %in% australasiaNodeList$scopus_id) %>% filter(scopus_id %in% australasiaNodeList$scopus_id) %>% filter(scopus_id != co_author_scopus_id)
australasiaEdgeList <- australasiaEdgeList %>% select(scopus_id,region,co_author_scopus_id,Source)
australasiaEdgeList <- unique(australasiaEdgeList)

names(australasiaNodeList)[names(australasiaNodeList) == 'scopus_id'] <- 'co_author_scopus_id'
australasiaEdgeList <- inner_join(australasiaNodeList,australasiaEdgeList,by="co_author_scopus_id")

names(australasiaEdgeList)[names(australasiaEdgeList) == 'Source.y'] <- 'Target'
names(australasiaEdgeList)[names(australasiaEdgeList) == 'Source.x'] <- 'Source'
australasiaEdgeList <- australasiaEdgeList %>% select(Source,Target)
write.csv(australasiaEdgeList,"australasiaEdgeList.csv",row.names = F)

#australasia end
