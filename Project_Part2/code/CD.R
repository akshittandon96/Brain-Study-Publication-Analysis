setwd("C:\\Users\\Nirupam\\Desktop\\stats")
library(dplyr)
library(plyr)
library(data.table)
library("readr")

brainAuthor <- read_csv("brain_author.csv")
brainPubAuthors <- read_csv("brain_publication_authors.csv")
brainPubDetails <- read_csv("brain_publication_details.csv")
cipCatFile <- read_csv("CIP_category.csv")
colnames(cipCatFile) <- c("cip_title", "cip_category")


selectedPubAuthors <- brainPubAuthors %>% select(scopus_id,co_author_scopus_id,eids)
write.csv(selectedPubAuthors,"selectedPubAuthors.csv")

selectedPubDetails <- brainPubDetails %>% select(eids,pub_year)
write.csv(selectedPubDetails,"selectedPubDetails.csv")
#creating year wise brain pub details 
pubdeatails2014 <- brainPubDetails %>% filter(pub_year >= 2014) %>% filter(pub_year <= 2018) %>% select(pub_year,eids)



#node list main
nodeList <- inner_join(brainAuthor,cipCatFile,by="cip_title")
nodeList <- distinct(nodeList %>% select(scopus_id,cip_category,region,min_pub_year))
setDT(nodeList,keep.rownames = TRUE)[]
names(nodeList)[names(nodeList) == 'rn'] <- 'Source'

#finalNodeList <- nodeList %>% select(Source,cip_category)
#node list main end 




#edge list main 


mainFrame <- left_join(brainPubAuthors,brainPubDetails, by="eids") %>% select(pub_year,scopus_id,co_author_scopus_id)
mainFrame <- unique(mainFrame)





#edgelist main end







# 
# joinedFrame <- inner_join(brainAuthor, brainPubAuthors, by="scopus_id")
# joinedFrameWithCategory <- inner_join(joinedFrame, cipCatFile, by="cip_title")
# joinedFramewithYear <- inner_join(joinedFrameWithCategory,yearFile,by="e_id")
# 
# onlyrequiredfields <- joinedFrameWithCategory %>% filter(num_publications >10) %>% filter(min_pub_year >1998) select(cip_category, region, scopus_id, min_pub_year, co_author_scopus_id)
# write.csv(onlyrequiredfields, "mainFile.csv")
# 
# 
# bigjoinfile <- read.csv("mainFile.csv")
# 
# #creating the ID dataframe
# idFrame <- unique(brainAuthor %>% select(scopus_id))
# setDT(idFrame,keep.rownames = TRUE)[]
# colnames(idFrame) <- c("id","scopus_id")
# 
# #attaching ids to author, co_author
# mappedValues <- inner_join(bigjoinfile,idFrame, by="scopus_id")
# names(mappedValues)[names(mappedValues) == 'id'] <- 'Source'
# colnames(idFrame) <- c("id","co_author_scopus_id")
# mappedValues <- inner_join(mappedValues,idFrame, by="co_author_scopus_id")
# names(mappedValues)[names(mappedValues) == 'id'] <- 'Target'
# mappedValues <- mappedValues %>% filter(Source != Target)
# head(mappedValues)
# 
# 
# 
# #us/canada test case
# edgeList <- mappedValues %>% filter(region=="US/Canada" & min_pub_year >= 2014) %>% select(Source,Target)
# write.csv(edgeList,"usa2014edge.csv",row.names=FALSE)
# nodeList <- unique(mappedValues %>%filter(region=="US/Canada" & min_pub_year >= 2014) %>% select(Source, cip_category))
# 
# authorNodeList <- mappedValues %>% filter(region=="US/Canada" & min_pub_year >= 2014) %>% select(Source,cip_category)
# coAuthorNodeList <- mappedValues %>% filter(region=="US/Canada" & min_pub_year >= 2014) %>% select(Target,cip_category)
# names(coAuthorNodeList)[names(coAuthorNodeList) == 'Target'] <- 'Source'
# 
# 
# nodeList2 <- rbind(authorNodeList, coAuthorNodeList)
# nodeList2 <- unique(nodeList2)
# write.csv(nodeList2,"usa2014node.csv",row.names=FALSE)
# 






















