setwd("C:\\Users\\Nirupam\\Desktop\\stats")
library(dplyr)
library(plyr)
library(data.table)
library("GGally")
require(ggiraph)
require(ggiraphExtra)
require(plyr)
#usa model
pagerank <- read.csv("usapagerank2.csv")
cdfile <- read.csv("usa-nodes.csv")
pagerankwithcd <- inner_join(pagerank,cdfile,by="scopus_id")
new <- within(pagerankwithcd, rm(Label,timeset,cip_category.x,min_pub_year,X,num_of_fund))
new$cip_category <- as.factor(new$cip_category.y)
new <- new %>% mutate(cip_category = relevel(cip_category, ref = "Technology"))
new$y_05 <- as.factor(new$y_05)
new <- new %>% mutate(y_05 = relevel(y_05, ref = "2015"))
new <- new %>% filter(!is.na(dept_rank))
new$total_deflated_dollar_2010[is.na(new$total_deflated_dollar_2010)] = 0
usamodel <- lm(log(1 +citations) ~  factor(CD) + log(1+dept_rank) +log(1+total_deflated_dollar_2010) + log(1+pageranks)+  cip_category + y_05 ,data=new)
summary(usamodel)

graphframe <- new %>% select (total_deflated_dollar_2010, pageranks, citations, cip_category, dept_rank) %>% filter(!is.na(dept_rank))
ggpairs(graphframe,aes(col = cip_category, alpha=0.4))

#usa  strict model 
pagerank <- read.csv("usapagerank2.csv")
cdfile <- read.csv("usa_nodes_strict.csv")
cdfile <- within(cdfile,rm(total_deflated_dollar_2010))
pagerankwithcd <- inner_join(pagerank,cdfile,by="scopus_id")
new <- within(pagerankwithcd, rm(Label,timeset,cip_category.x,min_pub_year,X))
new$cip_category <- as.factor(new$cip_category.y)
new <- new %>% mutate(cip_category = relevel(cip_category, ref = "Technology"))
new$y_05 <- as.factor(new$y_05)
new <- new %>% mutate(y_05 = relevel(y_05, ref = "2015"))
new <- new %>% filter(!is.na(dept_rank))
new$total_deflated_dollar_2010[is.na(new$total_deflated_dollar_2010)] = 0
usamodel <- lm(log(1 +citations) ~  factor(Stricter_CD) + log(1+dept_rank) +log(1+total_deflated_dollar_2010) + log(1+pageranks)+  cip_category + y_05 ,data=new)
summary(usamodel)
graphframe <- new %>% select (total_deflated_dollar_2010, pageranks, citations, cip_category, dept_rank) %>% filter(!is.na(dept_rank))
ggpairs(graphframe,aes(col = cip_category, alpha=0.4))

#europe model
eupage <- read.csv("europewithPagerank.csv")
names(eupage)[names(eupage) == 'co_author_scopus_id'] <- 'scopus_id'
eucd <- read.csv("europe-nodes.csv")
eucomb <- inner_join(eupage,eucd,by="scopus_id")
eucomb <- within(eucomb, rm(Label,timeset,cip_category.x,min_pub_year,X,num_of_fund))
eucomb$total_deflated_dollar_2010[is.na(eucomb$total_deflated_dollar_2010)] = 0
eucomb$cip_category.y <- as.factor(eucomb$cip_category.y)
eucomb$y_05 <- as.factor(eucomb$y_05)
eucomb <- eucomb %>% mutate(cip_category.y = relevel(cip_category.y, ref = "Technology"))
eucomb <- eucomb %>% mutate(y_05 = relevel(y_05, ref = "2015"))
europemodel <-  lm(log(1 +citations) ~  factor(CD) +log(1+total_deflated_dollar_2010) + log(1+pageranks)+  cip_category.y + y_05 ,data=eucomb)
summary(europemodel)

graphframe <- eucomb %>% select (total_deflated_dollar_2010, pageranks, citations, cip_category.y) 
ggpairs(graphframe,aes(col = cip_category.y, alpha=0.4))
#europe stricter model
eupage <- read.csv("europewithPagerank.csv")
names(eupage)[names(eupage) == 'co_author_scopus_id'] <- 'scopus_id'
eucd <- read.csv("europe_nodes_strict.csv")
eucd <- within(eucd,rm(total_deflated_dollar_2010))
eucomb <- inner_join(eupage,eucd,by="scopus_id")
eucomb <- within(eucomb, rm(Label,timeset,cip_category.x,min_pub_year,X))
eucomb$total_deflated_dollar_2010[is.na(eucomb$total_deflated_dollar_2010)] = 0
eucomb$cip_category.y <- as.factor(eucomb$cip_category.y)
eucomb$y_05 <- as.factor(eucomb$y_05)
eucomb <- eucomb %>% mutate(cip_category.y = relevel(cip_category.y, ref = "Technology"))
eucomb <- eucomb %>% mutate(y_05 = relevel(y_05, ref = "2015"))
eucomb$total_deflated_dollar_2010[is.na(eucomb$total_deflated_dollar_2010)] = 0
europemodel <-  lm(log(1 +citations) ~  factor(Stricter_CD) +log(1+total_deflated_dollar_2010) + log(1+pageranks)+  factor(cip_category.y) + factor(y_05) ,data=eucomb)
summary(europemodel)
graphframe <- eucomb %>% select (total_deflated_dollar_2010, pageranks, citations, cip_category.y) 
ggpairs(graphframe,aes(col = cip_category.y, alpha=0.4))
#australasia model

auspage <- read.csv("australasiawithPagerank.csv")
auscd <- read.csv("austrasia-nodes.csv")
auscomb <- inner_join(auspage,auscd,by="scopus_id")
auscomb <- within(auscomb, rm(Label,timeset,cip_category.x,min_pub_year,X,num_of_fund))
auscomb$y_05 <- as.factor(auscomb$y_05)
auscomb <- auscomb %>% mutate(cip_category.y = relevel(cip_category.y, ref = "Technology"))
auscomb <- auscomb %>% mutate(y_05 = relevel(y_05, ref = "2015"))
auscomb$total_deflated_dollar_2010[is.na(auscomb$total_deflated_dollar_2010)] = 0
ausmodel <- lm(log(1 +citations) ~  factor(CD)  + log(1+pageranks)+  cip_category.y + y_05 ,data=auscomb)
summary(ausmodel)
graphframe <- auscomb %>% select  (pageranks, citations, cip_category.y) 
ggpairs(graphframe,aes(col = cip_category.y, alpha=0.4))

#australia strict model

auspage <- read.csv("australasiawithPagerank.csv")
auscd <- read.csv("austrasia_nodes_strict.csv")
auscd <- within(auscd,rm(total_deflated_dollar_2010))
auscomb <- inner_join(auspage,auscd,by="scopus_id")
auscomb <- within(auscomb, rm(Label,timeset,cip_category.x,min_pub_year,X))
auscomb$cip_category.y <- as.factor(auscomb$cip_category.y)
auscomb$y_05 <- as.factor(auscomb$y_05)
auscomb <- auscomb %>% mutate(cip_category.y = relevel(cip_category.y, ref = "Technology"))
auscomb <- auscomb %>% mutate(y_05 = relevel(y_05, ref = "2015"))
auscomb$total_deflated_dollar_2010[is.na(auscomb$total_deflated_dollar_2010)] = 0
ausmodel <- lm(log(1 +citations) ~  factor(Stricter_CD) +log(1+total_deflated_dollar_2010) + log(1+pageranks)+  factor(cip_category.y) + factor(y_05) ,data=auscomb)
summary(ausmodel)
graphframe <- auscomb %>% select  (pageranks, citations, cip_category.y) 
ggpairs(graphframe,aes(col = cip_category.y, alpha=0.4))