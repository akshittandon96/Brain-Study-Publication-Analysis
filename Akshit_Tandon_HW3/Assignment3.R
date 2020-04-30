setwd("C:\\Users\\akshi\\OneDrive\\Documents\\R Files")
library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)
library(readr)
library(sqldf)
library(VCA)
library(car)

                                              #Question1

#Part a
tensileData <- read.table("AirCondition.txt", header=TRUE)
data <- tensileData %>%gather(Sample,Strength,X1:X4)
data
factorData <- factor(data$Sample)
analysis <- aov(data$Strength ~ factorData)
summary(analysis)

boxplot(data$Strength~factorData,xlab="Variety",
        ylab="Strength",
        main="Boxplots of different tensile strength metals")
#Part a ends

#Part b
data$Sample <- as.factor(data$Sample)
anova_vca <- fitVCA(Strength ~ Sample, data,"anova")
anova_vca
#Part b ends


                                                #Question2
#Part a
organism_data <- read.csv("OrganismData.csv",header = TRUE)
factors <- factor(organism_data$Medium)
anova_analysis <- aov(organism_data$Diameters ~ factors)
summary(anova_analysis)

boxplot(organism_data$Diameters~factors,xlab="Medium",
        ylab="Diameters",
        main="Boxplots of different medium diameters")
#Part a ends

#Part b 
posthoc <- TukeyHSD(x=anova_analysis, "factors", conf.level=0.95)
posthoc
#Part b ends

                                                #Question 3

#Part a
r_squared <- 0.18
beta_1 <- -0.62
n = 178
f = ((n-2)*r_squared)/(1-r_squared)
f
pf(f,1,n-2,lower.tail = F)
#Part a ends

#part b

#Part b ends


                                            #Question4

#Part a
myData <- read.csv("q4.csv",header = TRUE)
colnames(myData) <- c("Y","X")
model <- lm(Y ~ X ,data = myData)
summary(model)
#Part a ends

#Part b
myData$X <- as.factor(myData$X)
model_1 <- lm(Y ~ X, data = myData)
summary(model_1)
#Part b ends


                                            #Question 5

# Part a
healthData <- read.table("Health.txt",header = TRUE)
multiModel <- lm(DEATHS ~ VALUE + DOCT+NURSE+VN, data = healthData)
summary(multiModel)
x_cor = cor(healthData[,c(2,3,4,5)])
x_cor = round(x_cor,2)
x_cor
vif(multiModel)

plot(healthData[,c(2:6)],pch=10,col="blue",main = "Scatter Plot")
#Part a ends

#Part c
perCapitaHealthData <- sqldf("select cast (DEATHS as real)/POP as DEATHS_PER_CAPITA,
                              cast(VALUE as real)/POP as VALUE_PER_CAPITA,
                              cast(NURSE as real)/POP as NURSE_PER_CAPITA,
                              cast(VN as real)/POP as VN_PER_CAPITA,
                              cast(DOCT as real)/POP as DOCT_PER_CAPITA 
                              from healthData") 
perCapitaMultiModel <- lm(DEATHS_PER_CAPITA ~ VALUE_PER_CAPITA + DOCT_PER_CAPITA + 
                            NURSE_PER_CAPITA + VN_PER_CAPITA, data = perCapitaHealthData)
summary(perCapitaMultiModel)

vif(perCapitaMultiModel)
#Part c ends


# *************************************************END***********************************************

