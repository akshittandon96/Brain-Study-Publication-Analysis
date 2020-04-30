setwd("D:/Statistical Method in Research/Datasets")
library(FSA)
library(dplyr)
library(tidyr)
library(lme4)
library(nlme)

#Q1
data <- matrix(c(22,23,24,31),ncol = 2,byrow = TRUE)
colnames(data) <- c("Single","Multiple")
rownames(data) <- c("Yes","No")
prob = c(0.46,0.54)
chisq.test(data,p = prob,correct = F)
#Q1 Ends

#Q2
job_data = read.table("Q2.txt",header = TRUE)
gender_balance <- job_data %>% gather(Gender,Count,Males,Females)
gender_balance$Job <- as.factor(gender_balance$Job)
gender_balance$Gender <- as.factor(gender_balance$Gender)
model = glm(Count ~  Job * Gender,family = poisson,data=gender_balance)
summary(model)

#Q2 Ends

#Q3
#Part 1
method_1 = c(94,87,90,74,86,97)
method_2 = c(82,85,79,84,61,72,80)
method_3 = c(89,68,72,76,69)
method_data = list(m1=method_1,m2=method_2,m3=method_3)
kruskal.test(method_data)
                                  #alternate way
students_scores = read.table("Methods.txt",header = TRUE)
kruskal.test(Score~Method,data=students_scores)
#Part 1 ends

#Part 2
comp_data = mutate(students_scores,
         Method = factor(Method, levels=unique(Method)))
comp_data
comp = dunnTest(Score ~ Method, data = comp_data,method = "bh")
comp = comp$res
comp
#Part 2 ends
#Q3 ends


#Q4
#part 1
control = c(66.1,79.3,55.3,68.8,57.8,71.8,81.3,54)
treated = c(59.1,58.9,55,65.9,54.1,69,60.2,55.5)
n = 8
diff <- c(treated - control)
diff <- diff[diff != 0]
diff.rank <- rank(abs(diff))
diff.rank.sign <- diff.rank * sign(diff)
ranks.pos <- sum(diff.rank.sign[diff.rank.sign > 0])
ranks.neg <- sum(diff.rank.sign[diff.rank.sign < 0])
ranks.pos
ranks.neg
qsignrank(0.05,n)
#Part 1 ends

#Part 2
t.test(treated,control, paired = TRUE)
#Part 2 ends
#Q4 Ends


#Q5

#LINEAR MIXED MODEL
yield_data <- read.csv("Q5_HW4.csv",stringsAsFactors = FALSE)
names(yield_data)[names(yield_data) == 'ï..Yr'] <- 'Yr'
yield_data$Var <- as.factor(yield_data$Var)
yield_data$Nit <- as.factor(yield_data$Nit)

#Part 1
interaction.plot(yield_data$Nit,yield_data$Var,yield_data$Yield,ylab = "Yield", 
                 xlab = "Nit",legend = T, type = "b")
#Part 1 ends

#Part 2 
yield_data.model <- lme(Yield ~ Var*Nit, random = ~ 1|Rep,data = yield_data)
summary(yield_data.model)
#Part 2 ends

#Part 3 
yield_data.model <- lme(Yield ~ Var*Nit, random = ~ 1|Rep,data = yield_data)
summary(yield_data.model)
#Part 3 ends
