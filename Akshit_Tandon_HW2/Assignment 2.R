#Q1
data_q1 <- read.csv("Q1.csv")
given_sd = 0.1 #0.2/2(normal distribution)
given_variance = given_sd^2
given_variance
sample_var <- var(data_q1$ï..gain_loss)
df = 14
SS <- sample_var*df
X_2 = SS/given_variance
X_2
chi_sq = qchisq(0.95,df)
chi_sq
#Q1 end


#Q2 
teaching <- read.csv("teachers.csv")
t.test(teaching$New,teaching$Standard,paired = F,
       alternative = "greater")
#Q2 end


#Q3
#a
p1 = 74/150
p2 = 73/91
x1 = 74
x2 = 73
n1 = 150
n2 = 91
alpha = 0.05
p_hat = (x1+x2)/(n1+n2)
z_val = (p1-p2)/sqrt(p_hat*(1-p_hat)*(1/n1+1/n2))
z_val
pnorm(z_val)
#a end

#b
s1_age_mean = 5.73
s1_size = 150
s2_age_mean = 9.02
s2_size = 91
SE <- sqrt((6.15^2/s1_size)+(6.10^2/s2_size))
t <- (5.73-9.02)/SE
t
df = s1_size + s2_size - 2
pt(t,df)

#b end
#Q3 ends


#Q4
Untreated <- c(5,1,1.8,1,3.6,5,2.6,1)
treated <- c(5,5,1.2,4.8,5,5,4.4,2)
alpha = 0.01
critical_t_val = -3.00
t.test(Untreated,treated,paired = TRUE,conf.level = 0.99,alternative = "less")
#Q4 ends


#Q5
p = 0.10
p_hat = 18/100
n = 100
SD <- sqrt(p*(1-p)/n)
SD
z_score = (p_hat - p)/SD
z_score
ans_a = pnorm(z_score)
ans_a
#Q5 ends


