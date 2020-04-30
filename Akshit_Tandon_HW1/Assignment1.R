library(pwr)

# Q1 start
#a start
myData <- c(24,46,57,57,64,65,82,89,90,90,111,117,128,
            143,148,152,166,171,186,191,197,209,223,
            230,247,249,254,258,264,269,270,273,284, 
            294,304,304,332,341,393,395,487,510,516,
            518,518,534,608,642,697,955,1160)

myMean <- mean(myData)
myMean
myMedian <- median(myData)
myMedian
boxplot(myData, outline = TRUE,horizontal = TRUE, xlab =
        "Times in days from remission induction to relapse")
boxplot.stats(myData)
#a end
plot(myData, line(x~y))

#b start
totalValue <- length(myData)
totalValue
counter <- 0
for(item in myData)
{
  if(item < 365)
  {
    counter = counter + 1
  }
}

counter
totalPercentage = (counter/totalValue) *100
totalPercentage
#b end
#q1 end

#Q2 start
#b start
p1 <- 1/150000
p2<- 5/150000
p3<- 25/150000
p4 <- 1000/150000

expected_value = p1*50000 + p2*10000 + p3*1000 + p4*10
expected_value
#b end

#d start
vec <- c(50000,rep(10000, each = 5),rep(1000,each = 25),rep(10,each = 1000),
         rep(0,each = 150000-1031))
vec
sd(vec)


# d end

# q2 end

#Q3 start
#b start
p_hat = 12/68
p <- 0.10
p
n <- 68
n
SD <- sqrt(p*(1-p)/n)
SD
z_score = (p_hat - p)/SD
ans_a = pnorm(z_score,lower.tail = FALSE)
ans_a
# b end

# d start
z_score_1 = (0.15 - 0.10)/SD
z_score_1
z_score_2 = (0.05-0.10)/SD
z_score_2
ans_d = pnorm(z_score_1,lower.tail = FALSE) + 
        pnorm(z_score_2)
ans_d
# d end
# q3 end

#Q4 start
#a start
pop_mean = 14200
sample_mean = 15300
pop_sd = 2600
givenAlpha = 0.05
mySampleSize = 75
se <- pop_sd/sqrt(mySampleSize)
t_value_stat = (sample_mean - pop_mean)/se
t_value_stat
alpha_range = qt(1-givenAlpha/2,df = mySampleSize-1)
critical_range = c(-alpha_range,alpha_range)
critical_range
#Now since, t value is not in critical range
#we reject the NULL hypothesis.
# a end

# b start
val = qt(0.99,df = 74)
val
marginError <- val * se
marginError
confidenceInterval = c(sample_mean - marginError , sample_mean + marginError)
confidenceInterval
# b end

# c start
power_curve <- function(n = 75,alpha = 0.05,H0 = 14200,H1 = 15300,
                        sig = 2600)
{
  zee <- qnorm(p = alpha,mean = 0,sd = 1,lower.tail = FALSE)
  cr <- zee * (sig/sqrt(n)) + H0;
  power <- pnorm(q = cr,mean = H1,sd = sig/sqrt(n),lower.tail = FALSE)
  power
}
power_curve()
mycurve1 <- lapply(X = 1:100, FUN = power_curve,alpha = .05
                   ,H0 = 14200,H1 = 15300,sig = 2600)
plot(x = 1:100,y = mycurve1,type = "line",xlab = "Sample Size (n)", ylim = c(0,1),
     ylab = "Power")
# c end
#q4 end


#q5 start
#a start
given_pop_mean = 2700
given_sample_mean = 2620
given_pop_sd = 450
alpha = 0.05
size = 36
standard_e = given_pop_sd/sqrt(size)
standard_e
t_value = (given_sample_mean - given_pop_mean)/standard_e
t_value
d_range = qt(1-alpha/2,df = size-1)
f_range = c(-d_range,d_range)
f_range
# a end

#b start
lower_bound = qnorm(alpha/2,mean = 2600,sd = standard_e)
lower_bound
upper_bound = qnorm(alpha/2,mean = 2600,sd = standard_e, lower.tail = FALSE)
upper_bound
given_mean = 2700
z_lower = (lower_bound - given_mean)/standard_e
z_lower
z_upper = (upper_bound - given_mean)/standard_e
z_upper
p_lower = pnorm(z_lower)
p_lower
p_upper = pnorm(z_upper,lower.tail = FALSE)
p_upper
power = p_lower + p_upper
power
#b end
#q5 end



# q5(b) Alternate Method 1
n = 36
SE = 450/6
alpha = 0.05
mu0 = 2600
I = c(alpha/2 , 1-alpha/2)
q = qnorm(I, mean = mu0,sd = SE)
q
mu = 2700
p = pnorm(q,mean = mu,sd = SE)
p
1 - diff(p)

# q5(b) Alternate Method 2
library(pwr)
pwr.t.test(d = (2600-2700)/450, n = 36,sig.level= 0.05,type = "one.sample",alternative = "two.sided")




#q4(c) Alternative method.
diff_n <- seq(0,100,10)
diff_power <- power.t.test(n = diff_n,delta = 14200-15300, sd =2600,
                           sig.level = 0.05,power = NULL,type = "one.sample",
                           alternative = "two.sided")

plot(diff_n,diff_power$power,type = "line",xlab = "Sizes",
     ylab = "Power")    
print(diff_power$power)                    


