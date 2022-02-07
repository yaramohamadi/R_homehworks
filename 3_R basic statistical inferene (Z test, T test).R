
# Statistical Inference HW3
# Yara Mohammadi - 880199265
# yara.mohamadi@gmail.com




######################################################################
########################### 2-C (R) ##################################

# I have assumed that we want the probability of seeing above 38.2 degrees less than 1%
# We will find the least sample size that is needed for this.

z = qnorm(0.99)

print(z)

observation = 38.2
mean = 37.1
s = 0.5

n = (z*s)^2/(observation - mean)^2

# Round up N (Since for more datas the probability of being above 38.2 decreases)
print(ceiling(n))





#####################################################################
############################# 8 - A (R) #############################
#####################################################################

######################## Loading the data ###########################

xray <- read.csv(file = "D:/UNI/99-3/Statistical Inference/HW/3/metadata.csv")
head(xray)

####################### EXPLORING THE DATA ##########################

#### Column names
names(xray)

#### Column Na values and statistics
summary(xray)

#### Column types
str(xray)


# choose went_icu for Y. Factors are:  "", "N", "Y"
# Choose age for X -> Contains NA values (372 samples, 54 NA values )
# Remove rows with NA Values in Age 

cc=is.na(xray$age)
m=which(cc==c("TRUE"))
xray=xray[-m,]

#### BarPlot of y
library(ggplot2)
ggplot(xray, aes(x = went_icu)) +
  geom_bar()

### Boxplot of X and Y
ggplot(xray, aes(x=went_icu, y=age)) + geom_boxplot() 

sample_NA = xray$age[which(xray$went_icu=="")]
sample_n = xray$age[which(xray$went_icu=="N")]
sample_y = xray$age[which(xray$went_icu=="Y")]


############## A: Confidence Interval calculation #################

#### Get sample size
sample_NA.n = length(sample_NA)
sample_n.n  = length(sample_n)
sample_y.n  = length(sample_y)

print(c(sample_NA.n, sample_n.n, sample_y.n))
# 337 and 3 and 32. We can draw a distribution for the first and the third! They follow central limit theorem

#### Get mean and standard error
sample_NA.mean = mean(sample_NA)
sample_y.mean  = mean(sample_y)

print(c(sample_NA.mean, sample_y.mean))

sample_NA.sd = sd(sample_NA)
sample_y.sd  = sd(sample_y)
sample_NA.se = sample_NA.sd/sqrt(sample_NA.n)
sample_y.se  = sample_y.sd/sqrt(sample_y.n)

print(c(sample_NA.se, sample_y.se))

#### Get Z_star for 2 sided 95% Confidence interval
z = qnorm(0.975)

print(c(z))

#### Calculate Error margin
sample_NA.margin = sample_NA.z * sample_NA.se
sample_y.margin = sample_y.z * sample_y.se

#### Print confidence Interval
print(c(sample_NA.mean-sample_NA.margin, sample_NA.mean+sample_NA.margin))
print(c(sample_y.mean-sample_y.margin, sample_y.mean+sample_y.margin))


############### Is the means difference Statistically significant? 


mean_test = t.test(sample_NA, sample_y, alternative = "two.sided", var.equal = FALSE)
print(mean_test)



#####################################################################
############################# 8 - B (R) #############################
#####################################################################

######################## Loading the data ###########################

xray <- read.csv(file = "D:/UNI/99-3/Statistical Inference/HW/3/metadata.csv")
head(xray)

# Choose Sex for X: Binomial distribution"
# Choose modality for Y: 2 levels "CT" and "X-ray"

# There is no Na in X. no need for cleaning
table(xray$sex)

# Remove rows with NA Values in Age 
cc=xray$sex == c("")
m=which(cc==c("TRUE"))
xray=xray[-m,]
# Drop NA Level
xray$sex <- droplevels(xray$sex)
table(xray$sex)


#### BarPlot of y
library(ggplot2)
ggplot(xray, aes(x = modality)) +
  geom_bar(aes(fill=sex))

### Boxplot of X and Y
ggplot(xray, aes(x=modality, y=sex)) + geom_boxplot() 


# Change Modality to binary
#levels(xray$modality)
#levels(xray$modality) <- c(0, 1) # 0 for CT, 1 for X-Ray
#table(xray$modality)


################ B: Confidence Interval Calculation #################


#### Checking CLT Conditions::::

# Number of Males who did CT
sample_CT_M_count = length(xray$modality[which((xray$modality==c("CT")) & (xray$sex==c("M")))])
# Number of Males and Females who did CT
sample_CT_total_count = length(xray$modality[which((xray$modality==c("CT")))])
n_CT = sample_CT_total_count

p_CT_M = sample_CT_M_count / n_CT

print("Checking CLT conditions for CT:")
print("PN:")
print(p_CT_M*n_CT)
print("(1-P)N:")
print((1-p_CT_M)*n_CT)
print("And we assume that samples are random")


# Number of Males who did X-ray
sample_xray_M_count = length(xray$modality[which((xray$modality==c("X-ray")) & (xray$sex==c("M")))])
# Number of Males and Females who did X-ray
sample_xray_total_count = length(xray$modality[which((xray$modality==c("X-ray")))])
n_xray = sample_xray_total_count

p_xray_M = sample_xray_M_count / n_xray

print("Checking CLT conditions for X-ray:")
print("PN:")
print(p_xray_M*n_xray)
print("(1-P)N:")
print((1-p_xray_M)*n_xray)
print("And we assume that samples are random")


### Get sample statistics:

print(c(n_CT, n_xray))
print(c(p_CT_M, p_xray_M))

# Binomial distribution:  N( p, SE = p(1-p)/sqrt(n) )
SE_CT = ( p_CT_M*(1-p_CT_M) )/sqrt(n_CT)
SE_xray = ( p_xray_M*(1-p_xray_M) )/sqrt(n_xray)

print(c(SE_CT, SE_xray))


#### Get Z_star for 2 sided 98% Confidence interval
z = qnorm(0.99)

print(c(z))


#### Calculate Error margin
margin_CT = z * SE_CT
margin_xray = z * SE_xray

#### Print confidence Interval
print(c(p_CT_M-margin_CT, p_CT_M+margin_CT))
print(c(p_xray_M-margin_xray, p_xray_M+margin_xray))


############### Is the proportion difference Statistically significant? 

prop_diff = table(xray$modality[which(xray$sex==c("M"))]) 
print(prop_diff)

print(prop.test(prop_diff))







######################################################################
############################## 9 (R) #################################

################# B: ####################

n = 50 # Weekends

s = 0.7
se = s/sqrt(n)

# Actual mean
mu_A = c(9, 10, 11, 12, 13)
# Sample mean
mu_0 = 11.5

effect_size <- mu_0 - mu_A


# Assuming same sample Sizes for mu_A and mu_0
#my_pwr <- pwr.t.test(n=n, d=effect_size/s, type='two.sample')

my_pwr <- power.t.test(n=n, d=(effect_size/s), alternative='two.sided')

print(my_pwr)

# Plotting the power curve
pwr_ <- my_pwr$power
df <- data.frame(mu_A, pwr_)

ggplot(df, aes(x=mu_A, y=pwr_))+
  geom_line(size=2) + 
  ggtitle("Power Curve") +
  xlab("Actual Mean") + ylab("Power")



#################### C: #######################

# We have these from question 7:
mu_0 = 11.5
mu_A = 11 # We chose this because it has the most power
se = s/sqrt(n)
z_star = 2


lim1 = 10.5
lim2 = 12

ggplot(data.frame(x = c(lim1, lim2)), aes(x)) + 
  stat_function(fun = dnorm,args = list(mean = mu_0, sd = se),   # Color for acceptance region mu_0
                geom = "area", 
                alpha = 0.3,
                aes(fill = 'Acceptance Region'),
                n = 1001, xlim = c(mu_0 - se*z_star, lim2)) + 
  stat_function(fun = dnorm,args = list(mean = mu_A, sd = se),  # Color for Power region mu_A
                geom = "area", 
                alpha = 0.3,
                aes(fill = "Power"),
                n = 1001, xlim = c(lim1, mu_0 - se*z_star)) +
  stat_function(fun = dnorm,args = list(mean = mu_A, sd = se),   # Color for type 2 error  
                geom = "area", 
                alpha = 1,
                aes(fill = 'Type2 Error'),
                n = 1001, xlim = c(mu_0 - se*z_star, lim2)) + 
  stat_function(fun = dnorm,args = list(mean = mu_0, sd = se),   # Color for type 1 error
                geom = "area", 
                alpha = 1,
                aes(fill = 'Type1 Error'),
                n = 1001, xlim = c(lim1, mu_0 - se*z_star)) +
  scale_fill_manual(values = c("green", "blue", "red", 'black') ) + 
  annotate(geom="text", x=mu_0, y=-0.1, label="Sample Mean",
           color="green") + 
  annotate(geom="text", x=mu_A, y=-0.1, label="Actual Mean",
           color="blue")




#############################################################
#############################################################
#############################################################