
# Statistical Inference
# HW4 R homework
# Yara Mohammadi Bahram
# 810199265



######################### 3 ############################


install.packages("palmerpenguins")

library(palmerpenguins)
head(penguins)

print(names(penguins))

############### A:

penguins <- subset(penguins, select=c('species', 'flipper_length_mm'))
penguins <- na.omit(penguins)

############## B:

library(ggplot2)

ggplot(penguins, aes(x=species, y=flipper_length_mm)) +
  geom_boxplot() +
  ggtitle('Species VS. Flipper length')

############# C:

ggplot(penguins, aes(x=flipper_length_mm, fill=species)) +
  geom_histogram(alpha = 0.6, position='identity') +
  ggtitle('Histogram of Flipper Length in each species')


adelie = subset(penguins, species == 'Adelie')$flipper_length_mm
chinstrap = subset(penguins, species == 'Chinstrap')$flipper_length_mm
gentoo = subset(penguins, species == 'Gentoo')$flipper_length_mm

print("Variances are:")
print(var(subset(penguins, species == 'Adelie')$flipper_length_mm))
print(var(subset(penguins, species == 'Chinstrap')$flipper_length_mm))
print(var(subset(penguins, species == 'Gentoo')$flipper_length_mm))

############ D:

ggplot(penguins, aes(x=species, y=flipper_length_mm, fill=species)) +
  geom_dotplot(binaxis='y', stackdir='center') +
  ggtitle('Species VS. Flipper length')

############ E:

means <- aggregate(penguins$flipper_length_mm, list(penguins$species), FUN=mean)
sds <- aggregate(penguins$flipper_length_mm, list(penguins$species), FUN=sd)

stat = merge(means, sds, by='Group.1')
names(stat)[1] <- "species"
names(stat)[2] <- "mean"
names(stat)[3] <- "sd"

print(stat)

########### F:

# Sum of squqres within groups
SSE1 <- (adelie - stat[1, 2])^2
SSE2 <- (chinstrap - stat[2, 2])^2
SSE3 <- (gentoo - stat[3, 2])^2
SSE <- sum(SSE1) + sum(SSE2) + sum(SSE3)

# Total sum of squares
all <- c(adelie, chinstrap, gentoo)
SST <- sum((all - mean(all))^2)

# Sum of squares between groups
SSG <- SST - SSE

print(c(SST, SSE, SSG))


# Degrees of freedom
dfG <- 3 - 1
dfT = length(all) - 1
dfE <- dfT - dfG
print(c(dfT, dfE, dfG))

# F value
f_value <- (SSG/dfG)/(SSE/dfE)
print(f_value)

# P-value
install.packages("report")
library("report")

p_val <- report(qf(p=0.05, df1=dfG, df2=dfE, lower.tail=FALSE))
print(p_val)

# Null hypothesis is rejeected. At least one sample differs significantly from the others. But we don't know which?



######################## 9 ############################


published_papers <- c(25, 27, 35, 42, 28, 37, 40, 31, 29, 33, 30, 26, 31, 28, 30, 15)

############## B:

bootstrap <- replicate(1000, sample(published_papers, replace=T))

mean <- mean(bootstrap)
SE <- sd(bootstrap)/sqrt(length(mean))

print("SE and mean statistics of the bootstrap:")
print(SE)
print(mean)


t_stat <- (mean-35)/SE
df <- length(bootstrap) - 1

p_val <- pt(t_stat, df, lower.tail = TRUE)
print("The university will survive with this p-value!")
print(p_val)


############# C:

T_star = qt(0.95, df, lower.tail=TRUE)

print('confidence interval (0.95) is:')
print(c(mean-T_star*SE, mean+T_star*SE))




######################## 10 #############################

############# A:

df <- read.csv("D:/UNI/99-3/Statistical Inference/HW/4/car_train.csv")

data <- df$economy
print(table(is.na(df$economy)))
# Remove NA values
data <- na.omit(data)
print(summary(data))
hist(data)
print(length(data))

##### I

m <- mean(data)
SE <- sd(data)/sqrt(length(data))

print("mean and SE statistics of economy:")
print(m)
print(SE)

# This is my alternate hypothesis
new_mean <- 8.48

z_score <- (new_mean - m)/SE
print(z_score)
print(pnorm(z_score, lower.tail = FALSE))
print("p-norm is near 0! Null hypothesis is rejected")


##### II

print("Confidence Interval (95%) is:")
print(c(m-SE*qnorm(0.975), m+SE*qnorm(0.975)))

##### III

actual_mean <- 9

z_score <- (m - actual_mean)/SE
print(z_score)
print("Probability of type 2 error is: ")
print(pnorm(z_score, lower.tail = TRUE))

##### IV

actual_mean <- c(8.45, 8.47, 8.49, 8.51, 8.53, 8.55)
index <- c(1:5)

z_score <- (m - actual_mean)/SE
pwr <- 1 - pnorm(z_score, lower.tail = TRUE)

library(ggplot2)

d <- data.frame(actual_mean, pwr)
ggplot(d, aes(x=actual_mean, y=pwr)) +
         geom_line() +
          ggtitle('power curve')


##### V

sam <- sample(data, 10)
bootstrap <- replicate(1000, sample(sam, replace=T))

m <- mean(bootstrap)
SE <- sd(bootstrap)/sqrt(length(bootstrap))

print("SE and mean statistics of the bootstrap:")
print(SE)
print(m)


# This is my alternate hypothesis
new_mean <- 8.48

z_score <- (new_mean - m)/SE
print(z_score)
print(pnorm(z_score, lower.tail = FALSE))
print("p-norm is much larger than (i)! Null hypothesis is NOT rejected")

print("Confidence Interval (95%) is:")
print(c(m-SE*qnorm(0.975), m+SE*qnorm(0.975)))
print("Confidence interval is larger than (ii)")

##### VI
print("Confidence interval by Percentile: ")
quantile(bootstrap, probs = c(0.025, 0.975))

##### VII 
print("Yes, the difference is ...")



############# B:

hist(df$litres)
hist(df$location)

litres = df$litres
location = df$location

print(table(is.na(litres)))
print(table(is.na(location)))

# Remove NA values
litres <- na.omit(litres)

# Sampling
litres <- sample(litres, 25)
location <- sample(location, 25)

##### VIII

# We should use t-test. Sample size is small, we are comparing 2 means, and we don't have population Variance (We will just assume that we don't)

##### IX

mean <- mean(location-litres)
SE1 <- var(litres)^2/length(litres)
SE2 <- var(location)^2/length(location)
SE = sqrt(SE1 + SE2)

print("SE and mean statistics:")
print(SE)
print(mean)

# Hypothesis testing (Null value = 0)
t_stat <- (mean-0)/SE
print(t_stat)
df <- length(location) - 1

p_val <- pt(t_stat, df, lower.tail = FALSE)
print("P Value is: (Null hypothesis is rejected!)")
print(p_val)

print("Confidence interval:")
t_star = qt(0.95, df)
print(c(0, 0+t_star*SE))

############# c:

df <- read.csv("D:/UNI/99-3/Statistical Inference/HW/4/car_train.csv")

used_automatic = subset(subset(df, df$category == 'Used'), df$transmission =='Automatic')$price
others =         subset(subset(df, df$category != 'Used'), df$transmission !='Automatic')$price

# Remove NA values
used_automatic <- na.omit(used_automatic)
others <- na.omit(others)


print(length(used_automatic))
print(length(others))

m1 <- mean(used_automatic)
m2 <- mean(others)
m <- m1-m2

SE1 <- var(used_automatic)^2/length(used_automatic)
SE2 <- var(others)^2/length(others)
SE = sqrt(SE1 + SE2)

print("mean and SE statistics of the price difference :")
print(m)
print(SE)

z_score <- (m - 0)/SE
print(z_score)
print(pnorm(z_score, lower.tail = TRUE))
print("p-norm is big! Null hypothesis is not rejected")


# Plotting the result:

library(ggplot2)

lim1 = -10000000
lim2 = 10000000
ggplot(data.frame(x = c(lim1, lim2)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = m, sd=SE),
                geom="area", alpha=0.3,
                aes(fill='Reject H0'),
                n=1001, xlim = (c(lim1, 0-2*SE))) +
  stat_function(fun = dnorm, args = list(mean = m, sd=SE),
                geom="area", alpha=0.3,
                aes(fill='Accept H0'),
                n=1001, xlim = (c(0-2*SE, 0+2*SE))) +
  stat_function(fun = dnorm, args = list(mean = m, sd=SE),
                geom="area", alpha=0.3,
                aes(fill='Reject H02'),
                n=1001, xlim = (c(0+2*SE, lim2))) +
  scale_fill_manual(values = c('green', 'red', 'red')) +
  geom_vline(xintercept = m, linetype="dashed", 
             color = "red", size=2)
