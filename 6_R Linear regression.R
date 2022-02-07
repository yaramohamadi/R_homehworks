
# Statistical Inference HW6 R codes
# Yara Mohammadi
# 810199265

# _________________ 4 __________________

# _________________ A

library(MASS)

df <- quine

df$Eth <- as.integer(df$Et == 'N') # 1 if not aboriginal 0 if aboriginal
df$Sex <- as.integer(df$Sex == 'M') # 1 If Male 0 if Female
df$Lrn <- as.integer(df$Lrn == 'SL') # If Slow learner 0 if Average learner

# ________________ B

day_mlr <- lm(Days ~ Eth + Sex + Lrn, data=df)
summary(day_mlr)

intercept <- day_mlr$coefficients[1]
eth_coef <- day_mlr$coefficients[2]
sex_coef <- day_mlr$coefficients[3]
lrn_coef <- day_mlr$coefficients[4]

# ________________ C

predict_line <- function(eth_val, sex_val, lrn_val){
  y_hat <- intercept + eth_coef*eth_val + sex_coef*sex_val + lrn_coef*lrn_val
  print(y_hat)
}

# ________________ D
  
# R2 is seen in the summary function. explanations are in the PDF
  
# ________________ E
  
library(ggplot2)
  
df$residuals <- day_mlr$residuals

ggplot(df, aes(x=Days, y=residuals)) + geom_point()
  

# As days increase, unexplained variance also increases. 
# The residuals are not uniformly distributed and their variability is not constant.
# So one of the conditions for Linear Regression is not met.


# _______________ 8 _________________

# _______________ A

df <- state.x77

df <- data.frame(df)

head(df)
typeof(df)


# Step 1
mlr <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=df)
summ <- summary(mlr)

elimination <- function(){
  print("Maximum p-value:")
  print(which.max(summ$coefficients[,4]))
  print("Value:")
  print(max(summ$coefficients[,4]))
  print("Greater than 0.05?")
  print(max(summ$coefficients[,4]) > 0.05)
}

elimination()

# Step 2 (Remove Area, repeat)
mlr <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost, data=df)
summ <- summary(mlr)

elimination()


# Step 3 (Remove Illiteracy, repeat)
mlr <- lm(Life.Exp ~ Population + Income + Murder + HS.Grad + Frost, data=df)
summ <- summary(mlr)

elimination()


# Step 4 (Remove Income, repeat)
mlr <- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data=df)
summ <- summary(mlr)

elimination()


# Step 5 (Remove Population, repeat)
mlr <- lm(Life.Exp ~ Murder + HS.Grad + Frost, data=df)
summ <- summary(mlr)

elimination()

# This is good, all predictors are significant

# Summary
summ


# _______________ B

mlr <- lm(Life.Exp ~ Murder, data=df)
summ <- summary(mlr)
summ

# ______________ C

library(ggplot2)

df$residuals <- mlr$residuals
ggplot(df, aes(x=residuals)) + geom_histogram(binwidth = 0.2)

sd_ <- summ$coefficients[,2][2]
sd_
# ______________ D

set.seed(10)

qqnorm(df$residuals, ylim=c(-0.5, 0.5), xlim=c(-1, 1))
qqline(df$residuals)

