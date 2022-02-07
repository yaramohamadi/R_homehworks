
# Statistical Inference
# HW5 R
# Yara MohammadiBahram 810199265


########################### 6 ###########################

observed <- c(110, 210)
expected <- c(12.31, 307.69)

chi <- sum((observed-expected)^2/expected)
df <- 2-1
print(chi)

pchisq(chi, df, lower.tail=FALSE)



########################## 7 ############################

library(MASS)

# Fix chi-square condition issue

print(caith)

# Add black and dark columns
caith$dark <- as.numeric(caith$black) + as.numeric(caith$dark)
caith <- subset(caith, select = -c(black))

print(caith)


# This is a vector of total counts for each row
total_rows <- as.numeric(rowSums(caith))
# This is a vector of total counts for each column
total_cols <- as.numeric(colSums(caith))
# Table total
table_total <- sum(total_rows)

# By matrix multiplication of total_row and transposed total_col, we derive expected count:
expected_count <- (total_rows %*% t(total_cols))/table_total

print("expected count")
print(expected_count)

print("observed count")
print(caith)

chi <- sum((caith - expected_count)^2/expected_count)

df <- (4-1)*(4-1)

pchisq(chi, df, lower.tail=FALSE)



########################## 8 ###########################

n_computers <- 20
p_success <- 0.5

n <- 10000

# Manual simulation
result <- c()
for (x in 1:n) {
  result <- append(result, rbinom(n=1, size=n_computers, p_success), after=length(result))
}

hist(result)

# Non parametric: there is no sample with value of 20 among all 10000 samples!! 
table(result)

# Parametric:

# Test statistics
p = mean(result)/20
SE = sqrt(p*(1-p)/n)

print(p)
print(SE)

# Z score
z <- ( 1 - p )/SE

print(z)

# P-value
print(pnorm(z, lower.tail = FALSE))
