
# ============================================
# R Script Solutions for UMA401: Probability and Statistics Lab
# TIET, Patiala
# ============================================

# Experiment 1: Basics of R Programming
# --------------------------------------

# (1) Maximum and Minimum of a vector
c <- c(5, 10, 15, 20, 25, 30)
cat("Max:", max(c), "\n")
cat("Min:", min(c), "\n")

# (2) Factorial with error check
factorial_calc <- function(n) {
  if(n < 0) {
    return("Error: Negative number")
  } else {
    return(factorial(n))
  }
}
cat("Factorial of 5:", factorial_calc(5), "\n")

# (3) Fibonacci sequence
fibonacci <- function(n) {
  fib <- numeric(n)
  fib[1] <- 0
  if(n > 1) fib[2] <- 1
  for(i in 3:n) {
    fib[i] <- fib[i-1] + fib[i-2]
  }
  return(fib)
}
cat("First 10 Fibonacci numbers:", fibonacci(10), "\n")

# (4) Simple calculator
calculator <- function(a, b, op) {
  switch(op,
         "+" = return(a + b),
         "-" = return(a - b),
         "*" = return(a * b),
         "/" = if(b != 0) return(a / b) else return("Division by zero error")
  )
}
cat("Calculator: 10 + 5 =", calculator(10, 5, "+"), "\n")

# (5) Plots
x <- c(1, 2, 3, 4, 5)
y <- c(2, 3, 5, 7, 11)
plot(x, y, main="Line Plot", type="b")
pie(x, labels=paste("Slice", x))
barplot(y, names.arg=x, col="lightblue")

# --------------------------------------
# More experiments (2 to 8) will follow
# These will include:
# - Descriptive statistics and probability
# - Distributions (Binomial, Normal, Poisson, etc.)
# - Expectation and variance
# - Continuous distributions
# - Joint distributions
# - Chi-square, t, and F-distributions
# - Hypothesis testing, regression
# --------------------------------------

# ============================================
# Experiment 2: Descriptive statistics, Sample space, definition of probability
# --------------------------------------------

# (1a) Sampling coins
coins <- c(rep("Gold", 20), rep("Silver", 30), rep("Bronze", 50))
set.seed(1)
sample_draw <- sample(coins, 10)
cat("Sample draw:", sample_draw, "\n")

# (1b) Surgical procedure outcomes
surgery_outcomes <- sample(c("Success", "Failure"), 10, replace=TRUE, prob=c(0.9, 0.1))
cat("Surgery outcomes:", surgery_outcomes, "\n")

# (2a & 2b) Birthday paradox simulation
birthday_sim <- function(n, trials=1000) {
  match_count <- 0
  for (i in 1:trials) {
    bdays <- sample(1:365, n, replace=TRUE)
    if (length(unique(bdays)) < n) match_count <- match_count + 1
  }
  return(match_count / trials)
}
# Find minimum n for which probability > 0.5
n <- 1
repeat {
  p <- birthday_sim(n)
  if (p > 0.5) break
  n <- n + 1
}
cat("Minimum n for birthday match > 0.5 is:", n, "\n")

# (3) Conditional probability using Bayes theorem
conditional_prob <- function(P_cloudy, P_rain, P_cloud_given_rain) {
  P_rain_given_cloud <- (P_cloud_given_rain * P_rain) / P_cloudy
  return(P_rain_given_cloud)
}
cat("P(Rain | Cloudy):", conditional_prob(0.4, 0.2, 0.85), "\n")

# (4) Iris dataset analysis
data(iris)
head(iris)
str(iris)
range(iris$Sepal.Length)
mean(iris$Sepal.Length)
median(iris$Sepal.Length)
quantile(iris$Sepal.Length, probs = c(0.25, 0.75))
IQR(iris$Sepal.Length)
sd(iris$Sepal.Length)
var(iris$Sepal.Length)
summary(iris)

# (5) Mode function
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
cat("Mode of Sepal.Length:", get_mode(iris$Sepal.Length), "\n")

# ============================================
# Experiment 3: Probability Distributions
# --------------------------------------------

# (1) Binomial Distribution: P(7 <= X <= 9) where X ~ Bin(12, 1/6)
p_7_to_9 <- pbinom(9, size=12, prob=1/6) - pbinom(6, size=12, prob=1/6)
cat("P(7<=X<=9) where X~Bin(12,1/6):", p_7_to_9, "\n")

# (2) Normal Distribution: P(X >= 84) for N(72, 15.2^2)
p_more_than_84 <- pnorm(84, mean=72, sd=15.2, lower.tail=FALSE)
cat("P(X>=84):", p_more_than_84 * 100, "%\n")

# (3) Poisson Distribution
p_0_car <- dpois(0, lambda=5)
p_48_50 <- ppois(50, lambda=50) - ppois(47, lambda=50)
cat("P(0 cars):", p_0_car, "\n")
cat("P(48 to 50 cars):", p_48_50, "\n")

# (4) Hypergeometric Distribution
phyper_custom <- dhyper(3, m=17, n=233, k=5)
cat("P(X=3) for hypergeometric:", phyper_custom, "\n")

# (5) Binomial properties for n=31, p=0.447
n <- 31
p <- 0.447
x_vals <- 0:n
pmf <- dbinom(x_vals, n, p)
cdf <- pbinom(x_vals, n, p)
mean_x <- n * p
var_x <- n * p * (1 - p)
sd_x <- sqrt(var_x)
cat("Mean:", mean_x, "Variance:", var_x, "SD:", sd_x, "\n")

# PMF and CDF plots
plot(x_vals, pmf, type="h", main="Binomial PMF", ylab="P(X=x)")
plot(x_vals, cdf, type="s", main="Binomial CDF", ylab="P(X<=x)")


# ============================================
# Experiment 4: Mathematical Expectation, Moments and Functions of Random Variables
# --------------------------------------------

# (1) Expected value using probability mass function
x <- 0:4
p_x <- c(0.41, 0.37, 0.16, 0.05, 0.01)
expected_x <- sum(x * p_x)
cat("Expected value of X:", expected_x, "\n")

# (2) Expected value of continuous random variable
f_t <- function(t) { t * 0.1 * exp(-0.1 * t) }
expected_t <- integrate(f_t, lower=0, upper=Inf)
cat("Expected value of T:", expected_t$value, "\n")

# (3) Net revenue expectation
x <- 0:3
p <- c(0.1, 0.2, 0.2, 0.5)
revenue <- c(-12, -2, 8, 18)
expected_revenue <- sum(p * revenue)
cat("Expected Revenue Y:", expected_revenue, "\n")

# (4) First and second moments for PDF
moment1 <- integrate(function(x) x * 0.5 * exp(-abs(x)), lower=1, upper=10)$value
moment2 <- integrate(function(x) x^2 * 0.5 * exp(-abs(x)), lower=1, upper=10)$value
variance <- moment2 - moment1^2
cat("Mean:", moment1, "Variance:", variance, "\n")

# (5) Transforming geometric to Y = X^2
x <- 1:5
px <- (3/4) * (1/4)^(x - 1)
y <- x^2
expected_y <- sum(px * y)
expected_y2 <- sum(px * y^2)
var_y <- expected_y2 - expected_y^2
cat("Expected Y:", expected_y, "Variance of Y:", var_y, "\n")

# ============================================
# Experiment 5: Continuous Probability Distributions
# --------------------------------------------

# (1) Uniform Distribution U(0, 60)
p_more_45 <- punif(45, min=0, max=60, lower.tail=FALSE)
p_20_30 <- punif(30, min=0, max=60) - punif(20, min=0, max=60)
cat("P(X > 45):", p_more_45, "P(20 < X < 30):", p_20_30, "\n")

# (2) Exponential Distribution
lambda <- 0.5
dval <- dexp(3, rate=lambda)
pexp_val <- pexp(3, rate=lambda)
cat("Density at x=3:", dval, "P(X <= 3):", pexp_val, "\n")
curve(dexp(x, rate=lambda), from=0, to=5, main="Exponential PDF", ylab="Density")
curve(pexp(x, rate=lambda), from=0, to=5, main="Exponential CDF", ylab="P(X ≤ x)")
hist(rexp(1000, rate=lambda), breaks=30, col="skyblue", main="Simulated Exponential Data")

# (3) Gamma Distribution: α = 2, β = 1/3
pgamma_3 <- pgamma(3, shape=2, rate=1/3)
pgamma_1c <- 1 - pgamma(1, shape=2, rate=1/3)
qgamma_70 <- qgamma(0.7, shape=2, rate=1/3)
cat("P(X <= 3):", pgamma_3, "P(X >= 1):", pgamma_1c, "P(X ≤ c) ≥ 0.70, c =", qgamma_70, "\n")

# ============================================
# Experiment 6: Joint probability mass and density functions
# --------------------------------------------

# (1) Joint PDF validation and marginals
library(cubature)
fxy <- function(x) 2*(2*x[1] + 3*x[2])/5
check_joint <- adaptIntegrate(fxy, lowerLimit=c(0,0), upperLimit=c(1,1))$integral
cat("Integral of joint PDF:", check_joint, "\n")

gx <- function(y) 2*(2*1 + 3*y)/5
hx <- function(x) 2*(2*x + 3*0)/5
gx_val <- integrate(gx, 0, 1)$value
hx_val <- integrate(hx, 0, 1)$value
expected_xy <- adaptIntegrate(function(x) x[1]*x[2]*2*(2*x[1] + 3*x[2])/5, c(0,0), c(1,1))$integral
cat("g(x=1):", gx_val, "h(y=0):", hx_val, "E[XY]:", expected_xy, "\n")

# (2) Joint PMF
x <- 0:3
y <- 0:2
pmf_matrix <- outer(x, y, Vectorize(function(x,y) (x+y)/30))
rownames(pmf_matrix) <- paste("X=", x)
colnames(pmf_matrix) <- paste("Y=", y)
print(pmf_matrix)

sum_pmf <- sum(pmf_matrix)
marginal_x <- apply(pmf_matrix, 1, sum)
marginal_y <- apply(pmf_matrix, 2, sum)
cond_x0_y1 <- pmf_matrix["X=0", "Y=1"] / marginal_y["Y=1"]

ex <- sum(x * marginal_x)
ey <- sum(y * marginal_y)
exy <- sum(outer(x, y, Vectorize(function(i,j) i*j*(i+j)/30)))
varx <- sum((x - ex)^2 * marginal_x)
vary <- sum((y - ey)^2 * marginal_y)
covxy <- exy - ex * ey
correlation <- covxy / (sqrt(varx) * sqrt(vary))

cat("Sum of PMF:", sum_pmf, "E[X]:", ex, "E[Y]:", ey, "Var(X):", varx, "Var(Y):", vary,
    "Cov(X,Y):", covxy, "Correlation:", correlation, "\n")

# ============================================
# Experiment 7: Chi-square, t-distribution, F-distribution
# --------------------------------------------

# (1) t-distribution histogram
hist(rt(100, df=10), main="t-distribution, df=10")

# (2) Chi-square distribution
hist(rchisq(100, df=2), main="Chi-square df=2")
hist(rchisq(100, df=10), main="Chi-square df=10")
hist(rchisq(100, df=25), main="Chi-square df=25")

# (3) Student t-distributions comparison
x_vals <- seq(-6, 6, length=100)
plot(x_vals, dt(x_vals, df=1), type="l", col="red", ylim=c(0, 0.4), main="t-distribution comparison")
lines(x_vals, dt(x_vals, df=4), col="blue")
lines(x_vals, dt(x_vals, df=10), col="green")
lines(x_vals, dt(x_vals, df=30), col="purple")
legend("topright", legend=c("df=1","df=4","df=10","df=30"),
       col=c("red","blue","green","purple"), lty=1)

# (4) F-distribution calculations
qf_val <- qf(0.95, df1=10, df2=20)
pf_0_1_5 <- pf(1.5, df1=10, df2=20)
pf_above_1_5 <- 1 - pf_0_1_5
quantiles <- qf(c(0.25, 0.5, 0.75, 0.999), df1=10, df2=20)
f_samples <- rf(1000, df1=10, df2=20)
hist(f_samples, breaks=30, col="lightblue", main="F-distribution Samples")

cat("95th percentile (F):", qf_val, "\nQuantiles:", quantiles, "\n")

# ============================================
# Experiment 8: Sample statistics and Hypothesis Testing
# --------------------------------------------

# (1) CSV File Analysis (assume data in 'Clt-data.csv')
# df <- read.csv("Clt-data.csv")
# head(df, 10)
# cat("Rows:", nrow(df), "\n")
# mean_val <- mean(df$WallThickness)
# hist(df$WallThickness, main="Wall Thickness", xlab="Thickness")
# abline(v=mean_val, col="red", lwd=2)
# Simulations omitted due to no data file

# (2) Regression analysis
age <- c(58, 69, 43, 39, 63, 52, 47, 31, 74, 36)
chol <- c(189, 235, 193, 177, 154, 191, 213, 165, 198, 181)
plot(age, chol, main="Age vs Cholesterol", xlab="Age", ylab="Cholesterol")
model <- lm(chol ~ age)
abline(model, col="blue")
pred <- predict(model, data.frame(age=60))
cat("Predicted cholesterol at age 60:", pred, "\n")

# (3) Paired t-test
before <- c(145, 173, 158, 141, 167, 159, 154, 167, 145, 153)
after <- c(155, 167, 156, 149, 168, 162, 158, 169, 157, 161)
t_test_result <- t.test(after, before, paired=TRUE, alternative="greater")
print(t_test_result)
