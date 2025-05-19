
# R Script: Complete Solutions for Probability and Statistics (UMA401) Lab Manual
# Author: [Your Name]
# Course: UMA401 - Probability and Statistics
# Institution: TIET, Patiala

# ======================================================
# Experiment 1: Basics of R programming
# ======================================================

# Q1: Create a vector and return max and min
# Explanation: We define a numeric vector and use max() and min() functions.
c <- c(5, 10, 15, 20, 25, 30)
cat("Experiment 1 - Q1: Max =", max(c), " Min =", min(c), "\n")

# Q2: Factorial with input validation
factorial_func <- function(n) {
  if (n < 0) {
    return("Error: Negative number.")
  } else {
    return(factorial(n))
  }
}
cat("Experiment 1 - Q2: Factorial of 5 =", factorial_func(5), "\n")

# Q3: Fibonacci series
fibonacci <- function(n) {
  fib <- numeric(n)
  fib[1] <- 0
  if (n > 1) fib[2] <- 1
  for (i in 3:n) fib[i] <- fib[i-1] + fib[i-2]
  return(fib)
}
cat("Experiment 1 - Q3: Fibonacci(10) =", fibonacci(10), "\n")

# Q4: Simple Calculator
calculator <- function(a, b, op) {
  return(switch(op,
                "+" = a + b,
                "-" = a - b,
                "*" = a * b,
                "/" = ifelse(b == 0, "Divide by zero", a / b),
                "Invalid operator"))
}
cat("Experiment 1 - Q4: 10 + 2 =", calculator(10, 2, "+"), "\n")

# Q5: Plotting
x <- c(10, 20, 30); labels <- c("A", "B", "C")
pie(x, labels, main="Pie Chart")
barplot(x, names.arg=labels, main="Bar Chart")
plot(x, type="o", main="Line Plot")

# ======================================================
# Experiment 2: Descriptive statistics and probability
# ======================================================

# Q1(a): Random sample from coin chest
set.seed(1)
sample(c(rep("Gold",20), rep("Silver",30), rep("Bronze",50)), 10)

# Q1(b): Simulate surgery outcomes
sample(c("Success", "Failure"), 10, replace=TRUE, prob=c(0.9, 0.1))

# Q2: Birthday paradox simulation
birthday_prob <- function(n, trials=10000) {
  count <- 0
  for (i in 1:trials) {
    bdays <- sample(1:365, n, replace=TRUE)
    if (length(unique(bdays)) < n) count <- count + 1
  }
  return(count / trials)
}
cat("Experiment 2 - Q2: P(n=23) =", birthday_prob(23), "\n")

# Q3: Conditional probability
P_rain_given_cloudy <- function() {
  P_rain <- 0.2
  P_cloudy <- 0.4
  P_cloudy_given_rain <- 0.85
  return((P_cloudy_given_rain * P_rain) / P_cloudy)
}
cat("Experiment 2 - Q3: P(Rain | Cloudy) =", P_rain_given_cloudy(), "\n")

# Q4: Iris dataset analysis
data(iris)
summary(iris$Sepal.Length)
sd(iris$Sepal.Length)
var(iris$Sepal.Length)

# Q5: Mode function
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
cat("Experiment 2 - Q5: Mode of Sepal.Length =", get_mode(iris$Sepal.Length), "\n")

# ======================================================
# Experiment 3: Probability Distributions
# ======================================================

# Q1: Binomial - Dice example
p <- sum(dbinom(7:9, size=12, prob=1/6))
cat("Experiment 3 - Q1: P(7 <= X <= 9) =", p, "\n")

# Q2: Normal distribution score
cat("Experiment 3 - Q2: P(X >= 84) =", pnorm(84, mean=72, sd=15.2, lower.tail=FALSE), "\n")

# Q3: Poisson distribution
cat("Experiment 3 - Q3: P(X=0) =", dpois(0, lambda=5), "\n")
cat("P(48 <= Y <= 50) =", sum(dpois(48:50, lambda=50)), "\n")

# Q4: Hypergeometric distribution
cat("Experiment 3 - Q4: P(X=3) =", dhyper(3, 17, 233, 5), "\n")

# Q5: Binomial distribution and its properties
n <- 31; p <- 0.447
cat("Experiment 3 - Q5: Mean =", n*p, " Var =", n*p*(1-p), " SD =", sqrt(n*p*(1-p)), "\n")

# ======================================================
# Experiment 4: Mathematical Expectation, Moments
# ======================================================

# Q1: Expected value of discrete variable
x <- 0:4
p <- c(0.41, 0.37, 0.16, 0.05, 0.01)
cat("Experiment 4 - Q1: E(X) =", sum(x * p), "\n")

# Q2: Expected value using integration
f <- function(t) t * 0.1 * exp(-0.1 * t)
cat("Experiment 4 - Q2: E(T) =", integrate(f, 0, Inf)$value, "\n")

# Q3: Expected net revenue
x <- 0:3
p <- c(0.1, 0.2, 0.2, 0.5)
revenue <- c(2*3, 12 - 2*2, 24 - 2, 36)
cat("Experiment 4 - Q3: E(Y) =", sum(revenue * p), "\n")

# ======================================================
# Experiment 5: Continuous Distributions
# ======================================================

# Q1: Uniform distribution
cat("Experiment 5 - Q1a: P(X > 45) =", 1 - punif(45, 0, 60), "\n")
cat("Experiment 5 - Q1b: P(20 < X < 30) =", punif(30, 0, 60) - punif(20, 0, 60), "\n")

# Q2: Exponential distribution
cat("Experiment 5 - Q2a: f(3) =", dexp(3, rate=0.5), "\n")
cat("Experiment 5 - Q2c: P(X <= 3) =", pexp(3, rate=0.5), "\n")

# Q3: Gamma distribution
cat("Experiment 5 - Q3a: P(X<=3) =", pgamma(3, shape=2, rate=1/3), "\n")
cat("Experiment 5 - Q3b: c such that P(X <= c) >= 0.7 =", qgamma(0.7, shape=2, rate=1/3), "\n")

# ======================================================
# Experiment 6: Joint Probability Functions
# ======================================================

# Q2: Discrete joint PMF
x <- 0:3
y <- 0:2
z <- outer(x, y, function(x, y) (x + y) / 30)
cat("Experiment 6 - Q2: PMF sum =", sum(z), "\n")

# ======================================================
# Experiment 7: t, Chi-square, F-distribution
# ======================================================

# Q1: t-distribution
hist(rt(100, df=10), main="t-distribution")

# Q4: F-distribution calculations
cat("Experiment 7 - Q4i: 95th percentile =", qf(0.95, 10, 20), "\n")
cat("Q4ii: Area [0,1.5] =", pf(1.5, 10, 20), " Area [1.5,∞) =", 1 - pf(1.5, 10, 20), "\n")

# ======================================================
# Experiment 8: Hypothesis Testing
# ======================================================

# Q2: Regression
age <- c(58,69,43,39,63,52,47,31,74,36)
chol <- c(189,235,193,177,154,191,213,165,198,181)
model <- lm(chol ~ age)
cat("Experiment 8 - Q2: Predicted cholesterol at age 60 =", predict(model, data.frame(age=60)), "\n")

# Q3: Paired t-test
before <- c(145,173,158,141,167,159,154,167,145,153)
after <- c(155,167,156,149,168,162,158,169,157,161)
t.test(before, after, paired=TRUE)
