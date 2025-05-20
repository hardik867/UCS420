
# ============================================================
# Experiment 3: Probability Distributions
# ============================================================

# (1) Binomial distribution
pbinom(9, size = 12, prob = 1/6) - pbinom(6, size = 12, prob = 1/6)

# (2) Normal distribution: P(X >= 84)
1 - pnorm(84, mean = 72, sd = 15.2)

# (3) Poisson distribution
dpois(0, lambda = 5)
ppois(50, lambda = 50) - ppois(47, lambda = 50)

# (4) Hypergeometric distribution
dhyper(3, m = 17, n = 233, k = 5)

# (5) Binomial properties
n <- 31
p <- 0.447
x <- 0:n
pmf <- dbinom(x, n, p)
cdf <- pbinom(x, n, p)
mean_val <- n * p
var_val <- n * p * (1 - p)
sd_val <- sqrt(var_val)

# ============================================================
# Experiment 4: Expectation, Moments & Functions
# ============================================================

# (1) Expected value
x <- 0:4
px <- c(0.41, 0.37, 0.16, 0.05, 0.01)
sum(x * px)

# (2) Expected value of T
f <- function(t) { t * 0.1 * exp(-0.1 * t) }
integrate(f, 0, Inf)

# (3) Revenue expected value
x <- 0:3
px <- c(0.1, 0.2, 0.2, 0.5)
revenue <- function(x) {
  12*x - 6*3 + (3 - x)*2
}
expected_revenue <- sum(revenue(x) * px)
expected_revenue

# (4) Moments
f <- function(x) 0.5 * exp(-abs(x))
moment1 <- integrate(function(x) x * f(x), 1, 10)$value
moment2 <- integrate(function(x) x^2 * f(x), 1, 10)$value
moment1
moment2 - moment1^2  # Variance

# (5) Geometric variable Y = X^2
p_geom <- function(x) (3/4)*(1/4)^(x-1)
x <- 1:5
y <- x^2
py <- p_geom(x)
expected_y <- sum(y * py)
var_y <- sum(y^2 * py) - expected_y^2
expected_y
var_y

# ============================================================
# Experiment 5: Continuous Distributions
# ============================================================

# (1) Uniform
1 - punif(45, 0, 60)
punif(30, 0, 60) - punif(20, 0, 60)

# (2) Exponential
dexp(3, rate = 1/2)
curve(dexp(x, rate = 1/2), from = 0, to = 5)
pexp(3, rate = 1/2)
curve(pexp(x, rate = 1/2), from = 0, to = 5)
hist(rexp(1000, rate = 1/2))

# (3) Gamma
dgamma(3, shape = 2, rate = 1/3)
1 - pgamma(1, shape = 2, rate = 1/3)
qgamma(0.70, shape = 2, rate = 1/3)

# ============================================================
# Experiment 6: Joint Distributions
# ============================================================

# (1) Joint density function
library(cubature)
f <- function(x) 2*(2*x[1] + 3*x[2])/5
adaptIntegrate(f, lowerLimit = c(0,0), upperLimit = c(1,1))

# (2) Joint mass function
x <- 0:3
y <- 0:2
f_xy <- outer(x, y, function(x, y) (x + y)/30)
rowSums(f_xy)
colSums(f_xy)
sum(f_xy)
f_xy[1,2]/sum(f_xy)  # P(x=0 | y=1)
EX <- sum(x * rowSums(f_xy))
EY <- sum(y * colSums(f_xy))
EXY <- sum(outer(x, y, "*") * f_xy)
VX <- sum((x - EX)^2 * rowSums(f_xy))
VY <- sum((y - EY)^2 * colSums(f_xy))
COVXY <- EXY - EX*EY
CORXY <- COVXY / (sqrt(VX) * sqrt(VY))

# ============================================================
# Experiment 7: t, Chi-square, F-distributions
# ============================================================

# (1) t-distribution
hist(rt(100, df = 10))

# (2) Chi-square
hist(rchisq(100, df = 2))
hist(rchisq(100, df = 10))
hist(rchisq(100, df = 25))

# (3) t-distribution comparison
x <- seq(-6, 6, length.out = 100)
plot(x, dt(x, df = 1), type = "l", col = "red")
lines(x, dt(x, df = 4), col = "blue")
lines(x, dt(x, df = 10), col = "green")
lines(x, dt(x, df = 30), col = "black")

# (4) F-distribution
qf(0.95, df1 = 10, df2 = 20)
pf(1.5, 10, 20)
1 - pf(1.5, 10, 20)
qf(c(0.25, 0.5, 0.75, 0.999), 10, 20)
hist(rf(1000, 10, 20))

# ============================================================
# Experiment 8: Hypothesis Testing
# ============================================================

# (1) Normality check
# Assuming 'Clt-data.csv' is in the working directory
data <- read.csv("Clt-data.csv")
hist(data$wall_thickness)
abline(v = mean(data$wall_thickness), col = "red", lwd = 2)

# Sampling
sample_means <- replicate(1000, mean(sample(data$wall_thickness, 10, replace = TRUE)))
hist(sample_means)

# (2) Regression
age <- c(58, 69, 43, 39, 63, 52, 47, 31, 74, 36)
chol <- c(189, 235, 193, 177, 154, 191, 213, 165, 198, 181)
plot(age, chol)
model <- lm(chol ~ age)
abline(model, col = "blue")
predict(model, data.frame(age = 60))

# (3) Paired t-test
before <- c(145, 173, 158, 141, 167, 159, 154, 167, 145, 153)
after  <- c(155, 167, 156, 149, 168, 162, 158, 169, 157, 161)
t.test(before, after, paired = TRUE)
