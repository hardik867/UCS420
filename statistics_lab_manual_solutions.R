
# ===================================
# Lab Manual R Solutions - Statistics
# ===================================

# ========== Experiment 1 ==========
c <- c(5, 10, 15, 20, 25, 30)
max(c)
min(c)

n <- as.integer(readline("Enter number: "))
if(n < 0) {
  print("Negative number")
} else {
  print(factorial(n))
}

n <- as.integer(readline("Enter n: "))
a <- 0
b <- 1
cat(a, b)
for(i in 3:n) {
  c <- a + b
  cat(c)
  a <- b
  b <- c
}

a <- as.numeric(readline("First number: "))
b <- as.numeric(readline("Second number: "))
op <- readline("Enter +, -, *, /: ")
if(op == "+") print(a + b)
if(op == "-") print(a - b)
if(op == "*") print(a * b)
if(op == "/") print(a / b)

x <- c(10, 20, 30)
labels <- c("A", "B", "C")
pie(x, labels)
barplot(x)
plot(x)

# ========== Experiment 2 ==========
sample(c(rep("Gold",20), rep("Silver",30), rep("Bronze",50)), 10)
sample(c("Success", "Fail"), 10, replace=TRUE, prob=c(0.9, 0.1))

set.seed(123)
n <- 23
count <- 0
for(i in 1:10000) {
  b <- sample(1:365, n, replace=TRUE)
  if(length(unique(b)) < n) count <- count + 1
}
count / 10000

P_cloud <- 0.4
P_rain <- 0.2
P_cloud_given_rain <- 0.85
P_rain_given_cloud <- (P_cloud_given_rain * P_rain) / P_cloud
P_rain_given_cloud

data(iris)
head(iris)
str(iris)
range(iris$Sepal.Length)
mean(iris$Sepal.Length)
median(iris$Sepal.Length)
IQR(iris$Sepal.Length)
sd(iris$Sepal.Length)
var(iris$Sepal.Length)
summary(iris)

getmode <- function(v) {
  uniq <- unique(v)
  uniq[which.max(tabulate(match(v, uniq)))]
}
getmode(iris$Sepal.Length)

# ========== Experiment 3 ==========
pbinom(9, 12, 1/6) - pbinom(6, 12, 1/6)
1 - pnorm(84, 72, 15.2)
dpois(0, 5)
ppois(50, 50) - ppois(47, 50)
dhyper(3, 17, 233, 5)

n <- 0:31
prob <- dbinom(n, 31, 0.447)
plot(n, prob, type="h")
plot(n, pbinom(n, 31, 0.447), type="s")
mean <- 31 * 0.447
var <- 31 * 0.447 * (1 - 0.447)
sd <- sqrt(var)
mean
var
sd

# ========== Experiment 4 ==========
x <- 0:4
p <- c(0.41, 0.37, 0.16, 0.05, 0.01)
sum(x * p)

f <- function(t) 0.1 * exp(-0.1 * t) * t
integrate(f, 0, Inf)

x <- 0:3
p <- c(0.1, 0.2, 0.2, 0.5)
revenue <- c(-10, 4, 18, 32)
sum(revenue * p)

f <- function(x) 0.5 * exp(-abs(x))
m1 <- integrate(function(x) x * f(x), 1, 10)
m2 <- integrate(function(x) x^2 * f(x), 1, 10)
mean <- m1$value
var <- m2$value - mean^2
mean
var

x <- 1:5
p <- (3/4) * (1/4)^(x - 1)
y <- x^2
EY <- sum(y * p)
VY <- sum((y^2) * p) - EY^2
EY
VY

# ========== Experiment 5 ==========
1 - punif(45, 0, 60)
punif(30, 0, 60) - punif(20, 0, 60)

dexp(3, 0.5)
curve(dexp(x, 0.5), 0, 5)
pexp(3, 0.5)
curve(pexp(x, 0.5), 0, 5)
hist(rexp(1000, 0.5), breaks=30)

dgamma(3, 2, 1/3)
1 - pgamma(1, 2, 1/3)
qgamma(0.70, 2, 1/3)

# ========== Experiment 6 ==========
library(cubature)
f <- function(xy) 2*(2*xy[1] + 3*xy[2])/5
adaptIntegrate(f, c(0,0), c(1,1))

gx1 <- integrate(function(y) f(c(1,y)), 0, 1)
hy0 <- integrate(function(x) f(c(x,0)), 0, 1)
gx1
hy0

exy <- function(xy) xy[1]*xy[2]*f(xy)
adaptIntegrate(exy, c(0,0), c(1,1))

x <- 0:3
y <- 0:2
fxy <- outer(x, y, function(x, y) (x + y)/30)
fxy
sum(fxy)
gx <- apply(fxy, 1, sum)
hy <- apply(fxy, 2, sum)
fxy[1,2] / hy[2]

Ex <- sum(x * gx)
Ey <- sum(y * hy)
Exy <- sum(outer(x, y, "*") * fxy)
Varx <- sum((x^2) * gx) - Ex^2
Vary <- sum((y^2) * hy) - Ey^2
Covxy <- Exy - Ex * Ey
Corr <- Covxy / sqrt(Varx * Vary)
Ex
Ey
Varx
Vary
Corr

# ========== Experiment 7 ==========
x <- rt(100, 10)
hist(x)

hist(rchisq(100, 2))
hist(rchisq(100, 10))
hist(rchisq(100, 25))

x <- seq(-6, 6, length=100)
plot(x, dt(x, 30), type="l")
lines(x, dt(x, 1), col="red")
lines(x, dt(x, 4), col="blue")
lines(x, dt(x, 10), col="green")

qf(0.95, 10, 20)
pf(1.5, 10, 20)
1 - pf(1.5, 10, 20)
qf(c(0.25, 0.5, 0.75, 0.999), 10, 20)
hist(rf(1000, 10, 20))

# ========== Experiment 8 ==========
data <- read.csv("Clt-data.csv")
head(data, 10)
nrow(data)
hist(data$thickness)
abline(v=mean(data$thickness), col="red")

means_10 <- replicate(1000, mean(sample(data$thickness, 10)))
hist(means_10)

means_50 <- replicate(1000, mean(sample(data$thickness, 50)))
hist(means_50)

age <- c(58,69,43,39,63,52,47,31,74,36)
chol <- c(189,235,193,177,154,191,213,165,198,181)
plot(age, chol)
model <- lm(chol ~ age)
abline(model)
predict(model, data.frame(age=60))

before <- c(145,173,158,141,167,159,154,167,145,153)
after <- c(155,167,156,149,168,162,158,169,157,161)
t.test(before, after, paired=TRUE)
