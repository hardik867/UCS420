
# -------------------------------
# PROBABILITY MASS FUNCTION TASKS
# -------------------------------

# (1) Joint Probability Density Function
# --------------------------------------
# f(x, y) = 2(2x + 3y)/5 for 0 <= x, y <= 1

library(cubature)  # For integral2()

# (i) Check if it's a valid joint density function
f_joint <- function(x) {
  2 * (2 * x[1] + 3 * x[2]) / 5
}
integral_result <- adaptIntegrate(f_joint, lowerLimit = c(0, 0), upperLimit = c(1, 1))
print(paste("Integral over [0,1]x[0,1]:", round(integral_result$integral, 4)))

# (ii) Marginal g(x) = ∫ f(x,y) dy from y = 0 to 1, evaluate at x = 1
gx <- function(y) 2 * (2 * 1 + 3 * y) / 5
gx_val <- integrate(gx, lower = 0, upper = 1)$value
print(paste("g(x=1):", round(gx_val, 4)))

# (iii) Marginal h(y) = ∫ f(x,y) dx from x = 0 to 1, evaluate at y = 0
hy <- function(x) 2 * (2 * x + 3 * 0) / 5
hy_val <- integrate(hy, lower = 0, upper = 1)$value
print(paste("h(y=0):", round(hy_val, 4)))

# (iv) Expected value of g(x, y) = xy
gxy <- function(x) x[1] * x[2] * 2 * (2 * x[1] + 3 * x[2]) / 5
exy_result <- adaptIntegrate(gxy, lowerLimit = c(0, 0), upperLimit = c(1, 1))
print(paste("E[XY]:", round(exy_result$integral, 4)))

# (2) Joint Probability Mass Function
# ------------------------------------
# f(x, y) = (x + y)/30 for x = 0:3, y = 0:2

x_vals <- 0:3
y_vals <- 0:2
pmf_matrix <- outer(x_vals, y_vals, function(x, y) (x + y)/30)

# (i) Display joint mass function matrix
print("Joint PMF matrix:")
print(pmf_matrix)

# (ii) Check if it is valid PMF
total_prob <- sum(pmf_matrix)
print(paste("Sum of joint PMF:", round(total_prob, 4)))

# (iii) Marginal distribution g(x)
gx_marginal <- apply(pmf_matrix, 1, sum)
names(gx_marginal) <- paste("x =", x_vals)
print("Marginal g(x):")
print(gx_marginal)

# (iv) Marginal distribution h(y)
hy_marginal <- apply(pmf_matrix, 2, sum)
names(hy_marginal) <- paste("y =", y_vals)
print("Marginal h(y):")
print(hy_marginal)

# (v) Conditional probability P(x=0 | y=1)
p_xy <- pmf_matrix[1, 2]  # x=0, y=1
p_y1 <- hy_marginal[2]    # y=1
p_cond <- p_xy / p_y1
print(paste("P(x=0 | y=1):", round(p_cond, 4)))

# (vi) Compute expectations, variances, covariance, and correlation
# Flatten matrix
fxy <- as.vector(pmf_matrix)
xy_grid <- expand.grid(x = x_vals, y = y_vals)

Ex <- sum(xy_grid$x * fxy)
Ey <- sum(xy_grid$y * fxy)
Exy <- sum(xy_grid$x * xy_grid$y * fxy)

Varx <- sum((xy_grid$x^2) * fxy) - Ex^2
Vary <- sum((xy_grid$y^2) * fxy) - Ey^2

Cov_xy <- Exy - Ex * Ey
cor_xy <- Cov_xy / sqrt(Varx * Vary)

print(paste("E(X):", round(Ex, 4)))
print(paste("E(Y):", round(Ey, 4)))
print(paste("E(XY):", round(Exy, 4)))
print(paste("Var(X):", round(Varx, 4)))
print(paste("Var(Y):", round(Vary, 4)))
print(paste("Cov(X,Y):", round(Cov_xy, 4)))
print(paste("Correlation Coefficient:", round(cor_xy, 4)))
