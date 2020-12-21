# load tidyverse
library(tidyverse)

LAozone <- read_csv("data/LAozone.csv")
LAozone_small <- LAozone[, c("ozone", "temp")]

# apply a function to an array of values
# 1 -> rows
# 2 -> columns
# c(1,2) -> rows and columns
means = apply(LAozone_small, 2, mean)
print(means)

# sample covariance
# 1/(n-1)
covs = cov(LAozone_small)
print(covs)

# b1 = s^2_{xy} / s^2_{xx}
b1 = covs[1, 2] / covs[2, 2]
print(b1)

# intercept
# b0 = y_mean - x_mean * b1
b0 = means["ozone"] - means["temp"] * b1
print(b0)

# model fit
# r2 = (s^2_{xy})^2 / (s^2_{xx} * s^2_{yy})
r2 = covs[1, 2] * covs[2, 1] / (covs[1, 1] * covs[2, 2])
print(r2)

# aprroximations for ozone value when temp is 70 and 60
reg <- function(intercept, slope, x_loc) {
  return(intercept + slope * x_loc)
}

y70 = reg(b0, b1, 70)
y60 = reg(b0, b1, 60)
print(y70)
print(y60)

# estimator for variance
# s^2 = (1/(n-2)) * Sum^n_{i=1}{(y_i - y_hat_i)^2}
# s^2 = (1/(n-2)) * SS_residual
ozones = LAozone_small["ozone"]
temps = LAozone_small["temp"] 
s2 =  (1 / (lengths(ozones) - 2))
y_diffs = 0.0
for (offset in 1:lengths(ozones)) {
  y_diffs = y_diffs + ((ozones[offset,][[1]] - reg(b0, b1, temps[offset,][[1]])) ** 2)
  offset = offset + 1
}
s2 = s2 * y_diffs
print(s2)

# T-test 
# T <- b1 * s_xx / s
T = b1 * sqrt((lengths(ozones) - 1) * covs[2, 2]) / sqrt(s2)

# should be within t distribution value
Tval = qt(0.995, 328)
sprintf("%f <? %f", T, Tval)

# linear regression in R
# see https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm
ozone_temp_model <- lm(ozone ~ temp, data = LAozone_small)
summary(ozone_temp_model)
# r^2 = Multiple R-Squared
# estimate for sigma (s) = residual standard error

# ex-4
cor_matrix = cor(LAozone)
print(cor_matrix)

# correlation of ozone to other variables
cor_matrix[1,]

# temp as highest correlation choose it is predictor variable
model_1 <- lm(ozone ~ temp, data = LAozone)

LAozone$season <- factor(if_else(LAozone$doy <= 89 | LAozone$doy > 273, "winter", "summer"))
model_2 <- lm(ozone ~ season, data = LAozone)

# extract model coefficients
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/coef
coef(model_1)
coef(model_2)

summary(model_1)
summary(model_2)

round(summary(model_2)$r.squared, 2) * 100