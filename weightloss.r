library(tidyverse)
# for matrix manipulation
library(matlib)
library(ggfortify)

weight <- read_csv("data/weightloss.csv")

X <- cbind(rep(1, ncol(weight)), weight$Before)
Xtr <- t(X)

# Hat Matrix
# H = X(X'X)^{-1}X'
Hat <- X %*% inv(Xtr %*% X) %*% Xtr

# The leverage = diagonal values of hat matrix
lev <- diag(Hat)
x_vals <- weight$Before
lev_df <- data.frame(x = x_vals, y = lev)
weight$Leverage <- lev
weight$Leverage_Point <- lev > 2 * mean(lev)

# Plot Leverate
ggplot(data = weight, aes(x = x_vals, y = lev, color = Leverage_Point)) +
    geom_point() +
    labs(x = "h_{ii}", y = "weight before diet\n") +
    theme_bw()

weight_model <- lm(weight$Loss ~ Before, data = weight)
fitted_weight_loss <- fitted.values(weight_model)

# calculate standarized residuals
# first residuals
res <- residuals(weight_model)

#standardized residuals
weight$stdres <- res / sigma(weight_model)

ggplot(weight, aes(x = fitted_weight_loss, y = stdres)) +
    geom_point() +
    labs(x = "fitted values", y = "standardized residuals") +
    theme_bw()

weight$outlier_res <- abs(weight$stdres) > 2

ggplot(weight, aes(x = Before, y = Loss)) +
    geom_point() +
    geom_smooth(method = "lm", aes(color = "no")) +
    geom_smooth(data = weight[-28,], aes(color = "yes"), method = "lm") +
    scale_colour_manual(name = "obs 28 removed", values = c("blue", "red")) +
    labs(x = "Weight before diet in pounds", y = "Weight loss in pounds\n(before - after diet)") #+
#theme_bw()

# cooks distance
# which choses a function and 4 is for cooks distance ?
cook_dist <- cooks.distance(weight_model)
autoplot(weight_model, which = 4)
