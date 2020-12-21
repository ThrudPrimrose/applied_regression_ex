# If you also want Jupyter Notebook with R
# install.packages('IRkernel')
# IRkernel::installspec()
# jupyter labextension install @techrah/text-shortcuts


library(tidyverse)
library(dplyr)
library(growthmodels)
library(ggplot2)

age <- c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0)
area <- c(2.02, 3.62, 5.71, 7.13, 8.33, 8.29, 9.81, 11.3, 12.18, 12.67, 10.62, 12.01)
palmdata <- data.frame(x = age, y = area)
mod <- lm(area ~ age, data = palmdata)

alpha <- tail(area, n = 1) #max y
beta <- -log(area[1] / alpha)

print(alpha)
print(beta)

v <- c()

create_fit_and_plot <- function(alpha, beta, gamma) {
  m <- nls(area ~ a * exp(-b * exp(-gamma * age)), start = c(a = alpha, b = beta), control = list(maxiter = 50))

  print(summary(m)$sigma)
  str <- paste("Gamma: ", toString(gamma), " Alpha: ", toString(alpha), " Beta: ", toString(beta), "\nResiduals: ", summary(m)$sigma)

  p <- ggplot(palmdata, aes(x = age, y = area)) +
   geom_point() +
   geom_smooth(method = "nls",
              method.args = list(formula = y ~ a * exp(-b * exp(-gamma * x)),
                                 start = list(a = alpha, b = beta)),
              data = palmdata,
              se = FALSE,
              aes(color = 'red')) +
              labs(x = "age", y = "area", title = str)

  print(p)
  return(summary(m)$sigma)
  #plot(age, area, main = str)
  #lines(age, predict(m), col = "red", lty = 2, lwd = 3)

  #m$residuals <- residuals(m);
  #acf(ts(m$residuals))
}

# fit data for different gamme values between 0.1 and 0.9
gamma <- 1.0
while (gamma >= 0.1) {
  v = c(v, create_fit_and_plot(alpha, beta, gamma))
  gamma = gamma - 0.05
}

gamma <- 0.30
while (gamma <= 0.42) {
  v = c(v, create_fit_and_plot(alpha, beta, gamma))
  gamma = gamma + 0.005
}

#find min va
a <- which.min(v)
g <- 1.0 - a * 0.05
val <- min(v)
print(paste("Gamma with: ", val, " is minimal"))

# something between 0.25 and 0.3 maybe?
create_fit_and_plot(alpha, beta, 0.36)

# the one in the exampl
create_fit_and_plot(alpha, beta, 0.35894)

# now play around with alpha increase it by +1
for (i in 0:10) {
  create_fit_and_plot(alpha, beta, 0.36)
  alpha = alpha + 1.0
}

# now play around with beta increase it by +1
for (j in 0:10) {
  create_fit_and_plot(alpha, beta, 0.36)
  beta = beta + 1.0
}

# now play around with beta decrease it by +1
for (k in 0:10) {
  create_fit_and_plot(alpha, beta, 0.36)
  beta = beta - 1.0
}

m <- nls(area ~ a * exp(-b * exp(-0.36 * age)), start = c(a = alpha, b = beta), control = list(maxiter = 50))
summary(m)
print("=======================")
lm <- lm(area ~ age, data = palmdata)
summary(lm)
print("=======================")
lmsqr <- lm(area ~ age + I(age ^ 2) - 1, data = palmdata)
summary(lmsqr)

colors <- c("linear" = "red", "quadratic" = "blue", "gompertz" = "green")

p <- ggplot(palmdata, aes(x = age, y = area)) +
  geom_point() +
  geom_smooth(method = "lm",
              data = palmdata,
              se = FALSE,
              aes(color = 'linear')) +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x ^ 2) - 1,
              data = palmdata,
              se = FALSE,
              aes(color = 'quadratic')) +
  geom_smooth(method = "nls",
              method.args = list(formula = y ~ a * exp(-b * exp(-gamma * x)),
                                 start = list(a = alpha, b = beta)),
              data = palmdata,
              se = FALSE,
              aes(color = 'gompertz')) +
  labs(x = "age", y = "area", title = "Alltogether, gamma: 0.36", color = "Legend") +
  scale_color_manual(values = colors)
print(p)
