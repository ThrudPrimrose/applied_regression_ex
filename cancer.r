library(tidyverse)
library(catdata)

crab <- read_csv("data/skin_cancer.csv")

vec <- c()

for (row in 1:nrow(crab)) {
  age_string <- crab[row, "age"]
  if (("85+" == age_string)) {
    vec <- c(vec, 90)
  } else {
    low <- substr(age_string, start = 1, stop = 2)
    up <- substr(age_string, start = 4, stop = 5)
    loi <- as.integer(low)
    upi <- as.integer(up)
    mean <- (loi + upi) / 2
    vec <- c(vec, mean)
  }
}

crab$age_i <- vec

print(crab)

gp <- ggplot(data = crab, mapping = aes(x = age_i, y = ncases, color = city))
gp <- gp + geom_point()

print(gp)

poisson_fit <- glm(formula = ncases ~ age_i + city, family = "poisson", data = crab)

gp <- gp + geom_smooth(method = "glm", data = crab, se = FALSE, method.args = list(family = "poisson"), formula = y ~ x)
gp <- gp + geom_smooth(method = "lm", data = crab, se = FALSE, formula = y ~ poly(x, degree = 2))

print(gp)

max_fit <- glm(formula = ncases ~ age_i + city + popsize, family = "poisson", data = crab)
stepAIC(max_fit, direction = "backward")

quad <- lm(formula = ncases ~ poly(age_i, degree = 2) + city, data = crab)

gp <- gp + stat_function(fun = lm,
  args = list(formula = ncases ~ poly(age_i, degree = 2) + city, data = crab),
  colour = "blue", size = 1.3)

plot(quad)

print(gp)

riskMinnea <- c()
riskDallas <- c()

for (row in 1:nrow(crab)) {
  s <- crab[row, "city"]
  f <- crab[row, "ncases"] / crab[row, "popsize"]
  if (("Minneapolis - St Paul" == s)) {
    riskMinnea <- c(riskMinnea, f)
  } else {
    riskDallas <- c(riskDallas, f)
  }
}

riskMinnea <- unlist(riskMinnea)
riskDallas <- unlist(riskDallas)

print(typeof(riskMinnea))
print("===")
print(typeof(riskDallas))

v <- riskMinnea / riskDallas
print(v)

cancer_fit_age_group <- glm(ncases ~ age + city, offset = log(popsize), data = crab, family = poisson(link = "log"))
summary(cancer_fit_age_group)

crab$fitted_group <- fitted(cancer_fit_age_group)
crab$rates <- crab$ncases / crab$popsize

ggplot(crab, aes(age, color = city)) +
  geom_point(aes(y = rates)) +
  geom_point(aes(y = fitted_group / popsize), shape = 5, size = 3)


cancer_fit_age_cont <- glm(ncases ~ poly(age_i, 2, raw = TRUE) +
  city, offset = log(popsize), data = crab, family = poisson(link = "log"))

crab$fitted_i <- fitted(cancer_fit_age_cont)

ggplot(crab, aes(age_i, color = city)) +
  geom_point(aes(y = rates)) +
  geom_line(aes(y = fitted_group / popsize, linetype = "model with grouped age")) +
  geom_line(aes(y = fitted_i / popsize, linetype = "model with continuous age"))

tmp <- ggplot(crab, aes(age_i, color = city)) +
  geom_point(aes(y = ncases)) +
  geom_line(aes(y = fitted_group / popsize, linetype = "model with grouped age")) +
  geom_line(aes(y = fitted_i / popsize, linetype = "model with continuous age"))

print(tmp)