library(tidyverse)
library(GGally)
library(car)
library(ggfortify)

crime <- read_csv("data/crime.csv")

crime <- filter(crime, state != "dc")

# print(crime, n = Inf)

crime_model <- lm(crime ~ pctmetro + pctwhite + pcths + poverty + single, data = crime)

res <- residuals(crime_model)
autoplot(crime_model)

# apply box cox transformation
boxCox(crime_model)

# lambda = 0 -> log
# lambda = 0.5 -> square root
# lambda = 1 -> no transform
# lambda = 2 -> square 
summary(p1 <- powerTransform(crime_model))

coef(p1, round = TRUE)

summary(m1 <- lm(bcPower(crime, p1$roundlam) ~ pctmetro + pctwhite + pcths + poverty + single, data = crime))
autoplot(m1)

summary(m2 <- lm(bcPower(crime, 0.0) ~ pctmetro + pctwhite + pcths + poverty + single, data = crime))
autoplot(m2)