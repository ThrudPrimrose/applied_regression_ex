library(tidyverse)
library(dplyr)
library(car)
library(lmtest)

ozone <- read_csv("data/LAozone.csv")
ozone_model <- lm(ozone ~ temp + I(doy ^ 2), data = ozone)
df <- data.frame(ozone$temp)
res = residuals(ozone_model)

# lag(df,1) = c(tail(df, -1)), 0)
lag = lag(res, 1)

lag_df = data.frame(x = res, y = lag)

ggplot(lag_df, aes(y, x)) + geom_point()

acf(res, plot = TRUE, main = "temp autocor")


# perform Durbin-Watson test
durbinWatsonTest(ozone_model)

# calcute by myself without lib call
# calculate for i: 2 -> n
# (e_t^2 + e_{t-1}^2 - 2 * e_t*e_{t-1}) / e_t^2
et = 0.0
etm1 = 0.0
etetm1 = 0.0
for (i in 2:length(res)) {
  et = et + res[i] * res[i]
  etm1 = etm1 + res[i - 1] * res[i - 1]
  etetm1 = etetm1 + 2 * (res[i] * res[i - 1])
}

dwt = (et + etm1 - etetm1) / et
print(dwt)

SS_res <- sum(lag_df$x ^ 2)
(r1 <- sum(lag_df$x[-1] * lag_df$y[-1]) / SS_res)
(DW_exact <- sum((lag_df$x[-1] - lag_df$y[-1]) ^ 2) / SS_res)
(DW_approx <- 2 * (1 - r1))

dwtest(ozone_model, alternative = "two.sided")
