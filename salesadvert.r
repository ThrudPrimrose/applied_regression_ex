library(tidyverse)
library(ggfortify)

sales_data <- read_csv("data/salesadvert.csv")
print(sales_data)

ggplot(sales_data, aes(x = Month)) +
geom_line(aes(y = Adver), color = "red") +
geom_line(aes(y = Sales), color = "blue") +
labs(x = "Month", "Expenditures")

lm_sales <- lm(Sales ~ Adver, data = sales_data)
summary(lm_sales)

acf_sales <- acf(residuals(lm_sales), plot = FALSE)
autoplot(acf_sales)

res_sales <- residuals(lm_sales)
(D <- sum(diff(res_sales) ^ 2) / sum(res_sales ^ 2))

lmtest::dwtest(lm_sales, alternative = "two.sided")

ar_sales <- arima(x = sales_data$Sales, xreg = sales_data$Adver, order = c(1, 0, 0))

acf_ar_sales <- acf(residuals(ar_sales), plot = FALSE)
autoplot(acf_ar_sales)