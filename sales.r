library(tidyverse)
sales <- read_csv("data/sales.csv")

GGally::ggpairs(sales)

model_sales <- lm(Y ~ ., data = sales)
model_full <- lm(Y ~ ., data = sales)
model_res <- lm(Y ~ . - X4, data = sales)

library(ggplot2)
library(ggfortify)
autoplot(model_sales)

summary(model_sales)

model_sales_12 <- lm(Y ~ X1 + X2, data = sales)

SS_full_resid <- anova(model_sales)["Residuals", "Sum Sq"]
SS_res_resid <- anova(model_sales_12)["Residuals", "Sum Sq"]
(F_ratio <- ((SS_res_resid - SS_full_resid) / 2) / (SS_full_resid / (15 - 4 - 1)))

pf(F_ratio, df1 = 2, df2 = 15 - 4 - 1, lower.tail = FALSE)