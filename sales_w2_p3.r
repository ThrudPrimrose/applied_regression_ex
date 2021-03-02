library(tidyverse)
library(ggplot2)
library(ggfortify)
sales <- read_csv("data/sales.csv")

GGally::ggpairs(sales)

model_all <- lm(Y ~ ., data = sales)

autoplot(model_all)
summary(model_all)
anova(model_all)

test_linht <- function(model_res, model_full, rank_a=1, df_full = 10)
{
    SS_full_resid <- anova(model_full)["Residuals", "Sum Sq"]
    SS_res_resid <- anova(model_res)["Residuals", "Sum Sq"]
    F_ratio <- ((SS_res_resid - SS_full_resid)/rank_a) / (SS_full_resid/(df_full))
    
    df_f <- anova(model_full)["Residuals", "Df"]
    df_r <- anova(model_res)["Residuals", "Df"]
    print(df_r)
    print(df_f)
    rank <- df_r - df_f;
    print(rank)

    print(F_ratio)
    prob <- pf(F_ratio, df1 = rank_a, df2 = df_full, lower.tail=FALSE)
    print(prob)
}

#b4=0
test_linht(lm(Y ~ X1 + X2 + X3, data=sales), model_all, rank_a=1)

#b3=b4 = 0
test_linht(lm(Y ~ X1 + X2, data=sales), model_all, rank_a=2)

#b2 = b3 | b2 - b3 = 0
test_linht(lm(Y ~ X1 + I(X2 + X3) + X4, data=sales), model_all, rank_a=1)

#b1=b2=b3=b4 = 0
#test_linht(lm(Y ~ ., data=sales), model_all, rank_a=4)
#need to do normal f test
f_val <- (sum(anova(model_all)[1:4, "Sum Sq"]) / 4) / (anova(model_all)["Residuals", "Sum Sq"] /10)
print("f_val prob:")
print(f_val)
pf(f_val,df1=4,df2=10,lower.tail = FALSE)

model_nox4 <- lm(Y~ . - X4, data=sales)
summary(model_nox4)