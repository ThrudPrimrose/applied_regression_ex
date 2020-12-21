# load tidyverse
library(tidyverse)
it_costs <- read_csv("data/it_costs.csv")

cat("", cor(it_costs$real_costs, it_costs$pred_costs), "\n",
    cor(it_costs$real_costs, log(it_costs$pred_costs)), "\n",
    cor(log(it_costs$real_costs), it_costs$pred_costs), "\n",
    cor(log(it_costs$real_costs), log(it_costs$pred_costs)))

model_costs <- lm(log(real_costs) ~ pred_costs, data = it_costs)
summary(model_costs)
anova(model_costs)