library(catdata)
library(tidyverse)
data(heart)

heart <- as.data.frame(heart)

table(y = heart$y, famhist = heart$famhist)

#both variables are binary it does not make sense to plot them
#ggplot(data = heart, mapping = aes(x = famhist, y = y)) +
# geom_point()

#logistic distribution -> family = binomial
max_fit <- glm(formula = y ~ ., family = "binomial", data = heart)

min_fit_famhist <- glm(formula = y ~ famhist, family = "binomial", data = heart)

min_fit_obesity <- glm(formula = y ~ obesity, family = "binomial", data = heart)

#use maximal model go backwards
stepAIC(max_fit, direction = "backward")

stepAIC(min_fit_famhist, direction = "forward")

stepAIC(min_fit_obesity, direction = "forward")