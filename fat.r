library(tidyverse)

# read bodyfat csv
fat <- read_csv("data/bodyfat.csv")

# linear regression
fat_lm <- lm(Fat ~ Age + Gend, data = fat)

# fit body fat
fat$fitted <- fitted.values(fat_lm)

# plot data
# ggplot(fat, aes(x = Age, y = Fat, color = Gend, shape = Gend)) + geom_point()

ggplot(fat, aes(x = Age, y = Fat)) +
    geom_point(aes(color = Gend, shape = Gend)) +
    scale_color_manual(name = "Gender", breaks = c("female", "male"), values = c("red", "blue")) +
    scale_shape_discrete(name = "Gender") +
    geom_line(aes(x = Age, y = fitted, color = Gend), size = 1)

summary(fat_lm)

fat_lm2 <- lm(Fat ~ Age * Gend, data = fat)
fat$fitted2 <- fat_lm2$fitted.values

ggplot(fat, aes(x = Age, y = Fat)) + geom_point(aes(color = Gend, shape = Gend)) +
    scale_color_manual(name = "Gender", breaks = c("female", "male"), values = c("red", "blue")) +
    scale_shape_discrete(name = "Gender") +
    geom_line(aes(x = Age, y = fitted2, color = Gend), size = 1)

summary(fat_lm2)