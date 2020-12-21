library(tidyverse)
wood <- read_csv("data/woodstrength.csv")

# check if linear fit is good
ggplot(wood, aes(x = Conc, y = Strength)) +
    geom_point() +
    geom_smooth(method = "lm") +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), color = "red") +
    geom_smooth(method = "lm", formula = y ~ poly(x, 3, raw = TRUE), color = "green") +
    geom_smooth(method = "lm", formula = y ~ poly(x, 4, raw = TRUE), color = "magenta") +
    geom_smooth(method = "lm", formula = y ~ poly(x, 5, raw = TRUE), color = "yellow") +
    labs(x = "hardwood concentration", y = "tensile strength")

wood_models <- list(lm(Strength ~ Conc, data = wood),
    lm(Strength ~ poly(Conc, degree = 2, raw = TRUE), data = wood),
    lm(Strength ~ poly(Conc, degree = 3, raw = TRUE), data = wood),
    lm(Strength ~ poly(Conc, degree = 4, raw = TRUE), data = wood),
    lm(Strength ~ poly(Conc, degree = 5, raw = TRUE), data = wood))

# obtainable in one line
wood_models <- map(1:5, function(d) lm(Strength ~ poly(Conc, degree = d, raw = TRUE), data = wood))

SSres <- map_dbl(wood_models, function(x) sum(residuals(x) ^ 2))

wood_models_orth <- list(lm(Strength ~ Conc, data = wood),
    lm(Strength ~ poly(Conc, degree = 2, raw = FALSE), data = wood),
    lm(Strength ~ poly(Conc, degree = 3, raw = FALSE), data = wood),
    lm(Strength ~ poly(Conc, degree = 4, raw = FALSE), data = wood),
    lm(Strength ~ poly(Conc, degree = 5, raw = FALSE), data = wood))

SSres_d <- data.frame(SSres, Degree = 1:5)
ggplot(SSres_d, aes(x = Degree, y = SSres)) + geom_col(fill = "blue")
