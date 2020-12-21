library(tidyverse)
nitrate <- read_csv("data/nitrate.csv")
print(nitrate)
day1 <- nitrate[nitrate$Day == "Day 1",]
day1 <- day1[c("Nitrate", "Light")]
day2 <- nitrate[nitrate$Day == "Day 2",]
day2 <- day2[c("Nitrate", "Light")]
print(day1)
print(day2)

beta1 <- 21000
beta2 <- 40

ggplot(nitrate, aes(x = Light, y = Nitrate, color = Day)) +
geom_point() +
labs(x = "Light Intensity", "Nitrate utilization") +
  geom_smooth(method = "nls",
              method.args = list(formula = y ~ b1 * x / (b2 + x),
                                 start = list(b1 = beta1, b2 = beta2)),
              data = nitrate,
              se = FALSE)


m1 <- nls(Nitrate ~ b1 * Light / (b2 + Light), start = c(b1 = beta1, b2 = beta2), control = list(maxiter = 50), data = day1)

m2 <- nls(Nitrate ~ b1 * Light / (b2 + Light), start = c(b1 = beta1, b2 = beta2), control = list(maxiter = 50), data = day2)

colors <- c("Day 1" = "red", "Day 2" = "blue", "X" = "green")
p <- ggplot(nitrate, aes(x = Light, y = Nitrate)) +
geom_point() +
labs(x = "Light Intensity", "Nitrate utilization") +
geom_smooth(method = "nls",
              method.args = list(formula = y ~ b1 * x / (b2 + x),
                                 start = list(b1 = beta1, b2 = beta2)),
              data = day1,
              se = FALSE,
              aes(color = "Day 1")) +
geom_smooth(method = "nls",
              method.args = list(formula = y ~ b1 * x / (b2 + x),
                                 start = list(b1 = beta1, b2 = beta2)),
              data = day2,
              se = FALSE,
              aes(color = "Day 2")) +
              scale_color_manual(values = colors)

print(p)

is_day2 <- function(z) {
  return(ifelse(z == "Day 2", 1, 0))
}

print(is_day2(nitrate$Day))

alpha1 <- -3000
alpha2 <- -13
beta1 <- 21000
beta2 <- 40

m2 <- nls(Nitrate ~ (b1 + alpha1 * is_day2(Day)) * Light / ((b2 + alpha2 * is_day2(Day)) + Light),
    start = c(b1 = beta1, b2 = beta2),
    control = list(maxiter = 50), data = nitrate)

print("===")
p <- ggplot(nitrate, aes(x = Light, y = Nitrate, z = Day)) +
geom_point() +
labs(x = "Light Intensity", "Nitrate utilization") +
geom_smooth(method = "nls",
                method.args = list(formula = y ~ (b1 + alpha1 * is_day2(z)) * x / ((b2 + alpha2 * is_day2(z)) + x),
                                 start = list(b1 = beta1, b2 = beta2)),
              data = nitrate,
              se = FALSE,
              aes(color = "X")) +
              scale_color_manual(values = colors)
print(p)

warnings()