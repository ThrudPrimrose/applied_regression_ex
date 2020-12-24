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
p <- ggplot(nitrate, aes(x = Light, y = Nitrate, colour = Day)) +
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


michaelis_menton <- function(x, beta_1, beta_2) { beta_1 * x / (beta_2 + x) }

(mime <- nls(Nitrate ~ beta1 * Light / (beta2 + Light),
  start = list(beta1 = 21000, beta2 = 40),
  data = nitrate))

p <- ggplot(nitrate) +
geom_point(aes(x = Light, y = Nitrate)) +
stat_function(fun = michaelis_menton,
  args = list(beta_1 = coef(mime)[1], beta_2 = coef(mime)[2]),
  colour = "blue", size = 1.3)

plot(p)

is_day2 <- function(z) {
  return(ifelse(z == "Day 2", 1, 0))
}

print(is_day2(nitrate$Day))

nitrate$isday2 <- is_day2(nitrate$Day)

alpha1 <- -3000
alpha2 <- -13
beta1 <- 21000
beta2 <- 40

m_with_day <- nls(Nitrate ~ (b1 + alpha1 * isday2) * Light / ((b2 + alpha2 * isday2) + Light),
    start = c(b1 = beta1, b2 = beta2),
    control = list(maxiter = 50), data = nitrate)

print("===")
p <- ggplot(nitrate, aes(x = Light, y = Nitrate, z = isday2)) +
geom_point() +
labs(x = "Light Intensity", "Nitrate utilization") +
geom_smooth(method = "nls",
                method.args = list(formula = y ~ (b1 + alpha1 * z) * x / ((b2 + alpha2 * z) + x),
                                 start = list(b1 = beta1, b2 = beta2)),
              data = nitrate,
              se = FALSE,
              aes(color = "X")) +
              scale_color_manual(values = colors)
print(p)

warnings()

# model fit test

model_unres <- lm(Nitrate ~ factor(Light), data = nitrate)
anova(mime, model_unres)

anova(mime, m1)

anova(mime, m2)

# H_0 = a1 = a2 = 0 (the days)
anova(mime, m_with_day)

# out put of anova(mime, m_with_day)
#Model 1: Nitrate ~ beta1 * Light/(beta2 + Light)
#Model 2: Nitrate ~ (b1 + alpha1 * isday2) * Light/((b2 + alpha2 * isday2) + Light)
#  Res.Df Res.Sum Sq Df Sum Sq F value Pr(>F)
#1     46   96536196                         
#2     46  101519079  0      0    

#p value >0.05 do not reject null hypothesis
