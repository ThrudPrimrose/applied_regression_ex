library(tidyverse)
sales_data <- read_csv("data/salesadvert.csv")
print(sales_data)

ggplot(sales_data, aes(x = Month)) +
geom_line(aes(y = Adver), color = "red") +
geom_line(aes(y = Sales), color = "blue") +
labs(x = "Month", "Expenditures")