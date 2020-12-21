library(tidyverse)
library(GGally)

crime <- read_csv("data/crime.csv")

crime_model <- lm(crime ~ pctmetro + pctwhite + pcths + poverty + single, data = crime)
# or 
# model_crime <-lm(crime~.-state, data = crime))

lev <- influence(crime_model)$hat

cd <- cooks.distance(crime_model)

studres <- rstudent(crime_model)

# the stundentized residuals in the lecture
print(summary(crime_model))
print("===================================")
print(summary(crime_model)$sigma)

studres_crime <- residuals(crime_model) / (summary(crime_model)$sigma * sqrt(1 - lev))

# outlier means > 2 times the mean leverage
crime$lev_outlier <- lev > 2 * mean(lev)
# cook's distance outlier when cook's distance for resid e is > 0.5
crime$cookd_outlier <- cd > 0.5
# studentized residual > 2 or 3 means model does not fit it well
crime$studres_outlier <- abs(studres) > 2.0

which(crime$lev_outlier)
which(crime$cookd_outlier)
which(crime$studres_outlier)

ggpairs(crime, columns = 3:7, # select appropriate columns
    mapping = aes(color = lev_outlier), # Add color asthetics to specify outliers
    legend = 1, # Takes the legend of the first plot 
    diag = list(continuous = wrap("densityDiag", alpha = 0.5)) # Transparent density plots on the diagonal
)

ggplot(crime, aes(x = pctwhite, y = crime)) +
    geom_point(aes(colour = studres_outlier)) +
    geom_text(aes(label = ifelse(studres_outlier, state, "")), vjust = 1) # defines the vertical position of the labels

