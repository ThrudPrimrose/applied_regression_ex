library(tidyverse)
library(class)

#Plot digits functions taken directly from the exercise sheet
plot_digits <- function(data, rowids) {
  transformed_data <- data[rowids,] %>%
  mutate(digit_id = row_number()) %>%
  group_by(digit_id, digit) %>%
  nest() %>%
  mutate(matrix = map(data, ~ t(matrix(as.numeric(.x), 16, 16, byrow = TRUE))[, 16:1] %>%
        data.frame() %>%
        mutate(x = row_number()) %>%
        gather(y, color, - x))) %>%
  ungroup() %>%
  unnest(cols = c(matrix)) %>%
  mutate(y = parse_number(y))

  digit_labels <- as.character(transformed_data$digit)
  names(digit_labels) <- transformed_data$digit_id

  ggplot(transformed_data, aes(x, y, fill = color)) +
     geom_raster(show.legend = FALSE) +
     scale_fill_gradient(low = "white", high = "black") +
     facet_wrap(~digit_id, labeller = as_labeller(digit_labels)) +
     coord_equal() +
     theme_minimal() +
     theme(plot.background = element_blank(), panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
}

#Percentage of missclassified numbers
missclass_error <- function(pred_val, true_val) {
  100 * sum(true_val != pred_val) / length(true_val)
}


ztest <- read_csv("data/zipcode_test.csv")
ztrain <- read_csv("data/zipcode_train.csv")

ztrain <- ztrain[ztrain$digit %in% 2:3,]
ztest <- ztest[ztest$digit %in% 2:3,]

#Print first couple of rows
head(ztrain[, 1:10])

#Plot the guessed digits
plot_digits(ztest, 1:10)
plot_digits(ztrain, 1:10)

#Linear reg with all covariates
lmit <- lm(digit ~ ., data = ztrain)

#Predict data with the linear model on train and test dataset
lmtrainpred <- predict(lmit)
lmtestpred <- predict(lmit, newdata = ztest)

#Map all approximations >2.5 to 3 and others 2
lm_train <- ifelse(lmtrainpred > 2.5, 3, 2)
lm_test <- ifelse(lmtestpred > 2.5, 3, 2)

lm_trainerr <- missclass_error(lm_train, ztrain$digit)
print(lm_trainerr)
lm_testerr <- missclass_error(lm_test, ztest$digit)
print(lm_testerr)

glm_zip <- glm(factor(digit) ~ ., data = ztrain, family = "binomial")

glm_trainpred <- predict(glm_zip, type = "response")
glm_testpred <- predict(glm_zip, newdata = ztest, type = "response")
glm_train <- ifelse(glm_trainpred > 0.5, 3, 2)
glm_test <- ifelse(glm_testpred > 0.5, 3, 2)

glm_trainerror <- missclass_error(glm_train, ztrain$digit)
print(glm_trainerror)
glm_testerror <- missclass_error(glm_test, ztest$digit)
print(glm_testerror)

#which returns the indices where the predicate is true
lm_missclass_train <- which(lm_train != ztrain$digit)
lm_missclass_test <- which(lm_test != ztest$digit)

plot_digits(ztrain, lm_missclass_train)
plot_digits(ztest, lm_missclass_test)

#now with machine learning
k <- c(1, 3, 5, 7, 15)


knn_train_pred <- lapply(k, function(kk)
  knn(train = ztrain[, -1], test = ztrain[, -1], cl = ztrain$digit, k = kk, prob = TRUE))

knn_test_pred <- lapply(k, function(kk)
  knn(train = ztest[, -1], test = ztest[, -1], cl = ztest$digit, k = kk, prob = TRUE))


print(knn_train_pred)

ztrain_digit <- factor(ztrain$digit, levels = 2:3)
ztest_digit <- factor(ztest$digit, levels = 2:3)

knn_train_digit <- factor(ztrain$digit, levels = 2:3)
knn_test_digit <- factor(ztest$digit, levels = 2:3)

knn_train_error <- sapply(knn_train_pred, function(x) missclass_error(x, ztrain_digit))
knn_test_error <- sapply(knn_test_pred, function(x) missclass_error(x, ztest_digit))

knn_train_missclass <- which(knn_train_pred[[2]] != ztrain$digit)
knn_test_missclass <- which(knn_test_pred[[2]] != ztest$digit)

head(knn_train_pred[[3]])
head(attr(knn_train_pred[[3]], "prob"))

round(100 * attr(knn_train_pred[[2]], "prob")[knn_train_missclass], 3)

round(100 * attr(knn_test_pred[[2]], "prob")[knn_test_missclass], 3)

plot_digits(ztrain, knn_train_missclass)
plot_digits(ztest, knn_test_missclass)