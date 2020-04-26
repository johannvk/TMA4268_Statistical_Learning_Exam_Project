# Task 1:
library(ISLR)
library(keras)
library(ggplot2)

set.seed(1)
College$Private = as.numeric(College$Private)
train.ind = sample(1:nrow(College), 0.5 * nrow(College))
college.train = College[train.ind, ]
college.test = College[-train.ind, ]
# summary(College)
# str(College)
# response is "Outstate".

# a) Normalize the predictors:
predict_cols = names(College)[-9]
n_pred = length(predict_cols)

college.train_predictors = scale(college.train[ , predict_cols], scale=T)  # Mean: 0, sd: 1.
col_means_train <- attr(college.train_predictors, "scaled:center") 
col_stddevs_train <- attr(college.train_predictors, "scaled:scale")

# Use means and standard deviations from training set to normalize test set
college.test_predictors  = scale(college.test[ , predict_cols], center=col_means_train, scale=col_stddevs_train)  

# str(college.train)
# summary(college.train)
# test_data <- scale(test_data, center = col_means_train, scale = col_stddevs_train)

# b) 
#LaTeX

# c) Train a neural network: 
# Prepare training data:
y_train = data.matrix(college.train[, "Outstate"])
x_train = data.matrix(college.train_predictors)

y_test = data.matrix(college.test[, "Outstate"])
x_test = data.matrix(college.test_predictors)

nn_model = keras_model_sequential()
nn_model %>% 
  layer_dense(units=64, activation='relu', input_shape=c(17)) %>%
  layer_dense(units=64, activation='relu') %>%
  layer_dense(units=1,  activation='relu')

summary(nn_model)

nn_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = "rmsprop",
  metrics = c('mae')
)

# Set seed before performing stochastic gradient descent:
set.seed(123)
nn_history = nn_model %>% fit(x_train, y_train, epochs=300, batch_size=8, validation_split=0.2)

nn_test_predictions = nn_model %>% predict(x_test)
nn_mse = sum((y_test - nn_test_predictions)^2)/length(y_test)

# Plot development in mae over epochs for training and validation test set:
plot(nn_history, metrics =c("mae", "val_mae"), smooth = F) + ggtitle("Fitted model")

# MSE on Outstate from College using subset-selection and BIC model choice: 3.401438 × 10^6.
# MSE on Outstate from College using lasso method, with cross-validated choice for lambda: 3.7449967 × 10^6.
# MSE on Outstate from College using nn, (64, 64)-model: 3.660810 x 10^6


# d) Use regularization techniques on the nn to achieve better performance:

nn_dropout_model = keras_model_sequential()
nn_dropout_model %>% 
  layer_dense(units=64, activation='relu', input_shape=c(17)) %>%
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units=64, activation='relu') %>%
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units=1,  activation='relu')

# summary(nn_dropout_model)

nn_dropout_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = "rmsprop",
  metrics = c('mae')
)

# Set seed before performing stochastic gradient descent:
set.seed(123)
nn_dropout_history = nn_dropout_model %>% fit(x_train, y_train, epochs=300, batch_size=8, validation_split=0.2)

# Plot development in mae over epochs for training and validation test set:
plot(nn_dropout_history, metrics =c("mae", "val_mae"), smooth = F) + ggtitle("Fitted model")

# nn_model %>% evaluate(x_test, y_test, verbose=0)
nn_dropout_test_predictions = nn_dropout_model %>% predict(x_test)
nn_dropout_mse = sum((y_test - nn_dropout_test_predictions)^2)/length(y_test)



