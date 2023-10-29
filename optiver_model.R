library(data.table)
library(lightgbm)
library(xgboost)

# Function to generate features
generate_features <- function(df) {
  features <- c('seconds_in_bucket', 'imbalance_buy_sell_flag',
                'imbalance_size', 'matched_size', 'bid_size', 'ask_size',
                'reference_price', 'far_price', 'near_price', 'ask_price', 'bid_price', 'wap',
                'imb_s1', 'imb_s2'
  )
  
  df$imb_s1 <- (df$bid_size - df$ask_size) / (df$bid_size + df$ask_size)
  df$imb_s2 <- (df$imbalance_size - df$matched_size) / (df$matched_size + df$imbalance_size)
  
  prices <- c('reference_price', 'far_price', 'near_price', 'ask_price', 'bid_price', 'wap')
  
  for (i in 1:length(prices)) {
    for (j in 1:length(prices)) {
      if (i > j) {
        df[[paste0(prices[i], "_", prices[j], "_imb")]] <- (df[[prices[i]]] - df[[prices[j]]]) / (df[[prices[i]]] + df[[prices[j]]])
        features <- c(features, paste0(prices[i], "_", prices[j], "_imb"))
      }
    }
  }
  
  for (i in 1:length(prices)) {
    for (j in 1:length(prices)) {
      for (k in 1:length(prices)) {
        if (i > j && j > k) {
          max_ <- pmax(df[[prices[i]]], df[[prices[j]]], df[[prices[k]]])
          min_ <- pmin(df[[prices[i]]], df[[prices[j]]], df[[prices[k]]])
          mid_ <- rowSums(df[, c(prices[i], prices[j], prices[k])] - min_ - max_)

df[[paste0(prices[i], "_", prices[j], "_", prices[k], "_imb2")]] <- (max_ - mid_) / (mid_ - min_)
features <- c(features, paste0(prices[i], "_", prices[j], "_", prices[k], "_imb2"))
        }
      }
    }
  }
  
  return(df[, features, with = FALSE])
}

TRAINING <- TRUE

if (TRAINING) {
  df_train <- read_csv("/Users/bigmac/Desktop/MDA woohoo/projet ML prévision/optiver-trading-at-the-close/train.csv", col_names =TRUE)
  df_ <- generate_features(df_train)
}

model_path <- '/kaggle/input/optiverbaselinezyz'

N_fold <- 5

models <- list()

if (TRAINING) {
  X <- as.matrix(df_)  # Assuming df_ is a data frame
  Y <- df_train$target
  
  not_na <- !is.na(Y)
  X <- X[not_na, ]
  Y <- Y[not_na]
  
  index <- 1:length(X)  # Create an index vector
  
  # Now you can use X, Y, and index in the rest of your code
}
# Function to train models
nrow(X)
index_train <- 1:as.integer(nrow(X)*0.8)
index_val <- as.integer(nrow(X)*0.8):nrow(X)

model_lgbm <- lgb.train(objective = 'regression_l1', data = lgb.Dataset(data = X[index_train,], label = Y[index_train]),
                   valids = list(val_data = lgb.Dataset(data = X[index_val,], label = Y[index_val])),
                   nrounds = 500,
                   early_stopping_rounds = 100,
                   verbose = 10)



model_xgb <- xgboost(data = X, label = Y, objective = 'reg:absoluteerror', nrounds=500)

# 
# train <- function(model_dict, modelname) {
#   if (TRAINING) {
#     model <- model_dict[[modelname]]
#     model <- lgb.train(objective = 'regression_l1', data = lgb.Dataset(data = X[index %% N_fold != i, ], label = Y[index %% N_fold != i]),
#                        valids = list(val_data = lgb.Dataset(data = X[index %% N_fold == i, ], label = Y[index %% N_fold == i])),
#                        nrounds = 500,
#                        early_stopping_rounds = 100,
#                        verbose = 10)
#     models[[modelname]] <<- model
#     saveRDS(model, paste0('./models/', modelname, '_', i, '.rds'))
#   } else {
#     models[[modelname]] <<- readRDS(paste0(model_path, '/', modelname, '_', i, '.rds'))
#   }
# }
# 
# model_dict <- list(
#   'lgb' = lgb.train(objective = 'regression_l1', nrounds = 500, data=lgb.Dataset(X, Y)),
#   'xgb' = xgboost(data = X, label = Y, objective = 'reg:absoluteerror'),
# )
# 
# for (i in 1:N_fold) {
#   train(model_dict, 'lgb')
# }

# Define a function to generate predictions using the trained models
# generate_predictions <- function(test, models) {
#   feat <- generate_features(test)
#   predictions <- sapply(models, function(model) {
#     predict(model, feat)
#   })
#   return(rowMeans(predictions))
# }
# 
# # Assuming you have an environment and data loading functions from your optiver2023 package
# # Use the generate_predictions function to make predictions
# env <- make_env()
# iter_test <- env$iter_test()
# 
# counter <- 0
# 
# while (TRUE) {
#   list_data <- iter_test$next_batch()
#   if (is.null(list_data)) break
#   
#   test <- list_data$data
#   revealed_targets <- list_data$revealed_targets
#   sample_prediction <- list_data$sample_prediction
#   
#   sample_prediction$target <- generate_predictions(test, models)
#   
#   env$predict(sample_prediction)
#   
#   counter <- counter + 1
# }
