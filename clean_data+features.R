
rm(list=objects())
library(parallel)

library(rpart)
library(magrittr)
library(party)
library(yarrr)
library(tree)
library(rpart.plot)
library(progress)
library(mgcv)
library(mgcViz)
library(dygraphs)
library(xts)
library(tidyverse)
library(viking)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(psych)
library(reshape2)
library(gridExtra)
library(corrplot)
library(Metrics)
library(xgboost)
library(lightgbm)
library(data.table)
library(caret)
library(zoo)
library(glmnet)
library(forecast)


data_all<-read_csv("/Users/dhia-elhaqouerfelli/Desktop/lejeune/projet prevision/optiver/train.csv", col_names =TRUE)


# 1. Supprimer les lignes avec des valeurs NA dans 'target'
dfclean <- data_all %>% filter(!is.na(target))

# 2. Imputer les valeurs pour 'far_price' et 'near_price' pour les secondes inférieures à 300
#dfclean <- dfclean %>%
#  mutate(far_price = ifelse(seconds_in_bucket < 300 & is.na(far_price), 0, far_price),
#         near_price = ifelse(seconds_in_bucket < 300 & is.na(near_price), 0, near_price))

# 2.0 
replace_by_cond <- function(data) {
  # Identifier les lignes où `far_price` est NA
  cond <- is.na(data$far_price)
  
  # Remplacer `far_price` en fonction de la condition
  data$far_price <- ifelse(cond, 
                           ifelse(data$imbalance_buy_sell_flag == 1, data$ask_price, 
                                  ifelse(data$imbalance_buy_sell_flag == -1, data$bid_price, 
                                         (data$bid_price + data$ask_price)/2)), 
                           data$far_price)
  
  # Remplacer `near_price` en fonction de la condition
  data$near_price <- ifelse(cond, 
                            ifelse(data$imbalance_buy_sell_flag == 1, data$ask_price, 
                                   ifelse(data$imbalance_buy_sell_flag == -1, data$bid_price, 
                                          (data$bid_price + data$ask_price)/2)), 
                            data$near_price)
  
  # Ajuster `reference_price` en fonction de la condition
  data$reference_price <- ifelse(cond, 
                                 ifelse((data$near_price > data$bid_price) & (data$near_price < data$ask_price), 
                                        data$near_price, 
                                        ifelse(data$near_price > data$ask_price, data$ask_price, data$bid_price)),
                                 data$reference_price)
  
  return(data)
}

cond <- function(data) {
  # Copie des données pour éviter de modifier l'ensemble de données original
  raw <- data
  
  # Suppression des enregistrements où 'wap' est NA
  rmv <- raw[!is.na(raw$wap), ]
  
  # Application de la fonction de remplacement conditionnel
  cond <- replace_by_cond(rmv)
  
  return(cond)
}
# Application de la fonction `cond` à l'ensemble de données `train`
# Assurez-vous que l'ensemble de données `train` a été correctement chargé et préparé en R
dfclean=cond(data_all)





n_stocks = 200

data_all_bis = copy(dfclean)

task_variances <- data_all_bis %>%
  group_by(stock_id) %>%
  summarise(variance = var(target, na.rm = FALSE)) %>%
  arrange(desc(variance))

# Select the top n_stocks tasks with the highest variance
top_n_tasks <- head(task_variances, n_stocks)
top_stocks = c(top_n_tasks$stock_id)

data_n <-subset(data_all, stock_id %in% top_stocks)
data_n[is.na(data_n)] <- NaN


ordinary <- c('imbalance_buy_sell_flag')
size <- c('bid_size', 'ask_size', 'imbalance_size', 'matched_size')
price1 <- c('bid_price', 'ask_price', 'wap', 'reference_price')
price2 <- c('far_price', 'near_price')
label <- 'target'


data_n <- select(data_n, time_id, stock_id, date_id, seconds_in_bucket, all_of(c(ordinary, size, price1, price2, label)))

data_n <- arrange(data_n, stock_id)

df_infos = data.frame(
  Characteristics = c("Number of features", "Number of stocks", "Number of days in the study", "Number of data points each day", "Time increment (seconds)", "Number of total data points per stocks"), 
  Value = c(as.integer(length(colnames(data_n))), as.integer(length(unique(data_n$stock_id))), as.integer(length(unique(data_n$date_id))), as.integer(length(unique(data_n$time_id))/length(unique(data_n$date_id))), as.integer(10), as.integer(length(unique(data_n$time_id)))))

df_infos




global_stock_id_feats <- list(
  median_size = data_n %>%
    group_by(stock_id) %>%
    summarise(median_bid_size = median(bid_size, na.rm = TRUE),
              median_ask_size = median(ask_size, na.rm = TRUE)) %>%
    transmute(stock_id, median_size = median_bid_size + median_ask_size) %>%
    ungroup() %>%
    select(-stock_id),
  
  std_size = data_n %>%
    group_by(stock_id) %>%
    summarise(std_bid_size = sd(bid_size, na.rm = TRUE),
              std_ask_size = sd(ask_size, na.rm = TRUE)) %>%
    transmute(stock_id, std_size = std_bid_size + std_ask_size) %>%
    ungroup() %>%
    select(-stock_id),
  
  ptp_size = data_n %>%
    group_by(stock_id) %>%
    summarise(ptp_bid_size = max(bid_size, na.rm = TRUE) - min(bid_size, na.rm = TRUE)) %>%
    ungroup() %>%
    rename(ptp_size = ptp_bid_size),
  
  median_price = data_n %>%
    group_by(stock_id) %>%
    summarise(median_bid_price = median(bid_price, na.rm = TRUE),
              median_ask_price = median(ask_price, na.rm = TRUE)) %>%
    transmute(stock_id, median_price = median_bid_price + median_ask_price) %>%
    ungroup() %>%
    select(-stock_id),
  
  std_price = data_n %>%
    group_by(stock_id) %>%
    summarise(std_bid_price = sd(bid_price, na.rm = TRUE),
              std_ask_price = sd(ask_price, na.rm = TRUE)) %>%
    transmute(stock_id, std_price = std_bid_price + std_ask_price) %>%
    ungroup() %>%
    select(-stock_id),
  
  ptp_price = data_n %>%
    group_by(stock_id) %>%
    summarise(ptp_bid_price = max(bid_price, na.rm = TRUE) - min(ask_price, na.rm = TRUE)) %>%
    ungroup() %>%
    rename(ptp_price = ptp_bid_price)
)


weights_vector = c(
  0.004, 0.001, 0.002, 0.006, 0.004, 0.004, 0.002, 0.006, 0.006, 0.002, 0.002, 0.008,
  0.006, 0.002, 0.008, 0.006, 0.002, 0.006, 0.004, 0.002, 0.004, 0.001, 0.006, 0.004,
  0.002, 0.002, 0.004, 0.002, 0.004, 0.004, 0.001, 0.001, 0.002, 0.002, 0.006, 0.004,
  0.004, 0.004, 0.006, 0.002, 0.002, 0.04 , 0.002, 0.002, 0.004, 0.04 , 0.002, 0.001,
  0.006, 0.004, 0.004, 0.006, 0.001, 0.004, 0.004, 0.002, 0.006, 0.004, 0.006, 0.004,
  0.006, 0.004, 0.002, 0.001, 0.002, 0.004, 0.002, 0.008, 0.004, 0.004, 0.002, 0.004,
  0.006, 0.002, 0.004, 0.004, 0.002, 0.004, 0.004, 0.004, 0.001, 0.002, 0.002, 0.008,
  0.02 , 0.004, 0.006, 0.002, 0.02 , 0.002, 0.002, 0.006, 0.004, 0.002, 0.001, 0.02,
  0.006, 0.001, 0.002, 0.004, 0.001, 0.002, 0.006, 0.006, 0.004, 0.006, 0.001, 0.002,
  0.004, 0.006, 0.006, 0.001, 0.04 , 0.006, 0.002, 0.004, 0.002, 0.002, 0.006, 0.002,
  0.002, 0.004, 0.006, 0.006, 0.002, 0.002, 0.008, 0.006, 0.004, 0.002, 0.006, 0.002,
  0.004, 0.006, 0.002, 0.004, 0.001, 0.004, 0.002, 0.004, 0.008, 0.006, 0.008, 0.002,
  0.004, 0.002, 0.001, 0.004, 0.004, 0.004, 0.006, 0.008, 0.004, 0.001, 0.001, 0.002,
  0.006, 0.004, 0.001, 0.002, 0.006, 0.004, 0.006, 0.008, 0.002, 0.002, 0.004, 0.002,
  0.04 , 0.002, 0.002, 0.004, 0.002, 0.002, 0.006, 0.02 , 0.004, 0.002, 0.006, 0.02,
  0.001, 0.002, 0.006, 0.004, 0.006, 0.004, 0.004, 0.004, 0.004, 0.002, 0.004, 0.04,
  0.002, 0.008, 0.002, 0.004, 0.001, 0.004, 0.006, 0.004
)

weights <- setNames(as.list(weights_vector), seq_along(weights_vector) - 1)




compute_rolling_averages <- function(df, window_sizes) {
  # Assurez-vous que df est une matrice ou un data frame avec des valeurs numériques
  df_values <- as.matrix(df)
  num_features <- ncol(df_values)
  
  # Création d'une liste pour stocker les résultats
  rolling_features <- vector("list", length(window_sizes))
  names(rolling_features) <- paste("Window", window_sizes)
  
  # Calcul parallèle des moyennes mobiles pour chaque taille de fenêtre
  cl <- makeCluster(detectCores() - 1) # Utiliser un cœur de moins que le total disponible
  clusterExport(cl, varlist = c("df_values", "window_sizes"), envir = environment())
  clusterEvalQ(cl, library(zoo))
  
  rolling_features <- parLapply(cl, window_sizes, function(window) {
    # Initialisation d'une matrice pour stocker les moyennes mobiles pour cette taille de fenêtre
    result_matrix <- matrix(NA, nrow = nrow(df_values), ncol = num_features)
    
    for (feature_idx in 1:num_features) {
      # Calcul de la moyenne mobile pour chaque caractéristique
      result_matrix[, feature_idx] <- rollapply(df_values[, feature_idx], window, mean, fill = mean(df_values[[feature_idx]]), align = "right")
    }
    
    return(result_matrix)
  })
  
  stopCluster(cl)
  
  return(rolling_features)
}

compute_rolling_averages_df <- function(df){
  # Itération sur chaque colonne de prix et assignation des résultats
  prices <- c("reference_price", "far_price", "near_price", "ask_price", "bid_price", "wap")
  window_sizes <- c(10,5) # Exemple de tailles de fenêtre
  
  for (price in prices) {
    rolling_avg_features <- compute_rolling_averages(df[[price]], window_sizes)
    
    # Assignation des résultats des moyennes mobiles au DataFrame
    for (i in seq_along(window_sizes)) {
      window <- window_sizes[i]
      column_name <- paste(price, "rolling_avg", window, sep = "_")
      df[[column_name]] <- rolling_avg_features[[i]]
    }
  }
  return (df)
}



compute_triplet_imbalance <- function(df, price) {
  comb_indices <- combn(price, 3, simplify = FALSE)
  features_list <- mclapply(comb_indices, function(c) {
    a <- df[[c[1]]]
    b <- df[[c[2]]]
    c <- df[[c[3]]]
    
    max_val <- pmax(a, b, c)
    min_val <- pmin(a, b, c)
    mid_val <- a + b + c - max_val - min_val
    
    imbalance_feature <- ifelse(mid_val == min_val, 0, (max_val - mid_val) / (mid_val - min_val))
    return(imbalance_feature)
  }, mc.cores = detectCores())
  
  features_matrix <- do.call(cbind, features_list)
  colnames(features_matrix) <- sapply(comb_indices, function(c) paste(c, collapse = "_"), USE.NAMES = FALSE)
  return(as.data.frame(features_matrix))
}
imbalance_features <- function(df) {
  prices <- c("reference_price", "far_price", "near_price", "ask_price", "bid_price", "wap")
  sizes <- c("matched_size", "bid_size", "ask_size", "imbalance_size")
  
  df <- df %>%
    mutate(volume = ask_size + bid_size,
           mid_price = (ask_price + bid_price) / 2,
           liquidity_imbalance = (bid_size - ask_size) / (bid_size + ask_size),
           matched_imbalance = (imbalance_size - matched_size) / (matched_size + imbalance_size),
           size_imbalance = bid_size / ask_size)
  
  
  df <- cbind(df, compute_triplet_imbalance(df, prices))
  
  
  return(df)
}



other_features_lgbm <- function(df) {
  
  prices <- c("reference_price", "far_price", "near_price", "ask_price", "bid_price", "wap")
  sizes <- c("matched_size", "bid_size", "ask_size", "imbalance_size")
  
  
  df <- df %>%
    mutate(
      non_auction_size = bid_size + ask_size,
      auction_size = imbalance_size + matched_size,
      size = auction_size + non_auction_size,
      non_auction_size1 = non_auction_size / size,
      auction_size1 = auction_size / size,
      bid_size1 = bid_size / non_auction_size,
      ask_size1 = ask_size / non_auction_size,
      bid_size2 = bid_size / size,
      ask_size2 = ask_size / size,
      imbalance_size1 = imbalance_size / auction_size,
      matched_size1 = matched_size / auction_size,
      imbalance_size2 = imbalance_size / size,
      matched_size2 = matched_size / size,
      non_auction_size_diff = bid_size1 - ask_size1,
      auction_size_diff = matched_size1 - imbalance_size1,
      size_diff = non_auction_size1 - auction_size1
    )
  df <- df %>%
    group_by(stock_id) %>%
    mutate(
      imbalance_momentum = (imbalance_size - lag(imbalance_size, 1)) / matched_size,
      price_spread = ask_price - bid_price,
      spread_intensity = price_spread - lag(price_spread),
      price_pressure = imbalance_size * price_spread,
      market_urgency = price_spread * liquidity_imbalance,
      depth_pressure = (ask_size - bid_size) * (far_price - near_price),
      spread_depth_ratio = price_spread / (bid_size + ask_size),
      mid_price_movement = as.integer(sign(dplyr::lag(mid_price, 5) - mid_price)),
      micro_price = ((bid_price * ask_size) + (ask_price * bid_size)) / (bid_size + ask_size),
      relative_spread = (ask_price - bid_price) / wap)
  
  # Calcul des features statistiques pour tous les prix et tailles
  df$all_prices_mean <- rowMeans(df[, prices], na.rm = TRUE)
  df$all_sizes_mean <- rowMeans(df[, sizes], na.rm = TRUE)
  
  # Autres features calculées similaires à l'exemple Python
  
  
  df<- df %>%
    group_by(stock_id, date_id) %>%
    mutate(
      bid_size_diff_10 = (bid_size / lag(bid_size, 10)) - 1
      # Répétez pour d'autres caractéristiques et délais comme requis
    ) %>%
    ungroup()
  
  price1 <- c('bid_price', 'ask_price', 'reference_price') 
  price2 <- c('far_price', 'near_price') 
  
  for(p in c(price1, price2)) {
    df[[paste0(p, "1")]] <- df[[p]] / df[['wap']]
  }
  df <- df %>%
    mutate(
      dow = date_id %% 5,  # Jour de la semaine
      seconds = seconds_in_bucket %% 60,
      minute = seconds_in_bucket %/% 60,
      time_to_market_close = 540 - seconds_in_bucket
    )
  
  # Assurez-vous que global_stock_id_feats est défini dans votre environnement
  for (key in names(global_stock_id_feats)) {
    df[[paste0("global_", key)]] <- df[["stock_id"]] %in% global_stock_id_feats[[key]]
  }
  
  return(df)
}



other_features_norm <- function(df) {
  
  prices <- c("reference_price", "far_price", "near_price", "ask_price", "bid_price", "wap")
  sizes <- c("matched_size", "bid_size", "ask_size", "imbalance_size")
  
  
  df <- df %>%
    mutate(
      non_auction_size = bid_size + ask_size,
      auction_size = imbalance_size + matched_size,
      size = auction_size + non_auction_size,
      non_auction_size1 = non_auction_size / size,
      auction_size1 = auction_size / size,
      bid_size1 = bid_size / non_auction_size,
      ask_size1 = ask_size / non_auction_size,
      bid_size2 = bid_size / size,
      ask_size2 = ask_size / size,
      imbalance_size1 = imbalance_size / auction_size,
      matched_size1 = matched_size / auction_size,
      imbalance_size2 = imbalance_size / size,
      matched_size2 = matched_size / size,
      non_auction_size_diff = bid_size1 - ask_size1,
      auction_size_diff = matched_size1 - imbalance_size1,
      size_diff = non_auction_size1 - auction_size1
    )
  df <- df %>%
    group_by(stock_id) %>%
    mutate(
      
      price_spread = ask_price - bid_price,
      spread_depth_ratio = price_spread / (bid_size + ask_size),
      micro_price = ((bid_price * ask_size) + (ask_price * bid_size)) / (bid_size + ask_size))
  
  
  # Calcul des features statistiques pour tous les prix et tailles
  df$all_prices_mean <- rowMeans(df[, prices], na.rm = TRUE)
  df$all_sizes_mean <- rowMeans(df[, sizes], na.rm = TRUE)
  
  # Autres features calculées similaires à l'exemple Python
  
  price1 <- c('bid_price', 'ask_price', 'reference_price') 
  price2 <- c('far_price', 'near_price') 
  
  for(p in c(price1, price2)) {
    df[[paste0(p, "1")]] <- df[[p]] / df[['wap']]
  }
  df <- df %>%
    mutate(
      dow = date_id %% 5,  # Jour de la semaine
      seconds = seconds_in_bucket %% 60,
      minute = seconds_in_bucket %/% 60,
      time_to_market_close = 540 - seconds_in_bucket
    )
  
  # Assurez-vous que global_stock_id_feats est défini dans votre environnement
  for (key in names(global_stock_id_feats)) {
    df[[paste0("global_", key)]] <- df[["stock_id"]] %in% global_stock_id_feats[[key]]
  }
  
  return(df)
}

pro_outliers <- function(data_frame) {
  # Créer un dataframe pour stocker les indicateurs d'outliers
  outliers <- data_frame
  
  # Parcourir chaque colonne du dataframe
  for(col_name in names(data_frame)) {
    # Vérifier si la colonne est numérique
    if(is.numeric(data_frame[[col_name]])) {
      # Calculer Q1, Q3 et IQR
      Q1 <- quantile(data_frame[[col_name]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data_frame[[col_name]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      # Calculer les bornes pour déterminer les outliers
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      # Marquer les valeurs en dehors des bornes comme TRUE (outlier)
      outliers[[col_name]] <- data_frame[[col_name]] < lower_bound | data_frame[[col_name]] > upper_bound
    } else {
      # Pour les colonnes non numériques, marquer toutes les valeurs comme FALSE
      outliers[[col_name]] <- FALSE
    }
  }
  
  # Convertir les valeurs logiques en valeurs numériques pour le calcul
  numeric_outliers <- as.data.frame(lapply(outliers, as.integer))
  
  # Ajouter une colonne 'has_outlier' pour marquer les lignes avec au moins un outlier
  data_frame$has_outlier <- apply(numeric_outliers, 1, function(row) any(row == 1))
  
  # Calculer et ajouter le nombre d'outliers par ligne
  data_frame$outlier_count <- rowSums(numeric_outliers == 1, na.rm = TRUE)
  
  # Ajouter une colonne indiquant la position de l'outlier
  data_frame$outlier_position <- apply(numeric_outliers, 1, function(row) {
    paste0(row, collapse = "")
  })
  
  return(data_frame)
}





generate_all_features <- function(df) {
  
  df <- pro_outliers(df)
  df <- compute_rolling_averages_df(df)
  df <- imbalance_features(df)
  df <- other_features_norm(df)
  
  
  return (df)
}


data_n <- generate_all_features(dfclean)
all_features <- setdiff(colnames(data_n), c("row_id", "target", "time_id", "date_id"))

dfclean_feat<- data_n

library(Matrix)

zero_sum <- function(prices, volumes) {
  std_error <- sqrt(volumes)
  step <- sum(prices) / sum(std_error)
  out <- prices - std_error * step
  return(out)
}

pred_boost <- function(pred){
  pred_adjusted <- zero_sum(pred, test_x$bid_size + test_x$ask_size)
  y_min <- -64
  y_max <- 64
  clipped_predictions <- pmin(pmax(pred_adjusted, y_min), y_max)
  return(clipped_predictions)
}


model1_lightgbm <- function(data_frame) {
  # Séparation des données en entraînement et test
  df_train <- subset(data_frame, date_id <= 420)
  df_test <- subset(data_frame, date_id > 420)
  
  # Préparation des données pour LightGBM
  train_y <- df_train$target
  train_x <- as.matrix(df_train[, colnames(df_train) != "target"])
  test_y <- df_test$target
  test_x <- as.matrix(df_test[, colnames(df_test) != "target"])
  
  # Configuration des paramètres de LightGBM
  params <- list(
    objective = "regression_l1",
    metric = "mae",
    num_leaves = 31,
    learning_rate = 0.05,
    n_estimators = 100
  )
  
  # Conversion des données en format LightGBM
  dtrain <- lgb.Dataset(data = train_x, label = train_y)
  dtest <- lgb.Dataset(data = test_x, label = test_y)
  
  # Entraînement du modèle
  lightgbm_model <- lgb.train(
    params = params,
    data = dtrain,
    valids = list(test = dtest),
    nrounds = 100,
    verbose = 0
  )
  
  # Prédiction sur l'ensemble de test
  predictions <- predict(lightgbm_model, test_x)
  
  # Calcul de l'erreur moyenne absolue (MAE)
  mae_errors <- mean(abs(test_y - predictions))
  
  return(list(model = lightgbm_model, mae_errors = mae_errors))
}

result_initial = model1_lightgbm(dfclean)
model_initial = result_initial$model
mae_initial = result_initial$mae_errors
mae_initial
feature_importance_initial <- lgb.importance(model_initial, percentage = TRUE)
feature_importance_initial

dfclean_out=pro_outliers(dfclean)
result_out = model1_lightgbm (dfclean_out)
model_out = result_out$model
mae_out = result_out$mae_errors
mae_out
feature_importance_out <- lgb.importance(model_out, percentage = TRUE)
feature_importance_out

dfclean_rolling=compute_rolling_averages_df(dfclean)
result_rolling = model1_lightgbm (dfclean_rolling)
model_rolling = result_rolling$model
mae_rolling = result_rolling$mae_errors
mae_rolling
feature_importance_rolling <- lgb.importance(model_rolling, percentage = TRUE)
feature_importance_rolling


dfclean_imb=imbalance_features(dfclean)
result_imb = model1_lightgbm (dfclean_imb)
model_imb = result_imb$model
mae_imb = result_imb$mae_errors
mae_imb
feature_importance_imb <- lgb.importance(model_imb, percentage = TRUE)
feature_importance_imb

dfclean_norm=other_features_norm(dfclean)
result_norm = model1_lightgbm (dfclean_norm)
model_norm = result_norm$model
mae_norm = result_norm$mae_errors
mae_out
feature_importance_out <- lgb.importance(model_out, percentage = TRUE)
feature_importance_out



result_out = model1_lightgbm (dfclean_feat_new...)
model_out = result_out$model
mae_out = result_out$mae_errors
mae_out_clipped = result_out$mae_errors_clipped




train_predict_lightgbm <- function(data_frame) {
  # Initialisation d'une liste pour stocker les modèles
  models <- list()
  
  # Initialisation d'une liste pour stocker les erreurs MAE de chaque modèle
  mae_errors <- numeric(47)
  
  # Boucle sur les périodes de 10 jours, pour un total de 47 itérations
  for (i in 0:46) {
    # Définition des intervalles de jours pour l'entraînement et le test
    train_start_day <- i * 10
    test_start_day <- train_start_day + 10
    
    # Sélection des données d'entraînement et de test en utilisant data_frame
    train_data <- filter(data_frame, date_id > train_start_day & date_id <= train_start_day + 10)
    test_data <- filter(data_frame, date_id > test_start_day & date_id <= test_start_day + 10)
    
    # Séparation des caractéristiques et de la cible
    train_x <- select(train_data, -target)
    train_y <- train_data$target
    test_x <- select(test_data, -target)
    test_y <- test_data$target
    
    ####### Préparation des données pour LightGBM
    dtrain <- lgb.Dataset(data = as.matrix(train_x), label = train_y)
    dtest <- lgb.Dataset(data = as.matrix(test_x), label = test_y)
    
    # Configuration des paramètres de LightGBM
    params <- list(
      objective = "regression_l1",  # Utiliser MAE comme objectif
      metric = "mae",
      num_leaves = 31,
      learning_rate = 0.05,
      n_estimators = 100
    )
    
    # Entraînement du modèle LightGBM
    lightgbm_model <- lgb.train(params, dtrain, valids = list(test = dtest), verbose = 0)  # verbose = 0 pour moins de sortie console
    
    # Stockage du modèle
    models[[i + 1]] <- lightgbm_model
    
    # Prédiction avec le modèle LightGBM
    lightgbm_predictions <- predict(lightgbm_model, as.matrix(test_x))
    
    # Calcul et stockage de l'erreur MAE
    mae_errors[i + 1] <- mean(abs(test_y - lightgbm_predictions))
  }
  
  # Retourner les modèles et les erreurs MAE dans une liste
  return(list(models = models, mae_errors = mae_errors))
}


result_initial = train_predict_lightgbm(dfclean)
models_initial = result_initial$models
mae_initial = result_initial$mae_errors
mae_initial
mean(mae_initial)


dfclean_out=pro_outliers(dfclean)
result_out = train_predict_lightgbm(dfclean_out)
models_out = result_out$models
mae_out = result_out$mae_errors
mae_out
mean(mae_out)


dfclean_rolling=compute_rolling_averages_df(dfclean)
result_rolling = train_predict_lightgbm(dfclean_rolling)
models_rolling = result_rolling$models
mae_rolling = result_rolling$mae_errors
mae_rolling
mean(mae_rolling)


dfclean_imb=imbalance_features(dfclean)
result_imb = model1_lightgbm (dfclean_imb)
model_imb = result_imb$model
mae_imb = result_imb$mae_errors
mae_imb_clipped = result_imb$mae_errors_clipped
mae_imb
mean(mae_imb)

dfclean_norm=other_features_norm(dfclean)
result_norm = model1_lightgbm (dfclean_norm)
model_norm = result_norm$model
mae_norm = result_norm$mae_errors
mae_norm_clipped = result_out$mae_errors_clipped
mae_out
mae_out_clipped
feature_importance_out <- lgb.importance(model_out, percentage = TRUE)
feature_importance_out



result_out = model1_lightgbm (dfclean_feat_new...)
model_out = result_out$model
mae_out = result_out$mae_errors
mae_out_clipped = result_out$mae_errors_clipped
















# Exemple d'utilisation de la fonction
result <- train_predict_lightgbm(dfclean_feat)
models <- result$models
mae_errors <- result$mae_errors



# Affichage des erreurs MAE
print(mae_errors)

# Vous pouvez ensuite examiner les erreurs MAE pour évaluer la performance de chaque modèle sur son ensemble de test correspondant.
mean(mae_errors)


# Initialiser un vecteur pour stocker les noms des 10 premières features de chaque modèle

top_features_all_models <- c()
for (model in result$models) {
  # Calculer l'importance des features pour le modèle actuel
  feature_importance <- lgb.importance(model, percentage = TRUE)
  
  # Trier les features par importance décroissante et extraire les noms des 10 premiers
  top_25_features <- head(feature_importance[order(-feature_importance$Gain), ], 25)$Feature
  
  # Ajouter les noms des features à notre vecteur
  top_features_all_models <- c(top_features_all_models, top_25_features)
}

# Calculer la fréquence d'apparition de chaque feature
feature_frequencies <- table(top_features_all_models)

# Trier les features par fréquence décroissante
feature_frequencies_sorted <- sort(feature_frequencies, decreasing = TRUE)

# Afficher les résultats
print(feature_frequencies_sorted)


# Identifier les features qui répondent à ce critère
features_to_keep <- names(feature_frequencies_sorted[feature_frequencies_sorted >= 15])

# Afficher les features à conserver
print(features_to_keep)      

# Ajouter 'target' et 'date_id' aux features à conserver
features_to_keep_with_target_date <- c(features_to_keep, 'target', 'date_id')

# Sélectionner ces caractéristiques dans le dataframe original
data_filtered <- select(data_n, all_of(features_to_keep_with_target_date))

# Afficher les premières lignes du nouveau dataframe pour vérification
head(data_filtered)

# Exemple d'utilisation de la fonction
result <- train_predict_lightgbm(data_filtered)
models <- result$models
mae_errors <- result$mae_errors

train_predict_lightgbm_new <- function(data_frame) {
  # Initialisation d'une liste pour stocker les modèles
  models <- list()
  
  # Initialisation d'une liste pour stocker les erreurs MAE de chaque modèle
  mae_errors <- numeric(47)
  
  # Boucle sur les périodes de 10 jours, pour un total de 47 itérations
  for (i in 0:46) {
    # Définition des intervalles de jours pour l'entraînement et le test
    train_end_day <- (i + 1) * 10
    test_start_day <- train_end_day + 1
    test_end_day <- test_start_day + 9
    
    # Sélection des données d'entraînement et de test en utilisant data_frame
    train_data <- data_frame[data_frame$date_id <= train_end_day, ]
    test_data <- data_frame[data_frame$date_id >= test_start_day & data_frame$date_id <= test_end_day, ]
    
    # Séparation des caractéristiques et de la cible
    train_x <- select(train_data, -target)
    train_y <- train_data$target
    test_x <- select(test_data, -target)
    test_y <- test_data$target
    
    ####### Préparation des données pour LightGBM
    dtrain <- lgb.Dataset(data = as.matrix(train_x), label = train_y)
    dtest <- lgb.Dataset(data = as.matrix(test_x), label = test_y)
    
    # Configuration des paramètres de LightGBM
    params <- list(
      objective = "regression_l1",  # Utiliser MAE comme objectif
      metric = "mae",
      num_leaves = 31,
      learning_rate = 0.05,
      n_estimators = 100
    )
    
    # Entraînement du modèle LightGBM
    lightgbm_model <- lgb.train(params, dtrain, valids = list(test = dtest), verbose = 0)  # verbose = 0 pour moins de sortie console
    
    # Stockage du modèle
    models[[i + 1]] <- lightgbm_model
    
    # Prédiction avec le modèle LightGBM
    lightgbm_predictions <- predict(lightgbm_model, as.matrix(test_x))
    
    # Calcul et stockage de l'erreur MAE
    mae_errors[i + 1] <- mean(abs(test_y - lightgbm_predictions))
  }
  
  # Retourner les modèles et les erreurs MAE dans une liste
  return(list(models = models, mae_errors = mae_errors))
}




result <- train_predict_lightgbm_new(data_filtered)
models_new <- result$models
mae_errors_new <- result$mae_errors

# Affichage des erreurs MAE
print(mae_errors_new)
print(mae_errors)
# Calcul de la moyenne des erreurs MAE
mean(mae_errors_new)
mean(mae_errors)

# Compare the two lists element-wise
comparison <- mae_errors >= mae_errors_new

# Convert the TRUE/FALSE values to 1/0
comparison_values <- as.integer(comparison)

# Calculate the proportion of 1's in the comparison
proportion_of_ones <- sum(comparison_values) / length(comparison_values)

# Return both the comparison list and the proportion
list(comparison_values = comparison_values, proportion_of_ones = proportion_of_ones)

difference <- mae_errors - mae_errors_new
difference
print(max(difference))
print(min(difference))
print(mean(difference))








# Sélectionner les données d'entraînement (10 premiers jours)
train_data <- data_filtered %>%
  filter(date_id %in% 0:10)

# Sélectionner les données de test (les 10 jours suivants, soit les jours 21 à 40)
test_data <- data_filtered %>%
  filter(date_id %in% 11:20)

# Afficher un résumé pour vérifier
print(paste("Taille des données d'entraînement :", nrow(train_data)))
print(paste("Taille des données de test :", nrow(test_data)))

# Pour les données d'entraînement
train_features <- train_data %>% select(-target) # Sélectionner toutes les colonnes sauf 'target'
train_target <- train_data$target # Sélectionner uniquement la colonne 'target'

# Pour les données de test
test_features <- test_data %>% select(-target) # Sélectionner toutes les colonnes sauf 'target'
test_target <- test_data$target # Sélectionner uniquement la colonne 'target'





# Séparation des caractéristiques et de la cible
train_y <- train_data$target
train_x <- train_data[, colnames(train_data) != "target"]
test_y <- test_data$target
test_x <- test_data[, colnames(test_data) != "target"]


library(gbm3)
train_x$outlier_position <- as.factor(train_x$outlier_position)
test_x$outlier_position <- as.factor(test_x$outlier_position)
str(train_x)
####### Entraînement du modèle GBM
gbm_model <- gbm(train_y ~ ., data = train_x, distribution = "gaussian", n.trees = 1000, interaction.depth = 3)

# Prédiction avec GBM
gbm_predictions <- predict(gbm_model, newdata = test_x, n.trees = 1000)
gbm_mae <- mean(abs(test_y - gbm_predictions))
summary(gbm_model)


####### XGBoost
train_x$outlier_position <- as.numeric(as.character(train_x$outlier_position))
test_x$outlier_position <- as.numeric(as.character(test_x$outlier_position))
train_x_matrix <- as.matrix(train_x)
str(train_x)
dtrain <- xgb.DMatrix(data = train_x_matrix, label = train_y)
dtest <- xgb.DMatrix(data = as.matrix(test_x), label = test_y)
params <- list(objective = "reg:squarederror", eval_metric = "mae")
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100)
xgb_predictions <- predict(xgb_model, dtest)
xgb_mae <- mean(abs(test_y - xgb_predictions))


library(mboost)


####### Ajustement du modèle glmboost
glmboost_model <- glmboost(train_y ~ ., data = train_x)

# Prédiction sur l'ensemble de test
glmboost_predictions <- predict(glmboost_model, newdata = test_x)

# Calcul de MAE pour glmboost
glmboost_mae <- mean(abs(test_y - glmboost_predictions))



####### Préparation des données pour LightGBM
dtrain <- lgb.Dataset(data = as.matrix(train_x), label = train_y)
dtest <- lgb.Dataset(data = as.matrix(test_x), label = test_y)

# Configuration des paramètres de LightGBM
params <- list(
  objective = "regression_l1",  # MAE
  metric = "mae",
  num_leaves = 31,
  learning_rate = 0.05,
  n_estimators = 100
)

# Entraînement du modèle LightGBM
lightgbm_model <- lgb.train(params, dtrain, valids = list(test = dtest), verbose = 1)

# Prédiction avec le modèle LightGBM
lightgbm_predictions <- predict(lightgbm_model, as.matrix(test_x))

# Calcul de MAE pour LightGBM
lightgbm_mae <- mean(abs(test_y - lightgbm_predictions))
print(paste("MAE pour LightGBM:", lightgbm_mae))



# Affichage des erreurs MAE
print(paste("GBM MAE:", gbm_mae))
print(paste("XGBoost MAE:", xgb_mae))
print(paste("MAE pour glmboost:", glmboost_mae))
print(paste("MAE pour LightGBM:", lightgbm_mae))




train_predict_compare <- function(train_data, test_data) {
  # Préparation des données
  train_y <- train_data$target
  train_x <- select(train_data, -target)
  test_y <- test_data$target
  test_x <- select(test_data, -target)
  

  ####### GBM
  train_x_gbm <- train_x
  test_x_gbm <- test_x
  # Exemple de réduction de niveaux en R
  train_x_gbm$outlier_position <- as.factor(train_x_gbm$outlier_position)
  levels_to_combine <- levels(train_x_gbm$outlier_position)[-(1:1024)]
  train_x_gbm$outlier_position <- factor(sapply(train_x_gbm$outlier_position, function(x) if(x %in% levels_to_combine) "Autre" else x))
  
  test_x_gbm$outlier_position <- as.factor(test_x_gbm$outlier_position)
  levels_to_combine <- levels(test_x_gbm$outlier_position)[-(1:1024)]
  test_x_gbm$outlier_position <- factor(sapply(test_x_gbm$outlier_position, function(x) if(x %in% levels_to_combine) "Autre" else x))
  
  
  gbm_model <- gbm(train_y ~ ., data = train_x_gbm, distribution = "gaussian", n.trees = 1000, interaction.depth = 3)
  gbm_predictions <- predict(gbm_model, newdata = test_x_gbm, n.trees = 1000)
  gbm_mae <- mean(abs(test_y - gbm_predictions))
  
  ####### XGBoost
  train_x_xgb <- train_x
  test_x_xgb <- test_x
  train_x_xgb$outlier_position <- as.numeric(as.character(train_x_xgb$outlier_position))
  test_x_xgb$outlier_position <- as.numeric(as.character(test_x_xgb$outlier_position))
  dtrain <- xgb.DMatrix(data = as.matrix(train_x_xgb), label = train_y)
  dtest <- xgb.DMatrix(data = as.matrix(test_x_xgb), label = test_y)
  xgb_model <- xgb.train(params = list(objective = "reg:squarederror", eval_metric = "mae"), data = dtrain, nrounds = 100)
  xgb_predictions <- predict(xgb_model, dtest)
  xgb_mae <- mean(abs(test_y - xgb_predictions))
  
  ####### GLMBoost
  ####### Ajustement du modèle glmboost
  glmboost_model <- glmboost(train_y ~ ., data = train_x_xgb)
  
  # Prédiction sur l'ensemble de test
  glmboost_predictions <- predict(glmboost_model, newdata = test_x_xgb)
  
  # Calcul de MAE pour glmboost
  glmboost_mae <- mean(abs(test_y - glmboost_predictions))
  
  
  ####### LightGBM
  dtrain_lgb <- lgb.Dataset(data = as.matrix(train_x), label = train_y)
  dtest_lgb <- lgb.Dataset(data = as.matrix(test_x), label = test_y)
  lightgbm_model <- lgb.train(params = list(objective = "regression_l1", metric = "mae", num_leaves = 31, learning_rate = 0.05, n_estimators = 100), dtrain_lgb, valids = list(test = dtest_lgb), verbose = 0)
  lightgbm_predictions <- predict(lightgbm_model, as.matrix(test_x))
  lightgbm_mae <- mean(abs(test_y - lightgbm_predictions))
  
  # Retourner les erreurs MAE pour chaque modèle
  return(list(
    GBM_MAE = gbm_mae,
    XGBoost_MAE = xgb_mae,
    GLMBoost_MAE = glmboost_mae,
    LightGBM_MAE = lightgbm_mae
  ))
}

# Exemple d'utilisation de la fonction
# Sélectionner les données d'entraînement (10 premiers jours)
train_data <- data_filtered %>%
  filter(date_id %in% 10:20)

# Sélectionner les données de test (les 10 jours suivants, soit les jours 21 à 40)
test_data <- data_filtered %>%
  filter(date_id %in% 21:30)

mae_errors <- train_predict_compare(train_data, test_data)
print(mae_errors)
mae2=mae_errors
mae1







train_predict_compare_0 <- function(data_frame) {
  # Initialisation de listes pour stocker les erreurs MAE de chaque modèle pour toutes les périodes
  mae_errors_gbm <- numeric(47)
  mae_errors_xgb <- numeric(47)
  mae_errors_glmboost <- numeric(47)
  mae_errors_lgb <- numeric(47)
  
  # Boucle sur les périodes de 10 jours, pour un total de 47 itérations
  for (i in 0:10) {
    # Définition des intervalles de jours pour l'entraînement et le test
    train_start_day <- i * 10
    test_start_day <- train_start_day + 10
    
    # Sélection des données d'entraînement et de test en utilisant data_frame
    train_data <- filter(data_frame, date_id > train_start_day & date_id <= train_start_day + 10)
    test_data <- filter(data_frame, date_id > test_start_day & date_id <= test_start_day + 10)
    
    # Appliquer la fonction train_predict_compare pour les données d'entraînement et de test actuelles
    mae_errors <- train_predict_compare(train_data, test_data)
    
    # Stocker les erreurs MAE pour chaque modèle
    mae_errors_gbm[i + 1] <- mae_errors$GBM_MAE
    mae_errors_xgb[i + 1] <- mae_errors$XGBoost_MAE
    mae_errors_glmboost[i + 1] <- mae_errors$GLMBoost_MAE
    mae_errors_lgb[i + 1] <- mae_errors$LightGBM_MAE
  }
  
  # Retourner les erreurs MAE pour chaque modèle et chaque période
  return(list(
    GBM_MAE = mae_errors_gbm,
    XGBoost_MAE = mae_errors_xgb,
    GLMBoost_MAE = mae_errors_glmboost,
    LightGBM_MAE = mae_errors_lgb
  ))
}

# Appliquer la fonction pour comparer les modèles sur les 47 périodes
resultats_mae <- train_predict_compare_0(data_filtered)

# Afficher les erreurs MAE pour chaque modèle et chaque période
print(resultats_mae$GBM_MAE)
print(resultats_mae$XGBoost_MAE)
print(resultats_mae$GLMBoost_MAE)
print(resultats_mae$LightGBM_MAE)

sum(resultats_mae$GBM_MAE)/11
sum(resultats_mae$XGBoost_MAE)/11
sum(resultats_mae$GLMBoost_MAE)/11
sum(resultats_mae$LightGBM_MAE)/11






train_predict_compare_new <- function(data_frame) {
  # Initialisation de listes pour stocker les erreurs MAE de chaque modèle pour toutes les périodes
  mae_errors_gbm <- numeric(47)
  mae_errors_xgb <- numeric(47)
  mae_errors_glmboost <- numeric(47)
  mae_errors_lgb <- numeric(47)
  
  # Boucle sur les périodes de 10 jours, pour un total de 47 itérations
  for (i in 0:46) {
    # Définition des intervalles de jours pour l'entraînement et le test
    train_end_day <- (i + 1) * 10
    test_start_day <- train_end_day + 1
    test_end_day <- test_start_day + 9
    
    # Sélection des données d'entraînement et de test en utilisant data_frame
    train_data <- data_frame %>% filter(date_id <= train_end_day)
    test_data <- data_frame %>% filter(date_id >= test_start_day & date_id <= test_end_day)
    
    # Appliquer la fonction train_predict_compare pour les données d'entraînement et de test actuelles
    mae_errors <- train_predict_compare(train_data, test_data)
    
    # Stocker les erreurs MAE pour chaque modèle
    mae_errors_gbm[i + 1] <- mae_errors$GBM_MAE
    mae_errors_xgb[i + 1] <- mae_errors$XGBoost_MAE
    mae_errors_glmboost[i + 1] <- mae_errors$GLMBoost_MAE
    mae_errors_lgb[i + 1] <- mae_errors$LightGBM_MAE
  }
  
  # Retourner les erreurs MAE pour chaque modèle et chaque période
  return(list(
    GBM_MAE = mae_errors_gbm,
    XGBoost_MAE = mae_errors_xgb,
    GLMBoost_MAE = mae_errors_glmboost,
    LightGBM_MAE = mae_errors_lgb
  ))
}

# Appliquer la fonction pour comparer les modèles sur les 47 périodes
resultats_mae_new <- train_predict_compare_new(data_filtered)

# Afficher les erreurs MAE pour chaque modèle et chaque période
print(resultats_mae_new$GBM_MAE)
print(resultats_mae_new$XGBoost_MAE)
print(resultats_mae_new$GLMBoost_MAE)
print(resultats_mae_new$LightGBM_MAE)









train_data <- data_filtered %>%
  filter(date_id %in% 0:10)

# Sélectionner les données de test (les 10 jours suivants, soit les jours 21 à 40)
test_data <- data_filtered %>%
  filter(date_id %in% 11:20)



train_y <- train_data$target
train_x <- select(train_data, -target)
test_y <- test_data$target
test_x <- select(test_data, -target)

####### Préparation des données pour LightGBM
dtrain <- lgb.Dataset(data = as.matrix(train_x), label = train_y)
dtest <- lgb.Dataset(data = as.matrix(test_x), label = test_y)

# Configuration des paramètres de LightGBM
params <- list(
  objective = "regression_l1",  # MAE
  metric = "mae",
  num_leaves = 31,
  learning_rate = 0.05,
  n_estimators = 100
)

# Entraînement du modèle LightGBM
lightgbm_model <- lgb.train(params, dtrain, valids = list(test = dtest), verbose = 1)

# Prédiction avec le modèle LightGBM
lightgbm_predictions <- predict(lightgbm_model, as.matrix(test_x))

clipped_predictions <- pred_boost(lightgbm_predictions)
lightgbm_mae_2 <- mean(abs(test_y - clipped_predictions))
# Calcul de MAE pour LightGBM
print(paste("MAE pour LightGBM:", lightgbm_mae_2))





train_predict_lightgbm_2 <- function(data_frame) {
  # Initialisation d'une liste pour stocker les modèles
  models <- list()
  
  # Initialisation d'une liste pour stocker les erreurs MAE de chaque modèle
  mae_errors <- numeric(47)
  
  # Boucle sur les périodes de 10 jours, pour un total de 47 itérations
  for (i in 0:10) {
    # Définition des intervalles de jours pour l'entraînement et le test
    train_end_day <- (i + 1) * 10
    test_start_day <- train_end_day + 1
    test_end_day <- test_start_day + 9
    
    # Sélection des données d'entraînement et de test en utilisant data_frame
    train_data <- data_frame[data_frame$date_id <= train_end_day, ]
    test_data <- data_frame[data_frame$date_id >= test_start_day & data_frame$date_id <= test_end_day, ]
    
    # Séparation des caractéristiques et de la cible
    train_x <- select(train_data, -target)
    train_y <- train_data$target
    test_x <- select(test_data, -target)
    test_y <- test_data$target
    
    ####### Préparation des données pour LightGBM
    dtrain <- lgb.Dataset(data = as.matrix(train_x), label = train_y)
    dtest <- lgb.Dataset(data = as.matrix(test_x), label = test_y)
    
    # Configuration des paramètres de LightGBM
    params <- list(
      objective = "regression_l1",  # Utiliser MAE comme objectif
      metric = "mae",
      num_leaves = 31,
      learning_rate = 0.05,
      n_estimators = 100
    )
    
    # Entraînement du modèle LightGBM
    lightgbm_model <- lgb.train(params, dtrain, valids = list(test = dtest), verbose = 0)  # verbose = 0 pour moins de sortie console
    
    # Stockage du modèle
    models[[i + 1]] <- lightgbm_model
    
    # Prédiction avec le modèle LightGBM
    lightgbm_predictions <- predict(lightgbm_model, as.matrix(test_x))
    clipped_predictions <- pred_boost(lightgbm_predictions)
    
    # Calcul et stockage de l'erreur MAE
    mae_errors[i + 1] <- mean(abs(test_y - clipped_predictions))
  }
  
  # Retourner les modèles et les erreurs MAE dans une liste
  return(list(models = models, mae_errors = mae_errors))
}




result2 <- train_predict_lightgbm_2(data_filtered)
models_2 <- result2$models
mae_errors_2 <- result2$mae_errors
print(mae_errors_2)
print(mae_errors_new)
sum(mae_errors_2)/11
mae_values <- sapply(mae_errors_new, function(x) x)
# Calculer la moyenne des 11 premiers éléments
moyenne_11_premiers <- sum(mae_values[1:11]) / 11
# Afficher la moyenne
print(moyenne_11_premiers)


train_predict_lightgbm_1 <- function(data_frame) {
  # Initialisation d'une liste pour stocker les modèles
  models <- list()
  
  # Initialisation d'une liste pour stocker les erreurs MAE de chaque modèle
  mae_errors <- numeric(47)
  
  # Boucle sur les périodes de 10 jours, pour un total de 47 itérations
  for (i in 0:19) {
    # Définition des intervalles de jours pour l'entraînement et le test
    train_start_day <- i * 10
    test_start_day <- train_start_day + 10
    
    # Sélection des données d'entraînement et de test en utilisant data_frame
    train_data <- filter(data_frame, date_id > train_start_day & date_id <= train_start_day + 10)
    test_data <- filter(data_frame, date_id > test_start_day & date_id <= test_start_day + 10)
    
    # Séparation des caractéristiques et de la cible
    train_x <- select(train_data, -target)
    train_y <- train_data$target
    test_x <- select(test_data, -target)
    test_y <- test_data$target
    
    ####### Préparation des données pour LightGBM
    dtrain <- lgb.Dataset(data = as.matrix(train_x), label = train_y)
    dtest <- lgb.Dataset(data = as.matrix(test_x), label = test_y)
    
    # Configuration des paramètres de LightGBM
    params <- list(
      objective = "regression_l1",  # Utiliser MAE comme objectif
      metric = "mae",
      num_leaves = 31,
      learning_rate = 0.05,
      n_estimators = 100
    )
    
    # Entraînement du modèle LightGBM
    lightgbm_model <- lgb.train(params, dtrain, valids = list(test = dtest), verbose = 0)  # verbose = 0 pour moins de sortie console
    
    # Stockage du modèle
    models[[i + 1]] <- lightgbm_model
    
    # Prédiction avec le modèle LightGBM
    lightgbm_predictions <- predict(lightgbm_model, as.matrix(test_x))
    clipped_predictions <- pred_boost(lightgbm_predictions)
    
    # Calcul et stockage de l'erreur MAE
    mae_errors[i + 1] <- mean(abs(test_y - clipped_predictions))
  }
  
  # Retourner les modèles et les erreurs MAE dans une liste
  return(list(models = models, mae_errors = mae_errors))
}

result1 <- train_predict_lightgbm_1(data_filtered)
models_1 <- result1$models
mae_errors_1 <- result1$mae_errors
print(mae_errors_1)
sum(mae_errors_1)/20




df_train <- data_filtered[data_filtered$date_id <= 420, ]
df_test <- data_filtered[data_filtered$date_id > 420, ]

train_y <- df_train$target
train_x <- df_train[, colnames(df_train) != "target"]
test_y <- df_test$target
test_x <- df_test[, colnames(df_test) != "target"]

####### Préparation des données pour LightGBM
dtrain <- lgb.Dataset(data = as.matrix(train_x), label = train_y)
dtest <- lgb.Dataset(data = as.matrix(test_x), label = test_y)

# Configuration des paramètres de LightGBM
params <- list(
  objective = "regression_l1",  # Utiliser MAE comme objectif
  metric = "mae",
  num_leaves = 31,
  learning_rate = 0.05,
  n_estimators = 100
)

# Entraînement du modèle LightGBM
lightgbm_model <- lgb.train(params, dtrain, valids = list(test = dtest), verbose = 0) 

# Prédiction avec le modèle LightGBM
lightgbm_predictions <- predict(lightgbm_model, as.matrix(test_x))

mae_errors_3_0 <- mean(abs(test_y - lightgbm_predictions))


clipped_predictions <- pred_boost(lightgbm_predictions)

# Calcul et stockage de l'erreur MAE
mae_errors_3_1 <- mean(abs(test_y - clipped_predictions))
mae_errors_3_0 <- mean(abs(test_y - lightgbm_predictions))
mae_errors_3_0
mae_errors_3_1





#train_data <- data_filtered[data_filtered$date_id <= 420, ]
#test_data <- data_filtered[data_filtered$date_id > 420, ]

mae_errors_33 <- train_predict_compare(train_data, test_data)
print(mae_errors_33)

train_data <- dfclean_feat[dfclean_feat$date_id <= 420, ]
test_data <- dfclean_feat[dfclean_feat$date_id > 420, ]


# Séparation des caractéristiques et de la cible
train_y <- train_data$target
train_x <- train_data[, colnames(train_data) != "target"]
test_y <- test_data$target
test_x <- test_data[, colnames(test_data) != "target"]

train_x <- train_x[, !(names(train_x) %in% c("row_id"))]
test_x <- test_x[, !(names(test_x) %in% c("row_id"))]

library(gbm3)
train_x_gbm <- train_x
test_x_gbm <- test_x
# Exemple de réduction de niveaux en R
train_x_gbm$outlier_position <- as.factor(train_x_gbm$outlier_position)
levels_to_combine <- levels(train_x_gbm$outlier_position)[-(1:1024)]
train_x_gbm$outlier_position <- factor(sapply(train_x_gbm$outlier_position, function(x) if(x %in% levels_to_combine) "Autre" else x))

test_x_gbm$outlier_position <- as.factor(test_x_gbm$outlier_position)
levels_to_combine <- levels(test_x_gbm$outlier_position)[-(1:1024)]
test_x_gbm$outlier_position <- factor(sapply(test_x_gbm$outlier_position, function(x) if(x %in% levels_to_combine) "Autre" else x))

####### Entraînement du modèle GBM
gbm_model <- gbm(train_y ~ ., data = train_x_gbm, distribution = "gaussian", n.trees = 1000, interaction.depth = 3)

# Prédiction avec GBM
gbm_predictions <- predict(gbm_model, newdata = test_x_gbm, n.trees = 1000)
gbm_mae <- mean(abs(test_y - gbm_predictions))
summary(gbm_model)


####### XGBoost
dtrain <- xgb.DMatrix(data = as.matrix(train_x), label = train_y)
dtest <- xgb.DMatrix(data = as.matrix(test_x), label = test_y)
params <- list(objective = "reg:squarederror", eval_metric = "mae")
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100)
xgb_predictions <- predict(xgb_model, dtest)
xgb_mae <- mean(abs(test_y - xgb_predictions))


library(mboost)


####### Ajustement du modèle glmboost
glmboost_model <- glmboost(train_y ~ ., data = train_x)

# Prédiction sur l'ensemble de test
glmboost_predictions <- predict(glmboost_model, newdata = test_x)

# Calcul de MAE pour glmboost
glmboost_mae <- mean(abs(test_y - glmboost_predictions))



####### Préparation des données pour LightGBM
dtrain <- lgb.Dataset(data = as.matrix(train_x), label = train_y)
dtest <- lgb.Dataset(data = as.matrix(test_x), label = test_y)

# Configuration des paramètres de LightGBM
params <- list(
  objective = "regression_l1",  # MAE
  metric = "mae",
  num_leaves = 31,
  learning_rate = 0.05,
  n_estimators = 100
)

# Entraînement du modèle LightGBM
lightgbm_model <- lgb.train(params, dtrain, valids = list(test = dtest), verbose = 1)

# Prédiction avec le modèle LightGBM
lightgbm_predictions <- predict(lightgbm_model, as.matrix(test_x))

# Calcul de MAE pour LightGBM
lightgbm_mae <- mean(abs(test_y - lightgbm_predictions))
print(paste("MAE pour LightGBM:", lightgbm_mae))



# Affichage des erreurs MAE
print(paste("GBM MAE:", gbm_mae))
print(paste("XGBoost MAE:", xgb_mae))
print(paste("MAE pour glmboost:", glmboost_mae))
print(paste("MAE pour LightGBM:", lightgbm_mae))




# Supposons que votre dataframe est data_filtered et que vous voulez exclure la colonne 'target'
predictive_variables <- names(dfclean)[names(dfclean) != "target"]

# Afficher les noms des variables prédictives
print(predictive_variables)

# Construction de la chaîne de caractères avec 's()' pour chaque variable prédictive
#predictive_formula <- paste("s(", predictive_variables, ")", collapse = " + ")
predictive_formula <- paste("s(", predictive_variables, ", k=", 3, ")", collapse = " + ")

# Ajouter la variable cible pour compléter la formule
model_formula <- as.formula(paste("target ~", predictive_formula))

# Affichage de la formule pour vérification
print(model_formula)

update.packages("mgcv")


train_data <- dfclean[dfclean$date_id <= 10, ]
test_data <- dfclean[dfclean$date_id > 10, ]
test_data <- test_data[test_data$date_id <= 20, ]

gam_model <- gam(model_formula, data = train_data)
summary(gam_model)
gam.check(gam_model)

predictions <- predict(gam_model, newdata = test_data, type = "response")

# Calculer le MAE
mae <- mean(abs(predictions - test_data$target))
# Afficher le MAE
print(mae)





dfclean_num <- dfclean[sapply(dfclean, is.numeric)]
correlations <- cor(dfclean_num, use = "complete.obs") 
correlations
# Extract the correlations with the target column
target_correlations <- correlations["target", ]

# Remove the correlation of the target with itself
target_correlations <- target_correlations[names(target_correlations) != "target"]

# View the correlations
print(target_correlations)


# Charger les bibliothèques nécessaires
library(ggplot2)
library(reshape2)
library(corrplot)

# Assurez-vous que dfclean contient uniquement des données numériques
dfclean_numeric <- dfclean[sapply(dfclean, is.numeric)]

# Calculer la matrice de corrélation
cor_matrix <- cor(dfclean_numeric, use = "complete.obs")
cor_matrix
# Utiliser corrplot pour visualiser les corrélations
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, 
         # Ajouter plus d'options selon les besoins
)


# Supposons que vous ayez un vecteur de corrélations nommé target_correlations comme suit
target_correlations <- c(stock_id = 0.0001299739, date_id = 0.0008955521, 
                         seconds_in_bucket = -0.0022073161, imbalance_size = -0.0011896780, 
                         imbalance_buy_sell_flag = 0.0151030069, reference_price = -0.0208998767, 
                         matched_size = 0.0001969894, far_price = -0.0010572724, 
                         near_price = -0.0005207745, bid_price = -0.0223256716, 
                         bid_size = -0.0179968854, ask_price = -0.0216744018, 
                         ask_size = 0.0149365055, wap = -0.0351652367, time_id = 0.0008909512)

# Convertir le vecteur en dataframe
correlations_df <- data.frame(variable = names(target_correlations), 
                              correlation_with_target = target_correlations)

# Afficher le dataframe
print(correlations_df)
# Ordonner le dataframe par ordre décroissant de corrélation avec target
correlations_df_sorted <- correlations_df %>% 
  arrange(desc(correlation_with_target))

# Afficher le dataframe trié
print(correlations_df_sorted)



# Calcul de la corrélation entre far_price et near_price
correlation_far_near <- cor(dfclean$far_price, dfclean$near_price, use = "complete.obs")

# Affichage de la corrélation
print(correlation_far_near)

# Calcul de la corrélation de Spearman entre far_price et near_price
correlation_far_near_spearman <- cor(dfclean$far_price, dfclean$near_price, 
                                     use = "complete.obs", method = "spearman")

# Affichage de la corrélation de Spearman
print(correlation_far_near_spearman)




# Calculer la matrice de corrélation
cor_matrix_spearman <- cor(dfclean_numeric, use = "complete.obs", method = "spearman")
cor_matrix_spearman
# Utiliser corrplot pour visualiser les corrélations
corrplot(cor_matrix_spearman, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, 
         # Ajouter plus d'options selon les besoins
)

# Extract the correlations with the target column
target_correlations <- cor_matrix_spearman["target", ]

# Remove the correlation of the target with itself
target_correlations <- target_correlations[names(target_correlations) != "target"]

# View the correlations
print(target_correlations)
target_correlations <- c(stock_id = -3.827539e-04, date_id = 1.095880e-03,
                         seconds_in_bucket = -2.761554e-03, imbalance_size = -1.879557e-03,
                         imbalance_buy_sell_flag = 1.616637e-02, reference_price = -1.309348e-02,
                         matched_size = -2.774164e-03, far_price = -3.117249e-05,
                         near_price = -1.358381e-03, bid_price = -1.420526e-02,
                         bid_size = -6.834553e-02, ask_price = -1.382706e-02,
                         ask_size = 6.709429e-02, wap = -2.930859e-02, time_id = 1.090145e-03)

# Convert the vector to a dataframe
correlations_df <- data.frame(variable = names(target_correlations),
                              correlation_with_target = target_correlations)
# Ordonner le dataframe par ordre décroissant de corrélation avec target
correlations_df_sorted <- correlations_df %>% 
  arrange(desc(correlation_with_target))

# Afficher le dataframe trié
print(correlations_df_sorted)











# Présumons que dfclean_num est votre dataframe et que "target" est le nom de votre variable cible

# Initialisation d'un vecteur pour stocker les corrélations de Spearman
target_correlations_spearman <- numeric(length = ncol(dfclean_num) - 1) # -1 pour exclure la colonne target elle-même

# Noms des colonnes sans la colonne target
variable_names <- setdiff(names(dfclean_num), "target")

# Calcul des corrélations de Spearman pour chaque variable explicative avec la variable cible
for (i in seq_along(variable_names)) {
  variable_name <- variable_names[i]
  target_correlations_spearman[i] <- cor(dfclean_num[[variable_name]], dfclean_num[["target"]], 
                                         use = "complete.obs", method = "spearman")
}

# Création d'un dataframe pour afficher les variables et leurs corrélations avec la cible
correlations_df <- data.frame(Variable = variable_names, 
                              CorrelationWithTarget = target_correlations_spearman)

# Trier le dataframe par ordre décroissant de corrélation
correlations_df_sorted <- correlations_df[order(-correlations_df$CorrelationWithTarget), ]

# Afficher le dataframe trié
print(correlations_df_sorted)

target_correlations_spearman
seq_along(variable_names)
correlations_df
correlations_df_sorted 
