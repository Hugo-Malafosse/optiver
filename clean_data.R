rm(list=objects())
library(rpart)
library(magrittr)
library(party)
library(yarrr)
library(tree)
library(rpart.plot)
library(progress)
library(mgcv)
library(mgcViz)
library(gridExtra)
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
library(caret)
library(Metrics)
library(xgboost)
library(lightgbm)



data_all<-read_csv("/Users/dhia-elhaqouerfelli/Desktop/lejeune/projet prevision/optiver/train.csv", col_names =TRUE)

summary.data.frame(data_all)


inspect_columns <- function(df) {
  # Calcul de diverses statistiques pour chaque colonne
  unique <- sapply(df, function(col) length(unique(col)) == nrow(df))
  cardinality <- sapply(df, function(col) length(unique(col)))
  with_null <- sapply(df, function(col) any(is.na(col)))
  null_pct <- sapply(df, function(col) round(sum(is.na(col)) / nrow(df) * 100, 2))
  first_row <- sapply(df, function(col) col[1])
  random_row <- sapply(df, function(col) col[sample(nrow(df), 1)])
  last_row <- sapply(df, function(col) col[nrow(df)])
  dtype <- sapply(df, function(col) class(col)[1])
  
  # Création du dataframe résultat
  result <- data.frame(unique, cardinality, with_null, null_pct, first_row, random_row, last_row, dtype, stringsAsFactors = FALSE)
  
  # Ajustement des noms de colonnes pour la cohérence avec l'exemple Python
  names(result) <- c('unique', 'cardinality', 'with_null', 'null_pct', '1st_row', 'random_row', 'last_row', 'dtype')
  
  return(result)
}
inspect_columns(data_all)

# Extraction des valeurs uniques dans la colonne 'seconds_in_bucket'
unique_values_seconds_in_bucket <- unique(data_all$seconds_in_bucket)

# Afficher les valeurs uniques
unique_values_seconds_in_bucket

# Filtrer le dataframe pour stock_id == 6 et date_id == 4
filtered_data <- subset(data_all, stock_id == 6 & date_id == 4)

# Afficher le dataframe filtré
print(filtered_data)

# Obtenir les dimensions du dataframe filtré
dimensions <- dim(filtered_data)

# Afficher les dimensions
print(dimensions)





#Identifier les identifiants uniques de stock et de date
all_stock_id <- sort(unique(data_all$stock_id))
all_date_id <- sort(unique(data_all$date_id))

# Créer toutes les combinaisons possibles de date_id et stock_id
all_combinations <- expand.grid(date_id = all_date_id, stock_id = all_stock_id)

missing_data <- all_combinations %>%
  # Anti-join exclut les lignes de all_combinations qui ont une correspondance dans data_all
  anti_join(data_all, by = c("date_id", "stock_id"))

# Afficher les combinaisons manquantes
print(missing_data)



# Vérification des valeurs manquantes (Na) avec les noms de colonnes
missing_values <- sapply(data_all, function(x) sum(is.na(x)) * 100 / nrow(data_all))
missing_values <- sort(missing_values, decreasing = TRUE)
missing_values_df <- data.frame( percent_missing = missing_values)
missing_values_df

# Calcul du nombre de valeurs manquantes par colonne
train_count <- sapply(data_all, function(x) sum(is.na(x)))

# Calcul du pourcentage de valeurs manquantes par colonne
train_percent <- sapply(data_all, function(x) sum(is.na(x)) / length(x))

# Création d'un dataframe pour afficher les résultats
train_mv_df <- data.frame(count = train_count, percentage = train_percent * 100)

# Affichage du dataframe
print(train_mv_df)










# Fonction pour compter les valeurs NA dans chaque groupe
count_nan <- function(group) {
  sum(is.na(group))
}

# Grouper les données par intervalles de 10 secondes dans 'seconds_in_bucket'
# et compter les valeurs NA dans chaque groupe (sauf pour 'seconds_in_bucket')
incr_nan <- data_all %>%
  mutate(bucket = cut(seconds_in_bucket, breaks = seq(-10, max(seconds_in_bucket, na.rm = TRUE) + 10, by = 10), include.lowest = FALSE)) %>%
  group_by(bucket) %>%
  summarise(across(-seconds_in_bucket, ~sum(is.na(.)))) %>%
  ungroup()  # Pour enlever le regroupement par 'bucket'

# Afficher les résultats
print(incr_nan)




# Transformation des données pour le tracé
incr_nan_long <- incr_nan %>%
  pivot_longer(-bucket, names_to = "variable", values_to = "missing_values")

# Tracer

ggplot(incr_nan_long, aes(x = bucket, y = missing_values, color = variable, group = variable)) +
  geom_line() + # Tracer des lignes en groupant par 'variable'
  theme_minimal() + # Appliquer un thème minimal
  labs(title = "Progression des valeurs manquantes par intervalle de temps",
       x = "Intervalles de temps dans le seau (secondes)",
       y = "Nombre de valeurs manquantes",
       color = "Variable") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotation des étiquettes de l'axe X pour une meilleure lisibilité
        plot.title = element_text(hjust = 0.5)) # Centrer le titre du graphique


# Analyser les valeurs manquantes par 'date_id'
analysis_nan_date <- data_all %>%
  group_by(date_id) %>%
  summarise(across(everything(), ~sum(is.na(.))))

# Afficher les résultats
print(analysis_nan_date)




# Fondre les données pour un format long approprié pour ggplot
analysis_nan_date_long <- reshape2::melt(analysis_nan_date, id.vars = "date_id")

# Filtrer les colonnes d'intérêt
columns_of_interest <- c('imbalance_size','far_price', 'near_price', 'reference_price', 'matched_size', 'bid_price', 'ask_price')  
analysis_nan_date_filtered <- subset(analysis_nan_date_long, variable %in% columns_of_interest)

# Créer le graphique
ggplot(data = analysis_nan_date_filtered, aes(x = date_id, y = value, color = variable)) +
  geom_line() +
  labs(title = "Missing values for 'columns_of_interest' over different date_ids",
       x = "date_id",
       y = "Number of Missing Values") +
  theme_minimal()


# Sélectionner uniquement les colonnes d'intérêt
columns_of_interest2 <- c('imbalance_size', 'reference_price', 'matched_size', 'bid_price', 'ask_price')
analysis_nan_date_filtered2 <- analysis_nan_date[, c('date_id', columns_of_interest2)]

# Transformer les données de format large à long pour le tracé avec ggplot2
analysis_nan_date_long2 <- melt(analysis_nan_date_filtered2, id.vars = 'date_id', variable.name = 'Variable', value.name = 'Number_of_Missing_Values')

# Tracer les valeurs manquantes pour les colonnes d'intérêt par date_id
ggplot(data = analysis_nan_date_long2, aes(x = date_id, y = Number_of_Missing_Values, color = Variable)) +
  geom_line() +
  labs(title = "Missing values for selected columns over different date_ids",
       x = "Date ID",
       y = "Number of Missing Values") +
  theme_minimal()

# Filtrer les lignes où imbalance_size est égal à 55
filtered_rows <- subset(analysis_nan_date, imbalance_size == 55)

# Afficher les résultats
print(filtered_rows)




analysis_nan_date_long3 <- reshape2::melt(analysis_nan_date, id.vars = "date_id", measure.vars = c("far_price", "near_price"))

# Générer le graphique
ggplot(data = analysis_nan_date_long3, aes(x = date_id, y = value, color = variable)) +
  geom_line() +
  labs(title = "Missing values for near and far prices over different date_ids",
       x = "Date ID",
       y = "Number of Missing Values",
       color = "Price Type") +
  theme_minimal()



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
dfclean=cond(data_all)




# 3. Calculer le pourcentage de valeurs NA après imputation
pourcentage_na_apres <- sapply(dfclean, function(x) mean(is.na(x))) * 100
pourcentage_na_apres_df <- as.data.frame(pourcentage_na_apres)
# Afficher le DataFrame vertical
print(pourcentage_na_apres_df)



# 5. Calculer le pourcentage de données supprimées
pourcentage_donnees_supprimees <- (nrow(data_all) - nrow(dfclean)) / nrow(data_all) * 100
print(pourcentage_donnees_supprimees)




# Les caractéristiques catégorielles et numériques
cat_features <- c('imbalance_buy_sell_flag', 'stock_id')
num_features <- c('imbalance_size', 'reference_price', 'matched_size', 'far_price', 'near_price', 'bid_price', 'bid_size', 'ask_price', 'ask_size', 'wap')

# Vérification de la distribution de chaque caractéristique numérique à l'aide d'histogrammes
# Pour cela, nous allons utiliser une boucle pour parcourir chaque caractéristique numérique et créer un graphique de densité pour chacune

# Ouvrir une nouvelle fenêtre graphique avec une grille de sous-graphiques
par(mfrow=c(5, 2))

# Boucle pour créer un graphique de densité pour chaque caractéristique numérique
for (feature in num_features) {
  # Création du graphique de densité
  p <- ggplot(dfclean, aes_string(x = feature)) +
    geom_density(fill = "blue", alpha = 0.5) + # Remplissage bleu avec transparence
    labs(title = paste(feature, "Distribution"), x = NULL, y = NULL) +
    theme_minimal(base_size = 11) # Utilisation du thème minimal avec une taille de base de 11 pour les titres
  print(p)
}

# Restaurer les paramètres graphiques par défaut
par(mfrow=c(1,1))

# Liste pour stocker les objets ggplot
plot_list <- list()

# Créer un objet ggplot pour chaque caractéristique numérique et l'ajouter à la liste
for (feature in num_features) {
  p <- ggplot(dfclean, aes_string(x = feature)) +
    geom_density(fill = "blue", alpha = 0.5) +
    labs(title = paste(feature, "Distribution")) +
    theme_minimal()
  plot_list[[feature]] <- p
}

# Utiliser grid.arrange pour combiner et afficher tous les graphiques
do.call(gridExtra::grid.arrange, c(plot_list, ncol=2))



# Pour atténuer l'effet des valeurs extrêmes
#df$far_price_adjusted <- log(df$far_price + 1)

# Création d'un graphique de la densité de la distribution de la variable cible
ggplot(dfclean, aes(x = target)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  ggtitle("Distribution de la variable cible") +
  theme_minimal() +
  theme(plot.title = element_text(size = 11))




# Création d'un DataFrame avec les fréquences relatives
df_cat <- as.data.frame(prop.table(table(dfclean$imbalance_buy_sell_flag)))

# Renommage des colonnes
colnames(df_cat) <- c('imbalance_buy_sell_flag', 'count')

# Impression du tableau des fréquences
print(df_cat)
print('--------------------------------------------------------')

# Création du graphique à secteurs
ggplot(df_cat, aes(x = "", y = count, fill = imbalance_buy_sell_flag)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = pi) +
  theme_void() +
  geom_text(aes(label = scales::percent(count)), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Paired") +
  ggtitle("Distribution de imbalance_buy_sell_flag")






# Sélection des caractéristiques pour la corrélation
corr_features <- c(num_features, 'target') # num_features devrait être défini précédemment

# Calcul de la matrice de corrélation
corr <- cor(dfclean[corr_features], use = "complete.obs") # 'use' gère les valeurs manquantes

# Affichage de la heatmap de corrélation
corrplot::corrplot(corr, method = "color", type = "upper", order = "hclust", 
                   tl.col = "black", tl.srt = 45, 
                   # Custom diverging colormap
                   col = colorRampPalette(c("#2A9D8F", "#E9C46A", "#F4A261", "#E76F51"))(200))



# Sélectionner les données d'entraînement (10 premiers jours)
train_data <- dfclean %>%
  filter(date_id %in% 0:10)

# Sélectionner les données de test (les 10 jours suivants, soit les jours 11 à 20)
test_data <- dfclean %>%
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

train_x <- train_x[, !(names(train_x) %in% c("row_id"))]
test_x <- test_x[, !(names(test_x) %in% c("row_id"))]

library(gbm3)

####### Entraînement du modèle GBM
gbm_model <- gbm(train_y ~ ., data = train_x, distribution = "gaussian", n.trees = 1000, interaction.depth = 3)

# Prédiction avec GBM
gbm_predictions <- predict(gbm_model, newdata = test_x, n.trees = 1000)
gbm_mae <- mean(abs(test_y - gbm_predictions))
summary(gbm_model)
# Création d'un dataframe à partir des informations fournies
gbm_summary_df <- data.frame(
  var = c('ask_size', 'bid_size', 'ask_price', 'wap', 'seconds_in_bucket',
          'reference_price', 'bid_price', 'time_id', 'imbalance_size',
          'near_price', 'stock_id', 'matched_size', 'far_price',
          'imbalance_buy_sell_flag', 'date_id'),
  rel_inf = c(26.48290084, 23.43026282, 14.86921002, 12.75944246, 7.24007569,
              6.85841395, 3.46032419, 1.82039499, 1.63430076,
              0.59202829, 0.32455397, 0.31465438, 0.16953865,
              0.04389897, 0.00000000)
)

# Afficher le dataframe
print(gbm_summary_df)


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
feature_importance <- lgb.importance(lightgbm_model, percentage = TRUE)
feature_importance
# Calcul de MAE pour LightGBM
lightgbm_mae <- mean(abs(test_y - lightgbm_predictions))
print(paste("MAE pour LightGBM:", lightgbm_mae))

# Création d'un dataframe à partir des informations fournies
feature_importance <- data.frame(
  Feature = c('time_id', 'ask_size', 'bid_size', 'matched_size', 'seconds_in_bucket',
              'imbalance_size', 'far_price', 'stock_id', 'near_price', 'ask_price',
              'bid_price', 'date_id', 'imbalance_buy_sell_flag', 'reference_price', 'wap'),
  Gain = c(0.20741683, 0.14283441, 0.13504249, 0.12850469, 0.11726064,
           0.04276973, 0.04040376, 0.03865599, 0.03184924, 0.03035828,
           0.02326079, 0.02067900, 0.01676120, 0.01601219, 0.00819076),
  Cover = c(0.167132304, 0.192543555, 0.222294178, 0.058962233, 0.127515423,
            0.032349290, 0.026258744, 0.046826687, 0.028825870, 0.015160933,
            0.015310196, 0.008117841, 0.038588927, 0.013590607, 0.006523212),
  Frequency = c(0.20000000, 0.08157895, 0.07982456, 0.14824561, 0.13859649,
                0.06052632, 0.05263158, 0.05614035, 0.03859649, 0.03859649,
                0.02807018, 0.02017544, 0.02192982, 0.02280702, 0.01228070)
)

# Afficher le dataframe
print(feature_importance)



# Affichage des erreurs MAE
print(paste("GBM MAE:", gbm_mae))
print(paste("XGBoost MAE:", xgb_mae))
print(paste("MAE pour glmboost:", glmboost_mae))
print(paste("MAE pour LightGBM:", lightgbm_mae))

boxplot(dfclean$imbalance_size)




# Initialisation d'une liste pour stocker les modèles
models <- list()

# Initialisation d'une liste pour stocker les erreurs MAE de chaque modèle
mae_errors <- numeric(47)

# Boucle sur les périodes de 10 jours, pour un total de 47 itérations
for (i in 0:46) {
  # Définition des intervalles de jours pour l'entraînement et le test
  train_start_day <- i * 10
  test_start_day <- train_start_day + 10
  
  # Sélection des données d'entraînement et de test
  train_data <- filter(dfclean, date_id > train_start_day & date_id <= train_start_day + 10)
  test_data <- filter(dfclean, date_id > test_start_day & date_id <= test_start_day + 10)
  
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
    objective = "regression_l1",  # MAE
    metric = "mae",
    num_leaves = 31,
    learning_rate = 0.05,
    n_estimators = 100
  )
  
  # Entraînement du modèle LightGBM
  lightgbm_model <- lgb.train(params, dtrain, valids = list(test = dtest), verbose = 0)
  
  # Stockage du modèle
  models[[i + 1]] <- lightgbm_model
  
  # Prédiction avec le modèle LightGBM
  lightgbm_predictions <- predict(lightgbm_model, as.matrix(test_x))
  
  # Calcul et stockage de l'erreur MAE
  mae_errors[i + 1] <- mean(abs(test_y - lightgbm_predictions))
}

# Affichage des erreurs MAE
print(mae_errors)

# Examiner les erreurs MAE pour évaluer la performance de chaque modèle sur son ensemble de test correspondant.
mean(mae_errors)




library(lightgbm)

# Initialisation d'une liste pour stocker les modèles
models_new <- list()

# Initialisation d'une liste pour stocker les erreurs MAE de chaque modèle
mae_errors_new <- numeric(47)

# Boucle sur les périodes de 10 jours, pour un total de 47 itérations
for (i in 0:46) {
  # Définition des intervalles de jours pour l'entraînement et le test
  train_end_day <- (i + 1) * 10
  test_start_day <- train_end_day + 1
  test_end_day <- test_start_day + 9
  
  # Sélection des données d'entraînement et de test
  train_data <- dfclean[dfclean$date_id <= train_end_day, ]
  test_data <- dfclean[dfclean$date_id >= test_start_day & dfclean$date_id <= test_end_day, ]
  
  # Séparation des caractéristiques et de la cible
  train_x <- train_data[, setdiff(names(train_data), "target")]
  train_y <- train_data[["target"]]
  test_x <- test_data[, setdiff(names(test_data), "target")]
  test_y <- test_data[["target"]]
  
  # Préparation des données pour LightGBM
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
  
  # Stockage du modèle
  models_new[[i + 1]] <- lightgbm_model
  
  # Prédiction avec le modèle LightGBM
  lightgbm_predictions <- predict(lightgbm_model, as.matrix(test_x))
  
  # Calcul et stockage de l'erreur MAE
  mae_errors_new[i + 1] <- mean(abs(test_y - lightgbm_predictions))
}

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



library(randomForest)

# Fonction pour prédire et remplacer les valeurs NA pour une colonne donnée (ici `far_price` ou `near_price`)
predict_and_replace_NA <- function(data, target_col) {
  # Séparer les données en deux ensembles: un où la cible n'est pas NA et l'autre où elle est NA
  data_with_target <- data[!is.na(data[[target_col]]), ]
  data_without_target <- data[is.na(data[[target_col]]), ]
  
  # Déterminer les colonnes à utiliser comme prédicteurs
  predictors_cols <- setdiff(names(data), c(target_col, "wap")) # Exclure 'wap' si nécessaire ou d'autres colonnes non pertinentes
  
  # Construction du modèle de forêt aléatoire
  rf_model <- randomForest(x = data_with_target[predictors_cols], y = data_with_target[[target_col]], na.action = na.omit)
  
  # Prédiction des valeurs manquantes
  predicted_values <- predict(rf_model, newdata = data_without_target[predictors_cols])
  
  # Remplacement des valeurs NA par les valeurs prédites dans l'ensemble de données original
  data[[target_col]][is.na(data[[target_col]])] <- predicted_values
  
  return(data)
}


df00_filtered <- data_all[data_all$stock_id < 10, ]

# Application de la fonction pour 'far_price' et 'near_price'
data_2 <- predict_and_replace_NA(df00_filtered, "far_price")
data_3 <- predict_and_replace_NA(data_2, "near_price")

# Vérification des résultats
head(data_3)





# Fonction pour prédire et remplacer les valeurs NA d'une colonne cible
predict_and_replace_NA <- function(df, target_col) {
  # Séparer les données en deux ensembles: avec et sans valeurs NA pour la colonne cible
  train_data <- df %>% filter(!is.na(.data[[target_col]]))
  predict_data <- df %>% filter(is.na(.data[[target_col]]))
  
  # Préparer les données pour l'entraînement de LightGBM
  dtrain <- lgb.Dataset(data = as.matrix(train_data %>% select(-target_col)), label = train_data[[target_col]])
  
  # Paramètres pour LightGBM
  params <- list(objective = "regression_l1", metric = "mae")
  
  # Entraîner le modèle LightGBM
  model <- lgb.train(params, dtrain, 100) # Le nombre 100 représente le nombre d'itérations
  
  # Prédiction des valeurs manquantes
  predictions <- predict(model, as.matrix(predict_data %>% select(-target_col)))
  
  # Remplacement des valeurs NA par les prédictions
  df[[target_col]][is.na(df[[target_col]])] <- predictions
  
  return(df)
}

# Appliquer la fonction pour 'far_price' et 'near_price'
data_2 <- predict_and_replace_NA(data_all, "far_price")
data_3 <- predict_and_replace_NA(data_2, "near_price")

# Vérifier les résultats
print(data_3)


data_3 <- data_3[!is.na(data_3$wap), ]

# 3. Calculer le pourcentage de valeurs NA après imputation
pourcentage_na_apres <- sapply(data_3, function(x) mean(is.na(x))) * 100
pourcentage_na_apres_df <- as.data.frame(pourcentage_na_apres)
# Afficher le DataFrame vertical
print(pourcentage_na_apres_df)

library(ggplot2)


# Densité pour 'far_price' avant et après le remplacement

ggplot() +
  geom_density(data = data_all, aes(x = far_price, fill = "Data originale"), alpha = 0.5) +
  geom_density(data = data_3, aes(x = far_price, fill = "Data LGBM"), alpha = 0.5) +
  geom_density(data = dfclean, aes(x = far_price, fill = "Méthode simple"), alpha = 0.5) +
  ggtitle("Densité de 'far_price' avant et après le remplacement") +
  labs(x = "far Price", y = "Densité") +
  scale_fill_manual(values = c("Data LGBM" = "red","Data originale" = "blue", "Méthode simple" = "yellow"),
                    name = "Légende:", 
                    labels = c("Data LGBM", "Data originale", "Méthode simple")) +
  coord_cartesian(xlim = c(0, 3)) +
  theme(legend.position = "bottom") # Changer la position de la légende si nécessaire

# Densité pour 'near_price' avant et après le remplacement

ggplot() +
  geom_density(data = data_all, aes(x = near_price, fill = "Data originale"), alpha = 0.5) +
  geom_density(data = data_3, aes(x = near_price, fill = "Data LGBM"), alpha = 0.5) +
  geom_density(data = dfclean, aes(x = near_price, fill = "Méthode simple"), alpha = 0.5) +
  ggtitle("Densité de 'near_price' avant et après le remplacement") +
  labs(x = "Near Price", y = "Densité") +
  scale_fill_manual(values = c("Data LGBM" = "red","Data originale" = "blue", "Méthode simple" = "yellow"),
                    name = "Légende:", 
                    labels = c("Data LGBM", "Data originale", "Méthode simple")) +
  coord_cartesian(xlim = c(0.9, 1.1)) +
  theme(legend.position = "bottom") # Changer la position de la légende si nécessaire


# Moyenne et variance pour 'far_price' avant et après
mean_far_original <- mean(data_all$far_price, na.rm = TRUE)
var_far_original <- var(data_all$far_price, na.rm = TRUE)

mean_far_met <- mean(dfclean$far_price, na.rm = TRUE)
var_far_met <- var(dfclean$far_price, na.rm = TRUE)

mean_far_filled <- mean(data_3$far_price, na.rm = TRUE)
var_far_filled <- var(data_3$far_price, na.rm = TRUE)

# Moyenne et variance pour 'near_price' avant et après
mean_near_original <- mean(data_all$near_price, na.rm = TRUE)
var_near_original <- var(data_all$near_price, na.rm = TRUE)

mean_near_met <- mean(dfclean$near_price, na.rm = TRUE)
var_near_met <- var(dfclean$near_price, na.rm = TRUE)

mean_near_filled <- mean(data_3$near_price, na.rm = TRUE)
var_near_filled <- var(data_3$near_price, na.rm = TRUE)

# Affichage des résultats
cat("Far Price - Original: Moyenne =", mean_far_original, ", Variance =", var_far_original, "\n")
cat("Far Price - methode: Moyenne =", mean_far_met, ", Variance =", var_far_met, "\n")
cat("Far Price - Filled: Moyenne =", mean_far_filled, ", Variance =", var_far_filled, "\n")
cat("Near Price - Original: Moyenne =", mean_near_original, ", Variance =", var_near_original, "\n")
cat("Near Price - methode: Moyenne =", mean_near_met, ", Variance =", var_near_met, "\n")
cat("Near Price - Filled: Moyenne =", mean_near_filled, ", Variance =", var_near_filled, "\n")

# Créer un dataframe à partir des statistiques fournies
price_stats <- data.frame(
  Price = c("Far Price - Original", "Far Price - Méthode", "Far Price - Filled",
            "Near Price - Original", "Near Price - Méthode", "Near Price - Filled"),
  Moyenne = c(1.001713, 1.000772, 0.97374, 0.9996601, 0.9998419, 0.9861606),
  Variance = c(0.5205197, 0.2329097, 0.2335798, 0.0001480894, 4.803862e-05, 0.0002764947)
)

# Afficher le dataframe
print(price_stats)







# 1. Détection des valeurs aberrantes pour 'ask_price'
ask_price <- data_all$ask_price %>% na.omit() %>% sort()

# Calcul des percentiles 25 et 75
s_25 <- quantile(ask_price, 0.25)
s_75 <- quantile(ask_price, 0.75)

# Calcul de l'IQR
iqr <- s_75 - s_25

# Définition des limites pour les valeurs aberrantes
s_lower <- max(s_25 - 1.5 * iqr, min(ask_price))
s_higher <- min(s_75 + 1.5 * iqr, max(ask_price))

# Filtrage des valeurs aberrantes inférieures
outlier_lower <- ask_price[ask_price < s_lower]

outlier_lower

# Filtrage des valeurs aberrantes supérieures
outlier_higher <- ask_price[ask_price > s_higher]

outlier_higher

ajout_detect_outliers <- function(data_frame) {
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
  
  # Ajouter une colonne 'has_outlier' pour marquer les lignes avec au moins un outlier
  #outliers$has_outlier <- apply(outliers, 1, function(row) as.integer(any(row)))
  data_frame$has_outlier <- apply(outliers, 1, function(row) as.integer(any(row)))
  
  return(data_frame)
}

# Appliquer la fonction au dataframe 'data_all' 
dfclean_outliers <- ajout_detect_outliers(dfclean)
print(head(dfclean_outliers))

# Initialisation d'une liste pour stocker les modèles
models_out <- list()

# Initialisation d'une liste pour stocker les erreurs MAE de chaque modèle
mae_errors_out <- numeric(47)

# Boucle sur les périodes de 10 jours, pour un total de 47 itérations
for (i in 0:46) {
  # Définition des intervalles de jours pour l'entraînement et le test
  train_start_day <- i * 10
  test_start_day <- train_start_day + 10
  
  # Sélection des données d'entraînement et de test
  train_data <- filter(dfclean_outliers, date_id > train_start_day & date_id <= train_start_day + 10)
  test_data <- filter(dfclean_outliers, date_id > test_start_day & date_id <= test_start_day + 10)
  
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
    objective = "regression_l1",  # MAE
    metric = "mae",
    num_leaves = 31,
    learning_rate = 0.05,
    n_estimators = 100
  )
  
  # Entraînement du modèle LightGBM
  lightgbm_model <- lgb.train(params, dtrain, valids = list(test = dtest), verbose = 1)
  
  # Stockage du modèle
  models_out[[i + 1]] <- lightgbm_model
  
  # Prédiction avec le modèle LightGBM
  lightgbm_predictions <- predict(lightgbm_model, as.matrix(test_x))
  
  # Calcul et stockage de l'erreur MAE
  mae_errors_out[i + 1] <- mean(abs(test_y - lightgbm_predictions))
}

# Affichage des erreurs MAE
print(mae_errors_out)
print(mae_errors)
# Examiner les erreurs MAE pour évaluer la performance de chaque modèle sur son ensemble de test correspondant.
mean(mae_errors_out)
mean(mae_errors)

# Compare the two lists element-wise
comparison <- mae_errors >= mae_errors_out

# Convert the TRUE/FALSE values to 1/0
comparison_values <- as.integer(comparison)

# Calculate the proportion of 1's in the comparison
proportion_of_ones <- sum(comparison_values) / length(comparison_values)

# Return both the comparison list and the proportion
list(comparison_values = comparison_values, proportion_of_ones = proportion_of_ones)

difference <- mae_errors - mae_errors_out
difference
print(max(difference))
print(min(difference))
print(mean(difference))



# Initialisation d'une liste pour stocker les modèles
models_new_out <- list()

# Initialisation d'une liste pour stocker les erreurs MAE de chaque modèle
mae_errors_new_out <- numeric(47)

# Boucle sur les périodes de 10 jours, pour un total de 47 itérations
for (i in 0:46) {
  # Définition des intervalles de jours pour l'entraînement et le test
  train_end_day <- (i + 1) * 10
  test_start_day <- train_end_day + 1
  test_end_day <- test_start_day + 9
  
  # Sélection des données d'entraînement et de test
  train_data <- dfclean_outliers[dfclean_outliers$date_id <= train_end_day, ]
  test_data <- dfclean_outliers[dfclean_outliers$date_id >= test_start_day & dfclean$date_id <= test_end_day, ]
  
  # Séparation des caractéristiques et de la cible
  train_x <- train_data[, setdiff(names(train_data), "target")]
  train_y <- train_data[["target"]]
  test_x <- test_data[, setdiff(names(test_data), "target")]
  test_y <- test_data[["target"]]
  
  # Préparation des données pour LightGBM
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
  
  # Stockage du modèle
  models_new_out[[i + 1]] <- lightgbm_model
  
  # Prédiction avec le modèle LightGBM
  lightgbm_predictions <- predict(lightgbm_model, as.matrix(test_x))
  
  # Calcul et stockage de l'erreur MAE
  mae_errors_new_out[i + 1] <- mean(abs(test_y - lightgbm_predictions))
}

# Affichage des erreurs MAE
print(mae_errors_new_out)
print(mae_errors_out)
print(mae_errors_new)
print(mae_errors)
# Calcul de la moyenne des erreurs MAE
mean(mae_errors_new_out)
mean(mae_errors_out)
mean(mae_errors_new)
mean(mae_errors)



detect_outliers <- function(data_frame) {
  # Créer un dataframe vide pour stocker les résultats
  outliers <- data.frame(matrix(ncol = ncol(data_frame), nrow = nrow(data_frame)))
  
  # Nommer les colonnes comme dans le dataframe original
  names(outliers) <- names(data_frame)
  
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
      outliers[[col_name]] <- !(data_frame[[col_name]] >= lower_bound & data_frame[[col_name]] <= upper_bound)
    } else {
      # Pour les colonnes non numériques, marquer toutes les valeurs comme FALSE
      outliers[[col_name]] <- FALSE
    }
  }
  
  return(outliers)
}

# Exemple d'utilisation avec le dataframe `data_all`
outlier_flags <- detect_outliers(dfclean)
print(outlier_flags)

# Filtrez pour ne garder que les lignes où 'far_price' est un outlier
outlier_rows <- outlier_flags %>% 
  filter(imbalance_size == TRUE) %>%
  select(-starts_with("imbalance_size")) # Supprime la colonne d'outlier pour 'far_price' si nécessaire

# Affichez ou utilisez les lignes filtrées
print(outlier_rows)



# Calcul du nombre d'outliers pour chaque colonne
number_of_outliers <- colSums(outlier_flags == TRUE)

# Afficher le nombre d'outliers pour chaque colonne
print(number_of_outliers)

number_of_outliers <- c(stock_id = 0, date_id = 0, seconds_in_bucket = 0, 
                        imbalance_size = 619374, imbalance_buy_sell_flag = 0, 
                        reference_price = 325886, matched_size = 637237, 
                        far_price = 901102, near_price = 702055, bid_price = 324025, 
                        bid_size = 509946, ask_price = 321691, ask_size = 505016, 
                        wap = 325523, target = 292257, time_id = 0, row_id = 0)

# Convertir ce vecteur en dataframe
outliers_df <- data.frame(
  Variable = names(number_of_outliers),
  Number_of_Outliers = number_of_outliers
)

# Nombre total de lignes
total_rows <- 5237760

# Ajout d'une colonne pourcentage dans le dataframe
outliers_df$Percentage <- (outliers_df$Number_of_Outliers / total_rows) * 100

# Afficher le dataframe avec la nouvelle colonne pourcentage
print(outliers_df)


# Compter le nombre de lignes avec au moins un outlier
number_of_rows_with_outlier <- sum(apply(outlier_flags, 1, function(x) any(x == TRUE)))

# Afficher le résultat
print(number_of_rows_with_outlier)


# Compter le nombre d'outliers dans chaque ligne
number_of_outliers_per_row <- rowSums(outlier_flags)

# Afficher le résultat
print(number_of_outliers_per_row)

outliers_count <- table(number_of_outliers_per_row)
print(outliers_count)

# Créer un dataframe à partir des informations fournies
outliers_count_df <- data.frame(
  Number_of_Outliers = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
  Frequency = c(2812825, 1025790, 776123, 219510, 126950, 61039, 131216, 
                55397, 21827, 6034, 985, 64)
)

# Afficher le dataframe
print(outliers_count_df)


# Calculer le nombre d'outliers par colonne
number_of_outliers_per_column <- colSums(outlier_flags)

# Créer un dataframe pour afficher les résultats
outliers_summary <- data.frame(
  Column = names(number_of_outliers_per_column),
  NumberOfOutliers = number_of_outliers_per_column
)

# Afficher le résumé
print(outliers_summary)







# Calculer un indicateur pour chaque ligne indiquant si cette ligne contient au moins un outlier
row_has_outlier <- apply(outlier_flags, 1, function(row) any(row == TRUE))

# Filtrer data_all pour ne garder que les lignes avec au moins un outlier
data_with_outliers <- dfclean[row_has_outlier, ]
outlier_flags_with <- outlier_flags[row_has_outlier, ]

# Étape 2: Enlever les colonnes où il n'y a aucun outlier
col_has_outlier <- apply(outlier_flags, 2, any) # `any` vérifie si au moins un TRUE existe dans la colonne
data_filtered_with_outliers <- data_with_outliers[, col_has_outlier]
outlier_filtered_flags_with <- outlier_flags_with[,col_has_outlier ]

















# Fonction pour compter les outliers communs entre deux colonnes
count_common_outliers <- function(col_names) {
  col1 <- col_names[1]
  col2 <- col_names[2]
  sum(outlier_filtered_flags_with[[col1]] & outlier_filtered_flags_with[[col2]])
}

# Générer toutes les combinaisons possibles de colonnes (deux par deux)
column_combinations <- combn(names(outlier_filtered_flags_with), 2, simplify = FALSE)

# Appliquer la fonction pour chaque paire et compter les outliers communs
outliers_counts <- sapply(column_combinations, count_common_outliers)

# Associer les résultats avec les noms des paires de colonnes
names(outliers_counts) <- sapply(column_combinations, function(cols) paste(cols, collapse = " & "))

# Afficher les résultats
print(outliers_counts)

# Ordonner outliers_counts en descendant
outliers_counts_sorted <- sort(outliers_counts, decreasing = TRUE)

# Afficher les résultats ordonnés
print(outliers_counts_sorted)


# Données fournies comme vecteur nommé
outliers_counts_sorted <- c(
  `far_price & near_price` = 674081, 
  `imbalance_size & matched_size` = 312554, 
  `reference_price & bid_price` = 305381,
  `bid_price & wap` = 302090, 
  `ask_price & wap` = 300117,
  `reference_price & wap` = 299753,
  `reference_price & ask_price` = 289430,
  `bid_price & ask_price` = 286183,
  `reference_price & near_price` = 227123,
  `bid_size & ask_size` = 224277,
  `near_price & wap` = 222545,
  `near_price & bid_price` = 221050,
  `near_price & ask_price` = 220727,
  `reference_price & far_price` = 210295,
  `far_price & wap` = 205442,
  `far_price & bid_price` = 204529,
  `far_price & ask_price` = 203645,
  `matched_size & far_price` = 140684,
  `matched_size & near_price` = 119299,
  `matched_size & ask_size` = 107325,
  `matched_size & bid_size` = 106097,
  `far_price & ask_size` = 103693,
  `far_price & bid_size` = 102601,
  `imbalance_size & far_price` = 93670,
  `imbalance_size & near_price` = 85204,
  `near_price & ask_size` = 67530,
  `imbalance_size & ask_size` = 66944,
  `imbalance_size & bid_size` = 66477,
  `near_price & bid_size` = 66387,
  `far_price & target` = 58079,
  `near_price & target` = 53309,
  `wap & target` = 48565,
  `bid_price & target` = 48443,
  `reference_price & target` = 48405,
  `ask_price & target` = 48306,
  `matched_size & wap` = 46437,
  `matched_size & bid_price` = 46175,
  `reference_price & matched_size` = 46156,
  `matched_size & ask_price` = 45753,
  `reference_price & bid_size` = 34602,
  `reference_price & ask_size` = 33756,
  `imbalance_size & target` = 33732,
  `bid_price & bid_size` = 33732,
  `bid_price & ask_size` = 33067,
  `bid_size & wap` = 33043,
  `ask_price & ask_size` = 32958,
  `ask_size & wap` = 32957,
  `bid_size & ask_price` = 32637,
  `imbalance_size & wap` = 30732,
  `imbalance_size & bid_price` = 30574,
  `imbalance_size & reference_price` = 30298,
  `imbalance_size & ask_price` = 30241,
  `matched_size & target` = 22866,
  `bid_size & target` = 22600,
  `ask_size & target` = 21989
)

# Convertir ce vecteur nommé en dataframe
outliers_counts_df <- data.frame(
  Variable_Pair = names(outliers_counts_sorted),
  Count = as.integer(outliers_counts_sorted)  # Conversion en entiers pour éviter les formats scientifiques
)

# Afficher le dataframe
print(outliers_counts_df)








library(pheatmap)
n <- 100

# Transformer les outliers_counts en une matrice carrée
# Initialiser une matrice de zéros
outliers_matrix <- matrix(0, nrow = length(names(data_filtered_with_outliers)), ncol = length(names(data_filtered_with_outliers)), 
                          dimnames = list(names(data_filtered_with_outliers), names(data_filtered_with_outliers)))

# Remplir la matrice avec les valeurs de outliers_counts
for (comb in names(outliers_counts)) {
  cols <- unlist(strsplit(comb, " & "))
  outliers_matrix[cols[1], cols[2]] <- outliers_counts[comb]
  outliers_matrix[cols[2], cols[1]] <- outliers_counts[comb] # Pour la symétrie
}

# Visualiser avec heatmap()
heatmap(outliers_matrix, 
        Rowv = NA, Colv = NA, # Empêcher le re-arrangement des lignes/colonnes
        main = "Heatmap of Common Outliers Between Columns",
        xlab = "Columns", ylab = "Columns",
        col = heat.colors(256), # Utiliser une palette de couleurs
        scale = "none") # Ne pas normaliser les données

print(outliers_matrix)
pheatmap(outliers_matrix, color = colorRampPalette(c("white", "red"))(n))







# Initialiser une liste pour stocker les résultats
triplets_outliers_counts <- list()

# Générer toutes les combinaisons de trois colonnes
column_combinations <- combn(names(outlier_filtered_flags_with), 3, simplify = FALSE)

# Parcourir chaque combinaison de trois colonnes
for(comb in column_combinations) {
  # Calculer le nombre de lignes où les trois colonnes sont des outliers
  count <- sum(apply(outlier_filtered_flags_with[comb], 1, function(row) all(row)))
  
  # Ajouter le résultat à la liste avec le nom des colonnes comme clé
  triplets_outliers_counts[[paste(comb, collapse = " & ")]] <- count
}

# Ordonner outliers_counts en descendant
triplets_outliers_counts_sorted <- sort(unlist(triplets_outliers_counts), decreasing = TRUE)

# Afficher les résultats
print(triplets_outliers_counts_sorted)




print(outliers_summary)




nombre_lignes_depasse_10_2 <- sum(dfclean$imbalance_size > 10^7, na.rm = TRUE)
print(nombre_lignes_depasse_10_2)
nombre_lignes_depasse_10_2 <- sum(dfclean$reference_price > 1.005 | dfclean$reference_price < 0.995	, na.rm = TRUE)
print(nombre_lignes_depasse_10_2)
nombre_lignes_depasse_10_2 <- sum(dfclean$matched_size > 7.4*10^7, na.rm = TRUE)
print(nombre_lignes_depasse_10_2)
nombre_lignes_depasse_10_2 <- sum(dfclean$far_price > 1.00245, na.rm = TRUE)
print(nombre_lignes_depasse_10_2)
nombre_lignes_depasse_10_2 <- sum(dfclean$near_price > 1.005, na.rm = TRUE)
print(nombre_lignes_depasse_10_2)
nombre_lignes_depasse_10_2 <- sum(dfclean$bid_price > 1.005, na.rm = TRUE)
print(nombre_lignes_depasse_10_2)
nombre_lignes_depasse_10_2 <- sum(dfclean$bid_size > 1.5*10^5, na.rm = TRUE)
print(nombre_lignes_depasse_10_2)
nombre_lignes_depasse_10_2 <- sum(dfclean$ask_price > 1.005, na.rm = TRUE)
print(nombre_lignes_depasse_10_2)
nombre_lignes_depasse_10_2 <- sum(dfclean$ask_size > 1.3*10^5, na.rm = TRUE)
print(nombre_lignes_depasse_10_2)
nombre_lignes_depasse_10_2 <- sum(dfclean$wap > 1.005, na.rm = TRUE)
print(nombre_lignes_depasse_10_2)
nombre_lignes_depasse_10_2 <- sum(dfclean$target > 13.5, na.rm = TRUE)
print(nombre_lignes_depasse_10_2)




# Calculer le nombre d'outliers dans chaque ligne
number_of_outliers_per_row <- rowSums(outlier_flags)

# Filtrer pour ne garder que les lignes avec exactement 2 outliers
data_with_exactly_two_outliers <- dfclean[number_of_outliers_per_row == 2, ]

# Filtrer pour ne garder que les lignes avec exactement 0 outliers
data_with_exactly_zero_outliers <- dfclean[number_of_outliers_per_row == 0, ]

# Filtrer pour ne garder que les lignes avec plus que 2 outliers
data_with_exactly_more_two_outliers <- dfclean[number_of_outliers_per_row > 2, ]


# Afficher la densité de la colonne 'target' dans data_with_exactly_two_outliers
ggplot(data_with_exactly_two_outliers, aes(x = target)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  ggtitle("Densité de la colonne 'target' avec exactement 2 outliers") + 
  xlab("Valeur de Target") + 
  ylab("Densité")

# Afficher la densité de la colonne 'target' dans data_with_exactly_two_outliers
ggplot(data_with_exactly_zero_outliers, aes(x = target)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  ggtitle("Densité de la colonne 'target' avec exactement 0 outliers") + 
  xlab("Valeur de Target") + 
  ylab("Densité")

# Afficher la densité de la colonne 'target' dans data_with_more_two_outliers
ggplot(data_with_exactly_more_two_outliers, aes(x = target)) + 
  geom_density(fill = "blue", alpha = 0.5) + 
  ggtitle("Densité de la colonne 'target' avec plus que 2 outliers") + 
  xlab("Valeur de Target") + 
  ylab("Densité")



# Ajouter une colonne 'group' à chaque dataframe pour indiquer le nombre d'outliers
data_with_exactly_two_outliers$group <- "Exactement 2 outliers"
data_with_exactly_zero_outliers$group <- "Exactement 0 outlier"
data_with_exactly_more_two_outliers$group <- "Plus de 2 outliers"

# Combiner les trois dataframes
combined_data <- bind_rows(data_with_exactly_two_outliers, 
                           data_with_exactly_zero_outliers, 
                           data_with_exactly_more_two_outliers)

# Tracer la densité de 'target' pour chaque groupe
ggplot(combined_data, aes(x = target, fill = group)) + 
  geom_density(alpha = 0.5) + 
  ggtitle("Densité de la colonne 'target' par nombre d'outliers") + 
  xlab("Valeur de Target") + 
  ylab("Densité") +
  scale_fill_manual(values = c("Exactement 2 outliers" = "blue", 
                               "Exactement 0 outlier" = "green", 
                               "Plus de 2 outliers" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())











install.packages("forecast")
library(forecast)

# Ajustement d'un modèle ARIMA automatique
arima_model <- auto.arima(dfclean$target)

# Faire des prédictions
predictions <- forecast(arima_model, h=length(dfclean$target))

# Calculer les résidus
residuals <- residuals(arima_model)

# Identifier les anomalies basées sur un seuil, par exemple 2 écarts-types
threshold <- 2 * sd(residuals)
anomalies <- which(abs(residuals) > threshold)
dfclean$anomaly <- ifelse(abs(residuals) > threshold, TRUE, FALSE)

ggplot(dfclean, aes(x = seq_along(target), y = target)) +
  geom_line() +
  geom_point(aes(color = anomaly), size = 2) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  ggtitle("Détection des anomalies dans les séries temporelles") +
  xlab("Temps") + ylab("Valeur de Target")
residuals
# Calculer le MAE
mae <- mean(abs(residuals))

# Afficher le MAE
print(mae)








# Calcul des résidus du modèle ARIMA
residus_arima <- residuals(arima_model)

# Identification des anomalies basées sur des critères, par exemple, les résidus dépassant 2 écarts-types
ecart_type <- sd(residus_arima)
seuil_anomalie <- 2 * ecart_type

# Identifier les points de données où les résidus dépassent ce seuil
anomalies_indices <- which(abs(residus_arima) > seuil_anomalie)

# Extraire les dates/times correspondant à ces anomalies
dates_anomalies <- time(dfclean)[anomalies_indices]

# Analyse contextuelle des anomalies

# Convertir les dates des anomalies en format approprié si nécessaire
dates_anomalies <- as.Date(dates_anomalies)

# Filtrer les données pour obtenir les informations contextuelles autour des dates d'anomalies
data_anomalies_context <- data_financial %>% 
  filter(date %in% dates_anomalies) %>%
  arrange(date)

# Examinez les données contextuelles pour comprendre les causes potentielles des anomalies
print(data_anomalies_context)

# Optionnellement, visualiser les anomalies sur la série temporelle
ggplot(data_ts, aes(x = time(data_ts), y = as.numeric(data_ts))) +
  geom_line() +
  geom_point(data = filter(data_ts, time(data_ts) %in% dates_anomalies), aes(x = time(data_ts), y = as.numeric(data_ts)), color = "red") +
  ggtitle("Visualisation des Anomalies dans les Données Financières") +
  xlab("Date") + ylab("Valeur")










# Chargement des packages nécessaires
library(dbscan)
library(ggplot2)
library(Rtsne)  # Pour t-SNE
# library(FactoMineR)  # Pour PCA si on utilise PCA

# Sélectionner uniquement les colonnes numériques de dfclean
dfclean_numeric <- dfclean[sapply(dfclean, is.numeric)]

# Mise à l'échelle des données numériques
data_for_clustering <- scale(dfclean_numeric)

# Application de DBSCAN
dbscan_result <- dbscan(data_for_clustering, eps = 0.5, minPts = 10)

# Réduction de dimensionnalité avec t-SNE
tsne_result <- Rtsne(data_for_clustering, dims = 2, perplexity = 30)

# Création d'un dataframe pour la visualisation
visualization_df <- data.frame(
  X = tsne_result$Y[,1],
  Y = tsne_result$Y[,2],
  cluster = as.factor(dbscan_result$cluster)
)

# Visualisation des résultats
ggplot(visualization_df, aes(x = X, y = Y, color = cluster)) +
  geom_point(alpha = 0.7) +
  scale_color_discrete(name = "Cluster") +
  theme_minimal() +
  ggtitle("DBSCAN Clustering with t-SNE Visualization")










