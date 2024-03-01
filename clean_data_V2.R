
#density target sur les valeurs outlyer et non outlyer (ligne 1149)
#ajout colonne outlier  (ligne 723)



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


# Liste pour stocker les objets ggplot
plot_list <- list()

# Créer un objet ggplot pour chaque caractéristique numérique et l'ajouter à la liste
for (feature in num_features) {
  p <- ggplot(dfclean, aes_string(y = feature)) +
    geom_boxplot(fill = "blue", alpha = 0.5) +
    labs(title = paste(feature, "Distribution")) +
    theme_minimal() +
    coord_flip()  # Utilisez coord_flip() pour dessiner des boxplots horizontaux
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



# Sélectionner les données d'entraînement (20 premiers jours)
train_data <- dfclean %>%
  filter(date_id %in% 0:20)

# Sélectionner les données de test (les 20 jours suivants, soit les jours 21 à 40)
test_data <- dfclean %>%
  filter(date_id %in% 21:40)

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
  lightgbm_model <- lgb.train(params, dtrain, valids = list(test = dtest), verbose = 1)
  
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
  scale_fill_manual(values = c("Data originale" = "blue", "Data LGBM" = "red", "Méthode simple" = "yellow"),
                    name = "Légende", 
                    labels = c("Data originale", "Data LGBM", "Méthode simple")) +
  coord_cartesian(xlim = c(0, 3)) +
  theme(legend.position = "bottom") # Changer la position de la légende si nécessaire

# Densité pour 'near_price' avant et après le remplacement

ggplot() +
  geom_density(data = data_all, aes(x = near_price, fill = "Data originale"), alpha = 0.5) +
  geom_density(data = data_3, aes(x = near_price, fill = "Data LGBM"), alpha = 0.5) +
  geom_density(data = dfclean, aes(x = near_price, fill = "Méthode simple"), alpha = 0.5) +
  ggtitle("Densité de 'near_price' avant et après le remplacement") +
  labs(x = "Near Price", y = "Densité") +
  scale_fill_manual(values = c("Data originale" = "blue", "Data LGBM" = "red", "Méthode simple" = "yellow"),
                    name = "Légende", 
                    labels = c("Data originale", "Data LGBM", "Méthode simple")) +
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
  test_data <- dfclean_outliers[dfclean_outliers$date_id >= test_start_day & dfclean_outliers$date_id <= test_end_day, ]
  
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
# Remplacer `data_all` par le nom de votre dataframe
outlier_flags <- detect_outliers(dfclean)
outlier_flags$stock_id <- dfclean$stock_id
print(outlier_flags)
# Calculer le nombre total d'outliers pour chaque stock_id
outliers_per_stock <- aggregate(. ~ stock_id, data = outlier_flags, FUN = sum, na.rm = TRUE)

# Afficher le résultat
print(outliers_per_stock)

# Calculer le nombre total d'outliers pour chaque stock_id en sommant les valeurs de chaque ligne (à l'exception de la première colonne qui est stock_id)
outliers_per_stock_total <- transform(outliers_per_stock, Total_Outliers = rowSums(outliers_per_stock[,-1]))

# Sélectionner uniquement les colonnes stock_id et Total_Outliers pour le résumé final
final_summary <- outliers_per_stock_total[, c("stock_id", "Total_Outliers")]

# Afficher le résumé final
print(final_summary)



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


# Calculer le nombre d'outliers par colonne
number_of_outliers_per_column <- colSums(outlier_flags)

# Créer un dataframe pour afficher les résultats (pourcentage)
outliers_summary_pourcentage <- data.frame(
  Column = names(number_of_outliers_per_column),
  NumberOfOutliers = number_of_outliers_per_column/52377.60
)

# Afficher le résumé
print(outliers_summary_pourcentage)

# Créer un dataframe pour afficher les résultats (pourcentage)
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






# Supposons que outliers_marked est votre dataframe marquant les outliers
# Par exemple, outliers_marked$col1 est TRUE si la valeur dans col1 est un outlier

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

# Afficher la densité de la colonne 'target' dans data_with_exactly_zero_outliers
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






ajout_detect_count_outliers <- function(data_frame) {
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
  
  return(data_frame)
}




# Appliquer la fonction au dataframe 'data_all' 
dfclean_count_outliers <- ajout_detect_count_outliers(dfclean)
print(head(dfclean_count_outliers))

# Initialisation d'une liste pour stocker les modèles
models_out_count <- list()

# Initialisation d'une liste pour stocker les erreurs MAE de chaque modèle
mae_errors_out_count <- numeric(47)

# Boucle sur les périodes de 10 jours, pour un total de 47 itérations
for (i in 0:46) {
  # Définition des intervalles de jours pour l'entraînement et le test
  train_start_day <- i * 10
  test_start_day <- train_start_day + 10
  
  # Sélection des données d'entraînement et de test
  train_data <- filter(dfclean_count_outliers, date_id > train_start_day & date_id <= train_start_day + 10)
  test_data <- filter(dfclean_count_outliers, date_id > test_start_day & date_id <= test_start_day + 10)
  
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
  models_out_count[[i + 1]] <- lightgbm_model
  
  # Prédiction avec le modèle LightGBM
  lightgbm_predictions <- predict(lightgbm_model, as.matrix(test_x))
  
  # Calcul et stockage de l'erreur MAE
  mae_errors_out_count[i + 1] <- mean(abs(test_y - lightgbm_predictions))
}



# Initialisation d'une liste pour stocker les modèles
models_new_out_count <- list()

# Initialisation d'une liste pour stocker les erreurs MAE de chaque modèle
mae_errors_new_out_count <- numeric(47)

# Boucle sur les périodes de 10 jours, pour un total de 47 itérations
for (i in 0:46) {
  # Définition des intervalles de jours pour l'entraînement et le test
  train_end_day <- (i + 1) * 10
  test_start_day <- train_end_day + 1
  test_end_day <- test_start_day + 9
  
  # Sélection des données d'entraînement et de test
  train_data <- dfclean_count_outliers[dfclean_count_outliers$date_id <= train_end_day, ]
  test_data <- dfclean_count_outliers[dfclean_count_outliers$date_id >= test_start_day & dfclean_count_outliers$date_id <= test_end_day, ]
  
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
  models_new_out_count[[i + 1]] <- lightgbm_model
  
  # Prédiction avec le modèle LightGBM
  lightgbm_predictions <- predict(lightgbm_model, as.matrix(test_x))
  
  # Calcul et stockage de l'erreur MAE
  mae_errors_new_out_count[i + 1] <- mean(abs(test_y - lightgbm_predictions))
}


# Affichage des erreurs MAE
print(mae_errors_out_count)
print(mae_errors_new_out_count)
# Examiner les erreurs MAE pour évaluer la performance de chaque modèle sur son ensemble de test correspondant.
mean(mae_errors_out_count)
mean(mae_errors_new_out_count)

# Compare the two lists element-wise
comparison_count <- mae_errors_out_count >= mae_errors_new_out_count

# Convert the TRUE/FALSE values to 1/0
comparison_values_count <- as.integer(comparison_count)

# Calculate the proportion of 1's in the comparison
proportion_of_ones_count <- sum(comparison_values_count) / length(comparison_values_count)

# Return both the comparison list and the proportion
list(comparison_values_count = comparison_values_count, proportion_of_ones_count = proportion_of_ones_count)

difference <- mae_errors_out_count - mae_errors_new_out_count
difference
print(max(difference))
print(min(difference))
print(mean(difference))







# Affichage des erreurs MAE
print(mae_errors_new_out_count)
print(mae_errors_new_out)
print(mae_errors_new)

print(mae_errors_out_count)
print(mae_errors_out)
print(mae_errors)


# Calcul de la moyenne des erreurs MAE
mean(mae_errors_new_out_count)
mean(mae_errors_new_out)
mean(mae_errors_new)

mean(mae_errors_out_count)
mean(mae_errors_out)
mean(mae_errors)







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



# Appliquer la fonction au dataframe 'data_all' 
dfclean_pro_outliers <- pro_outliers(dfclean)
print(head(dfclean_pro_outliers))

# Initialisation d'une liste pour stocker les modèles
models_pro <- list()

# Initialisation d'une liste pour stocker les erreurs MAE de chaque modèle
mae_errors_pro <- numeric(47)

# Boucle sur les périodes de 10 jours, pour un total de 47 itérations
for (i in 0:46) {
  # Définition des intervalles de jours pour l'entraînement et le test
  train_start_day <- i * 10
  test_start_day <- train_start_day + 10
  
  # Sélection des données d'entraînement et de test
  train_data <- filter(dfclean_pro_outliers, date_id > train_start_day & date_id <= train_start_day + 10)
  test_data <- filter(dfclean_pro_outliers, date_id > test_start_day & date_id <= test_start_day + 10)
  
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
  models_pro[[i + 1]] <- lightgbm_model
  
  # Prédiction avec le modèle LightGBM
  lightgbm_predictions <- predict(lightgbm_model, as.matrix(test_x))
  
  # Calcul et stockage de l'erreur MAE
  mae_errors_pro[i + 1] <- mean(abs(test_y - lightgbm_predictions))
}



# Initialisation d'une liste pour stocker les modèles
models_new_pro <- list()

# Initialisation d'une liste pour stocker les erreurs MAE de chaque modèle
mae_errors_new_pro <- numeric(47)

# Boucle sur les périodes de 10 jours, pour un total de 47 itérations
for (i in 0:46) {
  # Définition des intervalles de jours pour l'entraînement et le test
  train_end_day <- (i + 1) * 10
  test_start_day <- train_end_day + 1
  test_end_day <- test_start_day + 9
  
  # Sélection des données d'entraînement et de test
  train_data <- dfclean_pro_outliers[dfclean_pro_outliers$date_id <= train_end_day, ]
  test_data <- dfclean_pro_outliers[dfclean_pro_outliers$date_id >= test_start_day & dfclean_pro_outliers$date_id <= test_end_day, ]
  
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
  models_new_pro[[i + 1]] <- lightgbm_model
  
  # Prédiction avec le modèle LightGBM
  lightgbm_predictions <- predict(lightgbm_model, as.matrix(test_x))
  
  # Calcul et stockage de l'erreur MAE
  mae_errors_new_pro[i + 1] <- mean(abs(test_y - lightgbm_predictions))
}


# Affichage des erreurs MAE
print(mae_errors_pro)
print(mae_errors_new_pro)
# Examiner les erreurs MAE pour évaluer la performance de chaque modèle sur son ensemble de test correspondant.
mean(mae_errors_pro)
mean(mae_errors_new_pro)

# Compare the two lists element-wise
comparison_pro <- mae_errors_pro >= mae_errors_new_pro

# Convert the TRUE/FALSE values to 1/0
comparison_values_pro <- as.integer(comparison_pro)

# Calculate the proportion of 1's in the comparison
proportion_of_ones_pro <- sum(comparison_values_pro) / length(comparison_values_pro)

# Return both the comparison list and the proportion
list(comparison_values_pro = comparison_values_pro, proportion_of_ones_pro = proportion_of_ones_pro)

difference <- mae_errors_pro - mae_errors_new_pro
difference
print(max(difference))
print(min(difference))
print(mean(difference))






# Affichage des erreurs MAE
print(mae_errors_new_pro)
print(mae_errors_new_out_count)
print(mae_errors_new_out)
print(mae_errors_new)

print(mae_errors_pro)
print(mae_errors_out_count)
print(mae_errors_out)
print(mae_errors)


# Calcul de la moyenne des erreurs MAE
mean(mae_errors_new_pro)
mean(mae_errors_new_out_count)
mean(mae_errors_new_out)
mean(mae_errors_new)

mean(mae_errors_pro)
mean(mae_errors_out_count)
mean(mae_errors_out)
mean(mae_errors)

 






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