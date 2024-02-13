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
# Assurez-vous que l'ensemble de données `train` a été correctement chargé et préparé en R
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
  filter(date_id %in% 0:20)

# Sélectionner les données de test (les 10 jours suivants, soit les jours 11 à 20)
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

# Vous pouvez ensuite examiner les erreurs MAE pour évaluer la performance de chaque modèle sur son ensemble de test correspondant.
mean(mae_errors)






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

# Application de la fonction pour 'far_price' et 'near_price'
data_2 <- predict_and_replace_NA(data_all, "far_price")
data_3 <- predict_and_replace_NA(data_2, "near_price")

# Vérification des résultats
head(data_3)

