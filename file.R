install.packages(c("tidyr", "readxl", "dplyr", "ggplot2", "patchwork", "ggcorrplot", 
                   "glmnet", "compositions", "factoextra", "plotly", "fmsb", "caret", 
                   "reshape2", "randomForest"))


library(caret)
library(reshape2)
library(randomForest)
library(tidyr)
library(compositions)
library(factoextra)
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggcorrplot)
library(glmnet)
library(plotly)
library(fmsb)



file_path <- "C:/Users/HP/Desktop/demos-main/data_abs.xlsx"
data <- read_excel(file_path)
summary(data)




# Trier par taux d'abstention descendant pour les plus hauts taux
top_10_high <- data %>% arrange(desc(txabs)) %>% head(10)
print("Top 10 départements avec le plus haut taux d'abstention :")
print(top_10_high)

# Trier par taux d'abstention croissant pour les plus faibles taux
top_10_low <- data %>% arrange(txabs) %>% head(10)
print("Top 10 départements avec le plus faible taux d'abstention :")
print(top_10_low)

# Ajouter une colonne "group" pour différencier les groupes (haut taux et faible taux)
top_10_high$group <- "Fort taux"
top_10_low$group <- "Faible taux"

# Combiner les deux sous-ensembles en un seul pour la visualisation
combined_data <- rbind(top_10_high, top_10_low)

# Créer le graphique combiné avec ggplot2
ggplot(combined_data, aes(x = reorder(Department, txabs), y = txabs, fill = group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Départements avec les plus forts et plus faibles taux d'abstention",
       x = "Département", y = "Taux d'abstention (%)") +
  scale_fill_manual(values = c("Fort taux" = "red", "Faible taux" = "blue")) +
  theme_minimal() +
  theme(legend.position = "right")

# Créer une colonne factorisée qui fixe l'ordre des départements en fonction du taux d'abstention
combined_data$DepartmentOrder <- factor(combined_data$Department, levels = combined_data$Department[order(combined_data$txabs)])

# Liste des variables que nous souhaitons visualiser
variables <- c("NonDiplome", "txcho", "TxPauv", "HLM", "Salairemoy", "Ouvrier", "Employe", "PI", "Cadres", "Artisant", "Agri")

# Boucle pour générer des graphiques
for (variable in variables) {
  
  # Créer un graphique pour chaque variable de la liste avec l'ordre des départements basé sur le taux d'abstention
  p <- ggplot(combined_data, aes(x = DepartmentOrder, y = get(variable), fill = group)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = paste("Comparaison de", variable, "dans les départements"),
         x = "Département", y = variable) +
    scale_fill_manual(values = c("Fort taux" = "red", "Faible taux" = "blue")) +
    theme_minimal() +
    theme(legend.position = "right")
  
  # Enregistrer le graphique
  ggsave(filename = paste0(variable, "_comparaison.png"), plot = p, width = 10, height = 6)
  
  # Afficher le graphique dans la console
  print(p)
}

# Analyse bivarié

# Fonction pour générer des graphiques bivariés entre taux d'abstention et les autres variables d'intérêt
variables <- c("NonDiplome", "TxPauv", "txcho", "HLM", "Cadres", "Ouvrier")

for (variable in variables) {
  p <- ggplot(combined_data, aes_string(x = variable, y = "txabs")) +
    geom_point(aes(color = group), size = 3) +  # Ajouter les points avec coloration par groupe (fort/faible taux d'abstention)
    geom_smooth(method = "lm", color = "black", se = TRUE) +  # Ajouter une ligne de régression linéaire
    labs(title = paste("Relation entre", variable, "et le taux d'abstention"),
         x = variable, y = "Taux d'abstention (%)") +
    scale_color_manual(values = c("Fort taux" = "red", "Faible taux" = "blue")) +
    theme_minimal() +
    theme(legend.position = "right")
  
  # Enregistrer le graphique
  ggsave(filename = paste0("bivarie_", variable, "_txabs.png"), plot = p, width = 10, height = 6)
  
  # Afficher le graphique dans la console
  print(p)
}

variables_quantitatives <- data[, sapply(data, is.numeric)]

# Créer une matrice de corrélation pour toutes les variables quantitatives
cor_matrix <- cor(variables_quantitatives, use = "complete.obs")

# Visualiser la matrice de corrélation
ggcorrplot(cor_matrix, 
           method = "square",          # Utiliser des carrés pour visualiser
           type = "lower",             # Montrer la moitié inférieure de la matrice
           lab = TRUE,                 # Afficher les valeurs de corrélation dans chaque case
           lab_size = 3,               # Taille des étiquettes de corrélation
           title = "Matrice de corrélation des Départements et Variables", 
           colors = c("blue", "white", "red"), # Dégradé du bleu (corrélations négatives) au rouge (corrélations positives)
           hc.order = TRUE) +          # Réorganiser les variables selon une méthode de clustering pour mettre en évidence les corrélations similaires
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotation des étiquettes de l'axe X

# Sauvegarder la matrice de corrélation en tant qu'image
ggsave(filename = "global_correlation_matrix.png", width = 10, height = 8)

## Donnée compositionnel

# Transformation CLR
# Charger les données (supposons que vos données sont dans 'combined_data')
# Sélectionner uniquement les colonnes représentant des parts des métiers
data_compositional <- data[, c("Artisant", "Agri", "Ouvrier", "Employe", "PI", "Cadres")]

# Convertir les données en objet compositionnel
data_compositional <- acomp(data_compositional)  # Utiliser la fonction acomp() du package compositions

# Appliquer la transformation CLR
data_clr <- clr(data_compositional)

# Convertir en DataFrame pour une utilisation ultérieure
data_clr_df <- as.data.frame(data_clr)

variables_quantitatives <- data_clr_df[, sapply(data_clr_df, is.numeric)]

# Créer une matrice de corrélation pour toutes les variables quantitatives
cor_matrix <- cor(variables_quantitatives, use = "complete.obs")

# Visualiser la matrice de corrélation
ggcorrplot(cor_matrix, 
           method = "square",          # Utiliser des carrés pour visualiser
           type = "lower",             # Montrer la moitié inférieure de la matrice
           lab = TRUE,                 # Afficher les valeurs de corrélation dans chaque case
           lab_size = 3,               # Taille des étiquettes de corrélation
           title = "Matrice de corrélation des variables compositionnels", 
           colors = c("blue", "white", "red"), # Dégradé du bleu (corrélations négatives) au rouge (corrélations positives)
           hc.order = TRUE) +          # Réorganiser les variables selon une méthode de clustering pour mettre en évidence les corrélations similaires
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotation des étiquettes de l'axe X

# Sauvegarder la matrice de corrélation en tant qu'image
ggsave(filename = "clr_global_correlation_matrix.png", width = 10, height = 8)

# Calculer la matrice de corrélation
cor_matrix <- cor(data_clr_df, use = "pairwise.complete.obs", method = "pearson")

# Transformer la matrice de corrélation en format long pour ggplot2
melted_cor_matrix <- melt(cor_matrix)

# Créer la visualisation avec ggplot2
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed() +
  labs(title = "Matrice de corrélation", x = "Variable 1", y = "Variable 2")


### Combiné les variables transformés

variables_non_transformees <- data[, c("txabs", "Salairemoy", "TxPauv")]

# Étape 4 : Combiner les variables CLR transformées et les variables non transformées
data_transf <- cbind(data_clr_df, variables_non_transformees)

# Vérifier le data frame combiné
head(data_transf)

## Réduction de dimension

# Sélectionner uniquement les variables explicatives (exclure txabs qui est la cible)
data_for_pca <- data_transf[, !colnames(data_transf) %in% c("txabs")]

# Standardiser les données
data_for_pca_scaled <- scale(data_for_pca)

# Vérifier l'aperçu des données standardisées
head(data_for_pca_scaled)

pca_result <- prcomp(data_for_pca_scaled, center = TRUE, scale. = TRUE)

# Afficher un résumé des résultats de la PCA
summary(pca_result)

# Afficher les composantes principales
print(pca_result)

# Visualiser la proportion de variance expliquée par chaque composante principale
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))

# Visualiser les individus (les observations) sur les deux premières composantes principales
fviz_pca_ind(pca_result, 
             col.ind = "cos2", # Colorier selon la qualité de la représentation
             gradient.cols = c("blue", "yellow", "red"),
             repel = TRUE)    # Éviter le chevauchement des étiquettes

# Visualiser les variables sur le plan des deux premières composantes principales
fviz_pca_var(pca_result,
             col.var = "contrib", # Colorier selon la contribution à la variance
             gradient.cols = c("blue", "yellow", "red"),
             repel = TRUE)       # Éviter le chevauchement des étiquettes


# Récupérer les trois premières composantes principales
data_pca_transf <- as.data.frame(pca_result$x[, 1:3])

# Renommer les colonnes pour clarifier qu'il s'agit des composantes principales
colnames(data_pca_transf) <- c("PC1", "PC2", "PC3")

# Ajouter la variable cible (taux d'abstention)
data_pca_transf$txabs <- data_transf$txabs

# Vérifier l'aperçu des nouvelles données réduites
head(data_pca_transf)


### Contribution dans PC1

# Afficher les loadings (contributions des variables) pour toutes les composantes
loadings <- pca_result$rotation

# Afficher les premières composantes (PC1, PC2, PC3)
print(loadings[, 1:3])



# Fonction pour créer un graphique avec les contributions cumulées et sauvegarder l'image
create_and_save_contrib_plot <- function(pca_result, pc_index, top_n = 10, output_file) {
  # Extraire les contributions des variables pour l'axe donné
  contrib <- get_pca_var(pca_result)$contrib[, pc_index]
  
  # Supprimer les valeurs NA des contributions
  contrib <- na.omit(contrib)
  
  # Trier les contributions par ordre décroissant
  contrib_sorted <- sort(contrib, decreasing = TRUE)
  
  # Sélectionner les top N contributions
  contrib_top <- contrib_sorted[1:min(top_n, length(contrib_sorted))]  # Assurer qu'il y a au moins top_n valeurs disponibles
  
  # Calculer le pourcentage cumulé
  contrib_cumsum <- cumsum(contrib_top)
  
  # Créer un data frame avec les contributions et les pourcentages cumulés
  contrib_df <- data.frame(
    Variable = names(contrib_top),
    Contribution = contrib_top,
    Cumulative = contrib_cumsum
  )
  
  # Créer le graphique avec ggplot2
  plot <- ggplot(contrib_df, aes(x = reorder(Variable, -Contribution), y = Contribution)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_line(aes(y = Cumulative, group = 1, color = "Pourcentage cumulé"), size = 1.2) +
    geom_point(aes(y = Cumulative), color = "red", size = 2) +
    scale_y_continuous(sec.axis = sec_axis(~ ., name = "Pourcentage cumulé (%)")) +
    labs(title = paste("Contribution des Variables à la Composante PC", pc_index),
         x = "Variable",
         y = "Contribution (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right") +
    scale_color_manual(name = "Légende", values = c("Pourcentage cumulé" = "red"))
  
  # Afficher le graphique dans R
  print(plot)
  
  # Sauvegarder le graphique en tant qu'image PNG
  ggsave(filename = output_file, plot = plot, width = 10, height = 6)
  
  message("Graphique sauvegardé sous: ", output_file)
}

# Créer et sauvegarder les graphiques pour les trois premières composantes principales
create_and_save_contrib_plot(pca_result, pc_index = 1, top_n = 10, output_file = "contribution_PC1.png")
create_and_save_contrib_plot(pca_result, pc_index = 2, top_n = 10, output_file = "contribution_PC2.png")
create_and_save_contrib_plot(pca_result, pc_index = 3, top_n = 10, output_file = "contribution_PC3.png")


### Visualisation en 3D

# Créer un DataFrame avec les scores de projection sur PC1, PC2, et PC3
scores_pca <- data.frame(pca_result$x[, 1:3])
names(scores_pca) <- c("PC1", "PC2", "PC3")

# Visualisation 3D avec plotly
plot_ly(data = scores_pca, 
        x = ~PC1, 
        y = ~PC2, 
        z = ~PC3, 
        type = "scatter3d", 
        mode = "markers",
        marker = list(size = 3, color = scores_pca$PC1, colorscale = 'Viridis')) %>%
  layout(title = "Projection des Observations sur les Trois Premières Composantes",
         scene = list(xaxis = list(title = "PC1"),
                      yaxis = list(title = "PC2"),
                      zaxis = list(title = "PC3")))


# CLUSTERING

# Normaliser les données après CLR
data_normalized <- scale(data_clr_df)  # Normaliser les variables CLR
# Calculer le nombre optimal de clusters en utilisant la méthode du coude
set.seed(123)  # Fixer le seed pour la reproductibilité
fviz_nbclust(data_normalized, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +  # Par exemple, pour visualiser une solution avec 4 clusters
  labs(subtitle = "Méthode du coude pour déterminer le nombre optimal de clusters")

# Appliquer le clustering K-Means avec le nombre optimal de clusters
k <- 4  # Choisissez k en fonction de la méthode du coude
kmeans_result <- kmeans(data_normalized, centers = k, nstart = 25)

# Ajouter les labels des clusters aux données
data_transf$cluster <- as.factor(kmeans_result$cluster)

# Visualiser les clusters avec une ACP
fviz_cluster(kmeans_result, data = data_normalized, geom = "point", ellipse.type = "convex") +
  labs(title = "Visualisation des clusters avec ACP")

# Créer une fonction pour obtenir les coordonnées des enveloppes convexes (hull)
get_convex_hull <- function(data) {
  data[chull(data$PC1, data$PC2), ]
}


### CREATION D'UN DATA FRAME TEMPORAIRE POUR FAIRE UNE DISTINCTION

# Ajouter la colonne `cluster` à `data`
data_with_clusters <- data %>%
  mutate(cluster = as.factor(kmeans_result$cluster))
print(head(data_with_clusters))

# Calculer les moyennes de chaque variable pour chaque cluster
cluster_means <- data_with_clusters %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))  # Calculer les moyennes uniquement pour les variables numériques

# Pour enlever la première colonne
cluster_means[, -1]
# Supprimer la colonne `...1` avec select()
cluster_means <- cluster_means %>%
  select(-`...1`)

# Afficher les moyennes par cluster
print(cluster_means)

# Transformer les données en format long
means_long <- cluster_means %>%
  mutate(cluster = cluster_means$cluster) %>%  # Ajouter de nouveau la colonne `cluster` pour la comparaison
  pivot_longer(cols = -cluster,                 # Toutes les colonnes sauf `cluster`
               names_to = "variable",           # Nommer la colonne qui contiendra les noms des variables
               values_to = "mean_value")

# Créer un bar plot groupé pour visualiser la comparaison des moyennes des variables par cluster
plot_cluster_comparison <- ggplot(means_long, aes(x = cluster , y = mean_value, fill = as.factor(variable))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Comparaison des Moyennes des Variables par Cluster",
       x = "Variables",
       y = "Valeur Moyenne",
       fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

# Afficher le graphique
print(plot_cluster_comparison)

# Sauvegarder le bar plot
ggsave(filename = "comparaison_moyennes_variables_clusters.png", plot = plot_cluster_comparison, width = 12, height = 8)

comparaison_plot <- ggplot(means_long, aes(x = as.factor(cluster), y = mean_value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Comparaison des Moyennes des Variables par Cluster",
       x = "Cluster", y = "Valeur Moyenne") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Sauvegarder la figure en tant que fichier PNG
ggsave(filename = "comparaison_moyennes_clusters.png", plot = comparaison_plot, width = 10, height = 6)


# REGRESSION
## Préparation des données


data_sans_trois_premieres <- data[, -(1:3)]

# Sélectionner les colonnes à standardiser (toutes sauf 'txabs')
variables_a_standardiser <- setdiff(names(data_sans_trois_premieres), "txabs")

# Appliquer la standardisation sur les colonnes sélectionnées
data_reg <- data_sans_trois_premieres %>%
  mutate(across(all_of(variables_a_standardiser), scale))

# Vérifier la structure du nouveau dataframe
print(head(data_reg))

# Fixer un seed pour rendre la sélection reproductible
set.seed(123)

# Créer un vecteur d'indices de division des données
indices <- createDataPartition(data_reg$txabs, p = 0.7, list = FALSE)

# Créer l'ensemble d'entraînement (70% des données)
train_set <- data_reg[indices, ]

# Créer l'ensemble de test (30% des données)
test_set <- data_reg[-indices, ]



### Random Forest

# Fixer un seed pour assurer la reproductibilité
set.seed(123)

# Entraîner le modèle Random Forest
rf_model <- randomForest(txabs ~ ., data = train_set, importance = TRUE, ntree = 500)

rf_predictions <- predict(rf_model, newdata = test_set)

# Calculer l'Accuracy en utilisant l'erreur quadratique moyenne (ou R²)
mse <- mean((rf_predictions - test_set$txabs)^2)
rmse <- sqrt(mse)
rsq <- cor(rf_predictions, test_set$txabs)

cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared:", rsq, "\n")

# Extraire l'importance des variables
importance_rf <- importance(rf_model)

# Convertir l'importance en dataframe pour ggplot2
importance_df <- data.frame(
  Variable = rownames(importance_rf),
  Importance = importance_rf[, "IncNodePurity"] # Utiliser IncNodePurity qui est un bon indicateur d'importance globale
)

# Trier les données par importance décroissante
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]

importance_plot <- ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Importance des Variables - Random Forest",
       x = "Variables",
       y = "Importance (IncNodePurity)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Afficher le plot
print(importance_plot)

# Sauvegarder le bar plot
ggsave(filename = "importance_variables_rf.png", plot = importance_plot, width = 10, height = 6)

### LOOCV Random forest

# Set a seed for reproducibility
set.seed(123)

# Define the training control for LOOCV
train_control <- trainControl(method = "LOOCV")

# Define the formula for the model (target ~ . means all other columns are used as predictors)
formula <- txabs ~ .

# Train the Random Forest model using LOOCV
rf_loocv_model <- train(
  formula,
  data = data_reg, # Use the whole dataset as caret automatically handles LOOCV splitting
  method = "rf",                  # Random Forest method
  trControl = train_control,      # Use LOOCV for cross-validation
  importance = TRUE,              # Calculate variable importance
  ntree = 500                     # Specify the number of trees in the forest
)


# Summary of the model, which includes RMSE, R^2, and other metrics
print(rf_loocv_model$results)

# Extract RMSE, R^2 from the model
rmse <- rf_loocv_model$results$RMSE
rsq <- rf_loocv_model$results$Rsquared

cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared:", rsq, "\n")

# Extract variable importance from the model
importance_rf <- varImp(rf_loocv_model, scale = FALSE)

# Print variable importance
print(importance_rf)

# Convert to dataframe for plotting
importance_df <- as.data.frame(importance_rf$importance)
importance_df$Variable <- rownames(importance_df)

# Plotting the variable importance
importance_plot <- ggplot(importance_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Importance des Variables - LOOCV Random Forest",
       x = "Variables",
       y = "Importance") +
  theme_minimal()

# Display the plot
print(importance_plot)

# Save the plot
ggsave(filename = "importance_variables_rf_loocv.png", plot = importance_plot, width = 10, height = 6)


### LASSO

# Fixer un seed pour assurer la reproductibilité
set.seed(123)

# Convertir les données d'entraînement en matrices
x_train <- as.matrix(train_set %>% select(-txabs)) # Toutes les variables sauf txabs
y_train <- train_set$txabs

# Entraîner le modèle Lasso avec validation croisée (LOOCV)
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = nrow(x_train))

# Afficher les résultats de la validation croisée
plot(lasso_model)
print(paste("Lambda optimal:", lasso_model$lambda.min))

# Convertir les données de test en matrices
x_test <- as.matrix(test_set %>% select(-txabs))
y_test <- test_set$txabs

# Effectuer les prédictions sur l'ensemble de test en utilisant le lambda optimal
lasso_predictions <- predict(lasso_model, s = lasso_model$lambda.min, newx = x_test)

# Calculer l'erreur quadratique moyenne (MSE) et l'erreur quadratique moyenne racine (RMSE)
mse <- mean((lasso_predictions - y_test)^2)
rmse <- sqrt(mse)

# Calculer le coefficient de détermination (R²)
rsq <- cor(lasso_predictions, y_test)^2

# Afficher les résultats
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared:", rsq, "\n")


# Extraire les coefficients au lambda optimal
lasso_coefficients <- coef(lasso_model, s = lasso_model$lambda.min)

# Convertir les coefficients en dataframe
importance_lasso <- as.data.frame(as.matrix(lasso_coefficients))
importance_lasso$Variable <- rownames(importance_lasso)
colnames(importance_lasso)[1] <- "Coefficient"

# Supprimer l'interception (Intercept)
importance_lasso <- importance_lasso %>% filter(Variable != "(Intercept)")

# Trier les coefficients par valeur absolue décroissante
importance_lasso <- importance_lasso %>%
  arrange(desc(abs(Coefficient)))

# Afficher les coefficients
print(importance_lasso)


# Créer un bar plot de l'importance des variables
importance_lasso_plot <- ggplot(importance_lasso, aes(x = reorder(Variable, abs(Coefficient)), y = (Coefficient))) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  coord_flip() +
  labs(title = "Importance des Variables - Régression Lasso",
       x = "Variables",
       y = "Coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Afficher le plot
print(importance_lasso_plot)

# Sauvegarder le bar plot
ggsave(filename = "importance_variables_lasso.png", plot = importance_lasso_plot, width = 10, height = 6)