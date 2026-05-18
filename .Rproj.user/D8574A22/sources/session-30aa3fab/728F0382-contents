###########################################################
# MINI PROJET ANALYSE MULTIVARIEE
# Sujet :
# Impact de la dépendance à l’IA sur les performances académiques
###########################################################

#############################
# 1. INSTALLATION PACKAGES
#############################

install.packages(c(
  "tidyverse",
  "FactoMineR",
  "factoextra",
  "cluster",
  "corrplot",
  "NbClust",
  "pheatmap"
))

#############################
# 2. LIBRARIES
#############################

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)
library(corrplot)
library(NbClust)
library(pheatmap)

#############################
# 3. IMPORT DATASET
#############################

data <- read.csv("ai_impact_student_performance_dataset.csv")

#############################
# 4. EXPLORATION DATA
#############################

head(data)

str(data)

summary(data)

dim(data)

#############################
# 5. VALEURS MANQUANTES
#############################

colSums(is.na(data))

#############################
# 6. VARIABLES NUMERIQUES
#############################

data_num <- data %>%
  select(
    ai_dependency_score,
    ai_usage_time_minutes,
    ai_ethics_score,
    last_exam_score,
    assignment_scores_avg,
    attendance_percentage,
    concept_understanding_score,
    study_consistency_index,
    improvement_rate,
    sleep_hours,
    social_media_hours,
    class_participation_score,
    final_score
  )

#############################
# 7. NORMALISATION
#############################

data_scaled <- scale(data_num)

#############################
# 8. ANALYSE DESCRIPTIVE
#############################

#################################
# Histogramme dépendance IA
#################################

ggplot(data,
       aes(x = ai_dependency_score)) +
  geom_histogram(
    bins = 20,
    fill = "steelblue",
    color = "black"
  ) +
  labs(
    title = "Distribution du score de dépendance à l’IA",
    x = "Score dépendance IA",
    y = "Fréquence"
  ) +
  theme_minimal()

#################################
# Temps utilisation IA
#################################

ggplot(data,
       aes(x = ai_usage_time_minutes)) +
  geom_histogram(
    bins = 30,
    fill = "darkgreen",
    color = "black"
  ) +
  labs(
    title = "Temps d’utilisation quotidien de l’IA",
    x = "Minutes",
    y = "Nombre d’étudiants"
  ) +
  theme_minimal()

#################################
# IA vs Score Final
#################################

ggplot(data,
       aes(
         x = ai_dependency_score,
         y = final_score
       )) +
  geom_point(
    alpha = 0.5,
    color = "blue"
  ) +
  geom_smooth(
    method = "lm",
    color = "red"
  ) +
  labs(
    title = "Relation entre dépendance IA et score final",
    x = "Dépendance IA",
    y = "Score final"
  ) +
  theme_minimal()

#################################
# Sleep vs Final Score
#################################

ggplot(data,
       aes(
         x = sleep_hours,
         y = final_score
       )) +
  geom_point(
    alpha = 0.5,
    color = "purple"
  ) +
  geom_smooth(
    method = "lm",
    color = "red"
  ) +
  labs(
    title = "Sommeil et performance académique",
    x = "Heures de sommeil",
    y = "Score final"
  ) +
  theme_minimal()

#################################
# Attendance vs Final Score
#################################

ggplot(data,
       aes(
         x = attendance_percentage,
         y = final_score
       )) +
  geom_point(
    alpha = 0.5,
    color = "darkorange"
  ) +
  geom_smooth(
    method = "lm",
    color = "red"
  ) +
  labs(
    title = "Présence et performance académique",
    x = "Présence %",
    y = "Score final"
  ) +
  theme_minimal()

#############################
# 9. MATRICE CORRELATION
#############################
cor_matrix <- cor(data_num, use = "complete.obs")

corrplot(
  cor_matrix,
  method = "circle",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",
  number.cex = 0.6,
  col = colorRampPalette(
    c("red", "white", "blue")
  )(200)
)

#############################
# 10. ACP
#############################

res.pca <- PCA(data_scaled,
               graph = FALSE)

#############################
# 11. VALEURS PROPRES
#############################

res.pca$eig

#############################
# 12. SCREE PLOT
#############################

fviz_eig(
  res.pca,
  addlabels = TRUE,
  ylim = c(0, 50)
)

#############################
# 13. CERCLE CORRELATIONS
#############################

fviz_pca_var(
  res.pca,
  col.var = "contrib",
  gradient.cols = c(
    "blue",
    "yellow",
    "red"
  ),
  repel = TRUE
)

#############################
# 14. PROJECTION INDIVIDUS
#############################

fviz_pca_ind(
  res.pca,
  geom.ind = "point",
  col.ind = "steelblue",
  repel = TRUE
)

#############################
# 15. BIPLOT ACP
#############################

fviz_pca_biplot(
  res.pca,
  geom.ind = "point",
  pointshape = 21,
  pointsize = 2,
  fill.ind = "lightblue",
  col.ind = "black",
  col.var = "red",
  repel = TRUE
)

#############################
# 16. NOMBRE OPTIMAL CLUSTERS
#############################

fviz_nbclust(
  data_scaled,
  kmeans,
  method = "wss"
) +
  theme_minimal()
#############################
# 17. KMEANS
#############################

set.seed(123)

km_res <- kmeans(
  data_scaled,
  centers = 4,
  nstart = 25
)

#############################
# 18. RESULTATS KMEANS
#############################

km_res

#############################
# 19. VISUALISATION CLUSTERS
#############################

fviz_cluster(
  km_res,
  data = data_scaled,
  ellipse.type = "norm",
  palette = "jco",
  ggtheme = theme_minimal()
)

#############################
# 20. AJOUT CLUSTER DATASET
#############################

data$cluster <- as.factor(km_res$cluster)

#############################
# 21. MOYENNES PAR CLUSTER
#############################

cluster_means <- aggregate(
  data_num,
  by = list(cluster = km_res$cluster),
  mean
)

cluster_means

#############################
# 22. HEATMAP CLUSTERS
#############################

pheatmap(
  as.matrix(cluster_means[,-1]),
  cluster_rows = TRUE,
  cluster_cols = TRUE
)

#############################
# 23. ACP + CLUSTERING
#############################

fviz_cluster(
  km_res,
  data = data_scaled,
  ellipse.type = "convex",
  geom = "point",
  palette = "jco",
  ggtheme = theme_minimal()
)

#############################
# 24. BOXPLOT PAR CLUSTER
#############################

ggplot(data,
       aes(
         x = cluster,
         y = final_score,
         fill = cluster
       )) +
  geom_boxplot() +
  labs(
    title = "Score final par cluster",
    x = "Cluster",
    y = "Score final"
  ) +
  theme_minimal()

#############################
# 25. DEPENDANCE IA PAR CLUSTER
#############################

ggplot(data,
       aes(
         x = cluster,
         y = ai_dependency_score,
         fill = cluster
       )) +
  geom_boxplot() +
  labs(
    title = "Dépendance IA par cluster",
    x = "Cluster",
    y = "Score dépendance IA"
  ) +
  theme_minimal()

#############################
# 26. EXPORT RESULTATS
#############################

write.csv(
  cluster_means,
  "cluster_means.csv",
  row.names = FALSE
)

#############################
# FIN PROJET
#############################