# -------------------------------
# STEP 0: Install & Load Libraries
# -------------------------------
install.packages(c("tidyverse", "janitor", "ggplot2", "cluster", "writexl"))

library(tidyverse)
library(janitor)
library(ggplot2)
library(cluster)
library(writexl)

# -------------------------------
# STEP 1: Load Dataset
# -------------------------------
df <- read.csv("C:/Users/MS994/Downloads/pds/crime_weather_cleaned2.csv")

df <- clean_names(df)

# Add row_id for safe merging later
df$row_id <- 1:nrow(df)

# -------------------------------
# STEP 2: Feature Selection
# -------------------------------
clust_df <- df %>%
  select(row_id, temp_max, temp_min, rain, vict_age, time_occ) %>%
  na.omit()

# -------------------------------
# STEP 3: Scaling
# -------------------------------
clust_scaled <- clust_df %>%
  select(-row_id) %>%
  scale() %>%
  as.data.frame()

# -------------------------------
# STEP 4: Elbow Method
# -------------------------------
wss <- numeric(10)

for (i in 1:10) {
  kmeans_model <- kmeans(clust_scaled, centers = i, nstart = 10)
  wss[i] <- kmeans_model$tot.withinss
}

plot(1:10, wss, type = "b", pch = 19,
     xlab = "Number of clusters (K)",
     ylab = "WSS",
     main = "Elbow Method")

# -------------------------------
# STEP 5: K-Means Clustering
# -------------------------------
set.seed(123)
k_model <- kmeans(clust_scaled, centers = 4, nstart = 25)

# Add cluster to clust_df
clust_df$cluster <- k_model$cluster

# -------------------------------
# STEP 6: Merge Back to Full Dataset
# -------------------------------
df_final <- df %>%
  left_join(clust_df %>% select(row_id, cluster), by = "row_id")

# Check distribution
table(df_final$cluster)

# -------------------------------
# STEP 7: Cluster Interpretation
# -------------------------------
aggregate(clust_df[, c("temp_max","temp_min","rain","vict_age","time_occ")],
          by = list(cluster = clust_df$cluster),
          FUN = mean)

# -------------------------------
# STEP 8: Visualization (Sample)
# -------------------------------
set.seed(123)
sample_df <- clust_df %>% sample_n(5000)

# Plot 1
p1 <- ggplot(sample_df, aes(temp_max, vict_age, color = factor(cluster))) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Temp vs Age", color = "Cluster")

p1

# Plot 2
p2 <- ggplot(sample_df, aes(time_occ, temp_max, color = factor(cluster))) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Time vs Temp")

p2

# Plot 3
p3 <- ggplot(sample_df, aes(factor(cluster), rain, fill = factor(cluster))) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Rain by Cluster")

p3

# Plot 4
p4 <- ggplot(sample_df, aes(factor(cluster))) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Cluster Count")

p4

# -------------------------------
# STEP 9: Silhouette Score (Sample)
# -------------------------------
set.seed(123)

sample_scaled <- clust_scaled %>% sample_n(5000)

k_sample <- kmeans(sample_scaled, centers = 4, nstart = 25)

sil <- silhouette(k_sample$cluster, dist(sample_scaled))

mean_sil <- mean(sil[, 3])
mean_sil

# -------------------------------
# STEP 10: Export Final Dataset
# -------------------------------

# CSV
write.csv(df_final,
          "C:/Users/MS994/Downloads/pds/crime_with_clusters.csv",
          row.names = FALSE)

# Excel
write_xlsx(df_final,
           "C:/Users/MS994/Downloads/pds/crime_with_clusters.xlsx")
