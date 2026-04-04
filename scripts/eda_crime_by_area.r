# Hotspots
df %>%
  count(AREA_NAME, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(AREA_NAME, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  labs(title = "Top 10 Crime Areas",
       x = "Area",
       y = "Crime Count") +
  theme_minimal()

# Map(Heat Scatter)
ggplot(df, aes(x = LON, y = LAT)) +
  geom_point(alpha = 0.2, color = "red") +
  labs(title = "Crime Location Scatter Plot") +
  theme_minimal()
