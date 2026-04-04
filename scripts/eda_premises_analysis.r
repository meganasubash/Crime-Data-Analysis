# Premises Analysis
df %>%
  count(PREMIS_DESC, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(PREMIS_DESC, n), y = n)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Top Crime Locations (Premises)") +
  theme_minimal()
