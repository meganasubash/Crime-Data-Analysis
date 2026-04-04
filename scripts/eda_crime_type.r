# Crime Type Analysis
df %>%
  count(CRM_CD_DESC, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(CRM_CD_DESC, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top Crime Types") +
  theme_minimal()
