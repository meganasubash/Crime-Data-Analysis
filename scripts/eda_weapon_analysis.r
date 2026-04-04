# Weapon Analysis
df %>%
  count(WEAPON_DESC, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(WEAPON_DESC, n), y = n)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Top Weapons Used") +
  theme_minimal()
