# Rain vs. Crime
df %>%
  count(RAIN_FLAG) %>%
  ggplot(aes(x = RAIN_FLAG, y = n, fill = RAIN_FLAG)) +
  geom_bar(stat = "identity") +
  labs(title = "Crime in Rain vs No Rain") +
  theme_minimal()

# Temperature vs. Crime
df %>%
  count(TEMP_LEVEL) %>%
  ggplot(aes(x = TEMP_LEVEL, y = n, fill = TEMP_LEVEL)) +
  geom_bar(stat = "identity") +
  labs(title = "Crime by Temperature Level") +
  theme_minimal()

# Temp vs Crime PATTERN
ggplot(df, aes(x = TEMP_MAX, y = VICT_AGE)) +
  geom_point(alpha = 0.3) +
  labs(title = "Temperature vs Victim Age") +
  theme_minimal()
