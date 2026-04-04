# Age Distribution
ggplot(df, aes(x = VICT_AGE)) +
  geom_histogram(bins = 30, fill = "skyblue") +
  labs(title = "Victim Age Distribution") +
  theme_minimal()

# Age Group
df %>%
  count(AGE_GROUP) %>%
  ggplot(aes(x = AGE_GROUP, y = n, fill = AGE_GROUP)) +
  geom_bar(stat = "identity") +
  labs(title = "Crime by Age Group") +
  theme_minimal()

#Gender Distribution
df %>%
  count(VICT_SEX) %>%
  ggplot(aes(x = VICT_SEX, y = n, fill = VICT_SEX)) +
  geom_bar(stat = "identity") +
  labs(title = "Crime by Gender") +
  theme_minimal()
