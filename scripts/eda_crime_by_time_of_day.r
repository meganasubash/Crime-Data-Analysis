# Crime by Hour
df %>%
  mutate(hour = as.numeric(substr(sprintf("%04d", TIME_OCC),1,2))) %>%
  count(hour) %>%
  ggplot(aes(x = hour, y = n)) +
  geom_line() +
  geom_point() +
  labs(title = "Crime Distribution by Hour",
       x = "Hour of Day",
       y = "Number of Crimes") +
  theme_minimal()

# Crime by Time Period
df %>%
  count(TIME_PERIOD) %>%
  ggplot(aes(x = TIME_PERIOD, y = n, fill = TIME_PERIOD)) +
  geom_bar(stat = "identity") +
  labs(title = "Crime by Time Period") +
  theme_minimal()

# Crime by WeekDay
df %>%
  count(WEEKDAY) %>%
  ggplot(aes(x = reorder(WEEKDAY, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Crime by Weekday",
       x = "Day",
       y = "Count") +
  theme_minimal()
