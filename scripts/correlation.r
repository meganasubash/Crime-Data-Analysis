# Correlation HeatMap
install.packages("corrplot")
library(corrplot)

numeric_df <- df %>%
  select(TEMP_MAX, TEMP_MIN, RAIN, VICT_AGE) %>%
  na.omit()

cor_matrix <- cor(numeric_df)

corrplot(cor_matrix, method = "color")
