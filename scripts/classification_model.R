# Decision Tree Model
# Install
install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)
library(dplyr)

# Load data
df <- read.csv("C:/Users/MS994/Downloads/pds/crime_weather_cleaned2.csv")

# Select relevant columns
model_df <- df %>%
  select(CRM_CD_DESC, VICT_AGE, TIME_PERIOD, TEMP_MAX, RAIN_FLAG, AREA_NAME) %>%
  na.omit()

# Convert to factors
model_df$CRM_CD_DESC <- as.factor(model_df$CRM_CD_DESC)
model_df$TIME_PERIOD <- as.factor(model_df$TIME_PERIOD)
model_df$RAIN_FLAG <- as.factor(model_df$RAIN_FLAG)
model_df$AREA_NAME <- as.factor(model_df$AREA_NAME)

# Train-Test Split
set.seed(123)
train_index <- sample(1:nrow(model_df), 0.7*nrow(model_df))

train <- model_df[train_index, ]
test  <- model_df[-train_index, ]

# Train Decision Tree
model <- rpart(CRM_CD_DESC ~ ., data = train, method = "class")

# Plot Tree
rpart.plot(model)

# Predictions
pred <- predict(model, test, type = "class")

# Accuracy
accuracy <- mean(pred == test$CRM_CD_DESC)
accuracy



# Random Forest
install.packages("randomForest")
library(randomForest)

rf_model <- randomForest(CRM_CD_DESC ~ ., data = train)

pred_rf <- predict(rf_model, test)

mean(pred_rf == test$CRM_CD_DESC)
