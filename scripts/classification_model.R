# library(rpart)
library(rpart.plot)
library(dplyr)
library(randomForest)

# Set directories and load data
OUTPUT_DIR <- Sys.getenv("OUTPUT_DIR", unset = "/data/outputs")
PLOTS_DIR  <- file.path(OUTPUT_DIR, "plots")
dir.create(PLOTS_DIR, showWarnings = FALSE, recursive = TRUE)

# Load cleaned data
INPUT_CSV <- file.path(OUTPUT_DIR, "crime_weather_cleaned2.csv")
df <- read.csv(INPUT_CSV, stringsAsFactors = FALSE)

# Prepare modelling data
model_df <- df %>%
  select(CRM.CD.DESC, VICT_AGE, TIME_PERIOD, TEMP_MAX, RAIN_FLAG, AREA.NAME) %>%
  filter(VICT_AGE > 0) %>%
  na.omit() %>%
  group_by(CRM.CD.DESC) %>%
  filter(n() >= 50) %>% # Only keep crimes that appear at least 50 times to ensure enough data for modeling
  ungroup() %>%
  mutate(
    CRM.CD.DESC = as.factor(CRM.CD.DESC),
    TIME_PERIOD = as.factor(TIME_PERIOD),
    RAIN_FLAG   = as.factor(RAIN_FLAG),
    AREA.NAME   = as.factor(AREA.NAME)
  )

cat("Model data dimensions:", nrow(model_df), "x", ncol(model_df), "\n")

# Train / Test split
# Set seed for reproducibility
# We will use a 70/30 split for training and testing
set.seed(123)
train_idx <- sample(seq_len(nrow(model_df)), 0.7 * nrow(model_df))
train <- model_df[ train_idx, ]
test  <- model_df[-train_idx, ]

# Decision Tree
cat("Training Decision Tree...\n")
# We will limit the depth of the tree to avoid overfitting and ensure it is interpretable
dt_model <- rpart(CRM.CD.DESC ~ ., data = train, method = "class",control = rpart.control(maxdepth = 6))   # cap depth to avoid huge trees

png(file.path(PLOTS_DIR, "15_decision_tree.png"), width = 1200, height = 800)
# rpart.plot is used to visualize the decision tree model. The 'box.palette = 0' argument sets a default color scheme for the boxes in the tree.
rpart.plot(dt_model, main = "Decision Tree – Crime Type", box.palette = 0)
# dev.off() is called after the plot to save the image and free up resources
dev.off()
cat("Saved: 15_decision_tree.png\n")

# Evaluate Decision Tree
# We predict the crime type on the test set using the decision tree model and calculate the accuracy by comparing the predicted values to the actual crime types in the test set.
pred_dt  <- predict(dt_model, test, type = "class")
acc_dt   <- mean(pred_dt == test$CRM.CD.DESC)
cat(sprintf("Decision Tree Accuracy: %.4f\n", acc_dt))

#  Random Forest 
####cat("Training Random Forest (this may take a while)...\n")
# We will use 100 trees for the random forest model. The 'importance = TRUE' argument allows us to later analyze which features were most important in making predictions.
# We set a seed for reproducibility, ensuring that the random processes in the model training can be replicated in future runs.
# set.seed(123) is used to ensure that the random processes in the model training can be replicated in future runs, which is important for reproducibility of results.
set.seed(123)
# The randomForest function is used to train a random forest model to predict the crime type (CRM.CD.DESC) based on all other features in the training data. We specify ntree = 100 to use 100 trees in the forest, and importance = TRUE to enable the calculation of variable importance measures.
rf_model <- randomForest(
  CRM.CD.DESC ~ .,
  data     = train,
  ntree    = 100,
  importance = TRUE
)

pred_rf <- predict(rf_model, test)
acc_rf  <- mean(pred_rf == test$CRM.CD.DESC)
cat(sprintf("Random Forest Accuracy: %.4f\n", acc_rf))

# Variable importance plot
png(file.path(PLOTS_DIR, "16_rf_variable_importance.png"), width = 900, height = 600)
varImpPlot(rf_model, main = "Random Forest : Variable Importance")
dev.off()
cat("Saved: 16_rf_variable_importance.png\n")