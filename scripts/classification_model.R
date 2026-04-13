# library(rpart)
library(rpart.plot)
library(dplyr)
library(randomForest)

# Set directories and load data
OUTPUT_DIR <- Sys.getenv("OUTPUT_DIR", unset = "/data/outputs")
PLOTS_DIR  <- file.path(OUTPUT_DIR, "plots")
dir.create(PLOTS_DIR, showWarnings = FALSE, recursive = TRUE)

# Load cleaned data
INPUT_CSV <- file.path(OUTPUT_DIR, "crime_cleaned.csv")
df <- read.csv(INPUT_CSV, stringsAsFactors = FALSE)

# Prepare modelling data
model_df <- df %>%
  select(CRIME_SEVERITY, VICT_AGE, TIME_PERIOD, TEMP_MAX, RAIN_FLAG, AREA_NAME, WEEKDAY, SEASON) %>%
  filter(VICT_AGE > 0) %>%
  na.omit() %>%
  mutate(
    CRIME_SEVERITY = as.factor(CRIME_SEVERITY),
    TIME_PERIOD    = as.factor(TIME_PERIOD),
    RAIN_FLAG      = as.factor(RAIN_FLAG),
    AREA_NAME      = as.factor(AREA_NAME),
    WEEKDAY        = as.factor(WEEKDAY),
    SEASON         = as.factor(SEASON)
  )

cat("Model data dimensions:", nrow(model_df), "x", ncol(model_df), "\n")
cat("Target class distribution:\n")
print(table(model_df$CRIME_SEVERITY))

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
dt_model <- rpart(CRIME_SEVERITY ~ ., data = train, method = "class", control = rpart.control(maxdepth = 6))

png(file.path(PLOTS_DIR, "15_decision_tree.png"), width = 1200, height = 800)
rpart.plot(dt_model, main = "Decision Tree - Crime Severity", box.palette = "RdYlGn")
dev.off()
cat("Saved: 15_decision_tree.png\n")

pred_dt  <- predict(dt_model, test, type = "class")
acc_dt   <- mean(pred_dt == test$CRIME_SEVERITY)
cat(sprintf("Decision Tree Accuracy: %.4f\n", acc_dt))
cat("Confusion Matrix:\n")
print(table(Predicted = pred_dt, Actual = test$CRIME_SEVERITY))

#  Random Forest 
####cat("Training Random Forest (this may take a while)...\n")
# We will use 100 trees for the random forest model. The 'importance = TRUE' argument allows us to later analyze which features were most important in making predictions.
# We set a seed for reproducibility, ensuring that the random processes in the model training can be replicated in future runs.
# set.seed(123) is used to ensure that the random processes in the model training can be replicated in future runs, which is important for reproducibility of results.
set.seed(123)
rf_model <- randomForest(
  CRIME_SEVERITY ~ .,
  data       = train,
  ntree      = 100,
  importance = TRUE
)

pred_rf <- predict(rf_model, test)
acc_rf  <- mean(pred_rf == test$CRIME_SEVERITY)
cat(sprintf("Random Forest Accuracy: %.4f\n", acc_rf))
cat("Confusion Matrix:\n")
print(table(Predicted = pred_rf, Actual = test$CRIME_SEVERITY))

# Variable importance plot
png(file.path(PLOTS_DIR, "16_rf_variable_importance.png"), width = 900, height = 600)
varImpPlot(rf_model, main = "Random Forest : Variable Importance")
dev.off()
cat("Saved: 16_rf_variable_importance.png\n")