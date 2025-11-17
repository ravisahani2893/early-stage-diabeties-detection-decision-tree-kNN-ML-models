# ============================================================
# K-Nearest Neighbors (KNN) Model
# Author: RaviKumar Sahani
# Email: ravisahani2893@gmail.com
# ============================================================


# Install all required libraries for kNN ML modeling and evaluation
install.packages(c(
  "class",    # For KNN implementation
  "caret"     # For model training, evaluation, and confusion matrices
))
# Load KNN library
library(class) 
# Load caret  library
library(caret)

# -------------------------------
# 1. Load and Prepare Dataset
# -------------------------------
# Load early diabeties prediction dataset using read.csv function
diabeties_data_set <- read.csv("data/diabetes_data_upload 2.csv")

# Copy original dataset for KNN processing
knn_data <- diabeties_data_set

# Convert target variable to factor data type
knn_data$class <- as.factor(knn_data$class)


# -------------------------------
# 2. Convert categorical Yes/No to numeric 0/1
# KNN requires numeric input only
# -------------------------------

knn_data_numeric <- knn_data %>%
  mutate(
    Gender = ifelse(Gender == "Male", 1, 0),
    class  = as.numeric(class) - 1   # Negative = 0, Positive = 1
  )


# Convert all binary symptom features (e.g., Polyuria, Polydipsia) to numeric
for (var in all_binary_features) {
  knn_data_numeric[[var]] <- ifelse(knn_data_numeric[[var]] == "Yes", 1, 0)
}

# Check structure of numeric dataset
str(knn_data_numeric)

# -------------------------------
# 3. Normalize Features
# -------------------------------
# KNN is distance-based, so all features must be on the same scale
# Normalization rescales each feature to range [0,1]
# -------------------------------

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

knn_norm <- knn_data_numeric

# Identify feature columns (exclude the target variable)
feature_cols <- setdiff(names(knn_norm), "class")

# Apply normalization only to feature columns
knn_norm[feature_cols] <- lapply(knn_norm[feature_cols], normalize)
str(knn_norm)

# -------------------------------
# 4. Train-Test Split
# -------------------------------
# Use same 80%-20% split as Decision Tree
# -------------------------------
train_index <- createDataPartition(diabeties_data_set$class, p = 0.8, list = FALSE)

# Split 80% data for training
train_knn <- knn_norm[train_index, ]

# Split the remaining 20% for testing
test_knn  <- knn_norm[-train_index, ]

train_X <- train_knn %>% select(-class)
train_y <- train_knn$class

test_X  <- test_knn %>% select(-class)
test_y  <- test_knn$class

# -------------------------------
# 5. Train KNN Model (k = 5 initially)
# KNN stores training data and Predictions are made based on nearest neighbors
# -------------------------------

set.seed(123)
k_value <- 5

knn_pred <- knn(train = train_X, test = test_X, cl = train_y, k = k_value)

# -------------------------------
# 6. Model Evaluation
# -------------------------------
# Use confusion matrix to check performance
# -------------------------------

conf_knn <- confusionMatrix(as.factor(knn_pred), as.factor(test_y))
print(conf_knn)

knn_accuracy <- conf_knn$overall["Accuracy"]
cat("KNN Accuracy (k=5):", knn_accuracy, "\n")

# -------------------------------
# 7. Hyperparameter Tuning: Find Best K
# -------------------------------
# Test K values from 1 to 20 and find the one giving highest accuracy
# --
accuracy_list <- c()

for (k in 1:20) {
  pred_k <- knn(train = train_X, test = test_X, cl = train_y, k = k)
  acc <- mean(pred_k == test_y)
  accuracy_list[k] <- acc
  cat("K =", k, " Accuracy:", acc, "\n")
}

best_k <- which.max(accuracy_list)
cat("\nBest K:", best_k, "with accuracy:", max(accuracy_list), "\n")

# Optional: Plot K vs Accuracy
plot(1:20, accuracy_list, type = "b",
     xlab = "K Value", ylab = "Accuracy",
     main = "KNN Accuracy for Different K Values")
