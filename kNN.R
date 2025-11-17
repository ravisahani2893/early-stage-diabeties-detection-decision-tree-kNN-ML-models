# ============================================================
# K-Nearest Neighbors (KNN) Model
# Author: RaviKumar Sahani
# Email: ravisahani2893@gmail.com
# ============================================================

library(class)
library(caret)

# -------------------------------
# 1. Prepare Dataset
# -------------------------------
diabeties_data_set <- read.csv("data/diabetes_data_upload 2.csv")
# Copy the dataset
knn_data <- diabeties_data_set

# Convert target variable to factor (already done earlier, but safe to re-apply)
knn_data$class <- as.factor(knn_data$class)


# -------------------------------
# 2. Convert all categorical Yes/No to numeric 0/1
# KNN requires numeric input
# -------------------------------

knn_data_numeric <- knn_data %>%
  mutate(
    Gender = ifelse(Gender == "Male", 1, 0),
    class  = as.numeric(class) - 1   # Negative = 0, Positive = 1
  )
str(knn_data_numeric)
# Convert binary Yes/No to numeric
for (var in all_binary_features) {
  knn_data_numeric[[var]] <- ifelse(knn_data_numeric[[var]] == "Yes", 1, 0)
}

# -------------------------------
# 3. Normalization (VERY IMPORTANT for KNN)
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
# 4. Train-Test Split (same as Decision Tree - 416/104 split)
# -------------------------------

train_knn <- knn_norm[1:416, ]
test_knn  <- knn_norm[417:520, ]

train_X <- train_knn %>% select(-class)
train_y <- train_knn$class

test_X  <- test_knn %>% select(-class)
test_y  <- test_knn$class

# -------------------------------
# 5. Train KNN Model (k = 5 initially)
# -------------------------------

set.seed(123)
k_value <- 5

knn_pred <- knn(train = train_X, test = test_X, cl = train_y, k = k_value)

# -------------------------------
# 6. Model Evaluation
# -------------------------------

conf_knn <- confusionMatrix(as.factor(knn_pred), as.factor(test_y))
print(conf_knn)

knn_accuracy <- conf_knn$overall["Accuracy"]
cat("KNN Accuracy (k=5):", knn_accuracy, "\n")

# ============================================================
# 7. Hyperparameter Tuning: Find Best K (1–20)
# ============================================================

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
