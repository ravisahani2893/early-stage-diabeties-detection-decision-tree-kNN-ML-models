#############################################
# Author: RaviKumar Sahani
# Email: ravisahani2893@gmail.com
# Project: Decision Tree Classification on Early Diabetes Prediction Dataset
#############################################

# Install all required libraries for Decision Tree ML modeling and evaluation
install.packages(c(
  "ggplot2",    # For data visualization (plots, charts, histograms)
  "dplyr",      # For data manipulation (filtering, selecting, mutating)
  "tidyr",      # For reshaping data (pivot_longer, pivot_wider)
  "corrplot",   # For plotting correlation matrices
  "rpart",      # For building decision tree models
  "rpart.plot", # For visualizing decision trees
  "caret"       # For model training, evaluation, and confusion matrices
))

library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(rpart)
library(rpart.plot)
library(caret)

# Load early diabeties prediction dataset using read.csv function
diabeties_data_set <- read.csv("data/diabetes_data_upload 2.csv")

# Quick overview of the diabeties data set
head(diabeties_data_set)

# Displays the Internal structure of an object. Here, it displays diabeties data set object structure like Age, Gender, etc.
str(diabeties_data_set)

# Displays the statistical summary of the diabeties data set like Min, Max, Median, Mean, etc.
summary(diabeties_data_set)

# Checking Missing or Not available values in the diabeties data set. Since there is no missing values in the dataset, will skip the data cleaning step
colSums(is.na(diabeties_data_set))


# Checking 'class' variable distribution in the diabeties data set. It returns the count of each unique value in the 'class' column.
# There are two classes in the dataset: Negative and Positive. Negative indicates no diabetes, while Positive indicates the presence of diabetes.
table(diabeties_data_set$class)

# Converting 'class' variable to a factor data type for better handling in analysis and modeling. Converting 'class' variable into a factor ensures that the decision tree will predict categories rather than numeric values. 
diabeties_data_set$class <- factor(diabeties_data_set$class)
levels(diabeties_data_set$class)

# Validate whether the 'class' variable is successfully converted to a factor data type or not.
is.factor(diabeties_data_set$class)


# Get all feature names in the diabeties data set.
all_features_in_dataset <- names(diabeties_data_set)

# Get binary variables from the dataset by removing non-binary variables like Age, Gender and class.
all_binary_features <- setdiff(all_features_in_dataset, c("Age", "Gender", "class"))
all_binary_features

# Convert all this binary variables to factor data type for better handling in analysis and modeling.
diabeties_data_set <- diabeties_data_set %>%
  mutate(across(all_of(all_binary_features), factor))

# Check whether the binary variables are successfully converted to factor data type or not for one variable as an example.
is.factor(diabeties_data_set$Polydipsia) 



# Function to check class distribution
check_class_split <- function(data) {
  prop <- prop.table(table(data$class)) * 100
  print(round(prop, 2))
}

# ===
# Exploratory Data Analysis (EDA)
# ===


# Bar Plot: Distribution of Diabetes Class
# This plot shows the count of observations in each class of the 'class' variable.
# 'class' has two categories: Negative (no diabetes) and Positive (diabetes).
# It helps us understand the balance of the dataset — whether both classes are equally represented.
# A balanced dataset is important for training a fair and accurate model.
ggplot(diabeties_data_set, aes(x = class)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Diabetes Class", x = "Class", y = "Count")

# Faceted Bar Plot: Distribution of All Binary Symptoms
# This plot shows the distribution of all binary features (symptoms) in the dataset.
# Each binary feature (e.g., Polyuria, Polydipsia) is reshaped into long format using pivot_longer,
# creating two columns: 'Feature' (name of the symptom) and 'Status' (Yes/No or 1/0).
# facet_wrap(~ Feature, scales = "free_x") creates a separate subplot for each symptom,
# allowing us to compare their distributions easily.
# geom_bar() displays counts of each status within each feature.
# theme_minimal() gives a clean, uncluttered look.
plot_data <- diabeties_data_set %>%
  select(all_of(all_binary_features)) %>%
  pivot_longer(cols = everything(), names_to = "Feature", values_to = "Status")

ggplot(plot_data, aes(x = Status)) +
  geom_bar(fill = "red") +
  facet_wrap(~ Feature, scales = "free_x") +
  labs(title = "Distribution of Symptoms", x = "Status", y = "Count") +
  theme_minimal()

# Function to plot a binary feature vs class
plot_feature_vs_class <- function(feature_name, data) {
  # ggplot bar chart
  # position = "dodge" separates bars for each class side by side
  ggplot(data, aes_string(x = feature_name, fill = "class")) +
    geom_bar(position = "dodge") +
    labs(title = paste(feature_name, "vs Diabetes Class"),
         x = feature_name,
         y = "Count") +
    theme_minimal()  # clean theme
}

# Apply the function to all binary features
for (feature in all_binary_features) {
  print(plot_feature_vs_class(feature, diabeties_data_set))
}

# Histogram: Distribution of Age
# This plot shows the distribution of the 'Age' variable in the dataset.
# Age is a continuous numeric variable, so a histogram is used to visualize its frequency across different age ranges.
# geom_histogram() creates bins (here 30) and counts how many patients fall into each age range.
# fill = "orange" sets the color of the bars.
# This plot helps identify patterns like which age groups are most common and if there are any outliers.
ggplot(diabeties_data_set, aes(x = Age)) +
  geom_histogram(fill = "orange", bins = 30) +
  labs(title = "Age Distribution", x = "Age", y = "Count")

# Boxplot: Age vs Diabetes Class
# This plot visualizes the distribution of Age for each class (Negative / Positive for diabetes).
# geom_boxplot() shows the median, quartiles, and potential outliers for Age within each class.
# fill = class colors the boxes differently for each class.
# This helps to see if Age differs between patients with and without diabetes.
ggplot(diabeties_data_set, aes(x = class, y = Age, fill = class)) +
  geom_boxplot() + 
  labs(title = "Age vs Diabetes Class", x = "Class", y = "Age")

# Convert Gender to numeric for correlation check 
diabeties_data_set$Gender_numeric <- ifelse(diabeties_data_set$Gender == "Male", 1, 0)
numneric_variables <- c("Age","Gender_numeric")

# Correlation between Age and Gender. The correlation matrix shows how strongly numeric variables are related to each other. And in decisionn tree, variables which are uncorrelated are preferred for splitting nodes to improve model performance.
cor_matrix <- cor(diabeties_data_set[, numneric_variables])
cor_matrix
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)


# ================================
# 4. Train-Test Split (Your Method)
# ================================
set.seed(1234)

# Create training index (80% of the data for training and 20% for testing)
train_index <- createDataPartition(diabeties_data_set$class, p = 0.8, list = FALSE)


# Split 80% data for training
train <- diabeties_data_set[train_index, ]
summary(train$class)

# check Training Class Distribution after splitting dataset in percentage
check_class_split(train)

# Split the remaining 20% for testing
test  <- diabeties_data_set[-train_index, ]

# check Testing Class Distribution after splitting dataset in percentage
check_class_split(test)

# ================================
# 5. Train Decision Tree Model
# ================================
dt_model <- rpart(
  class ~ .,
  data = train,
  method = "class",
  parms = list(split = "gini"),
  control = rpart.control(cp = 0.01)
)

# Plot the Decision Tree with detailed node information
rpart.plot(
  dt_model,            # The trained decision tree model object from rpart()
  extra = 101,         # Display predicted class, class probabilities, and number of observations at each node
  fallen.leaves = TRUE,# Position the leaf nodes at the bottom of the plot for a cleaner appearance
  main = "Decision Tree" # Title of the plot
)

# ================================
# 6. Predictions & Evaluation
# ================================
pred_unpruned <- predict(dt_model, test, type = "class")

conf_unpruned <- confusionMatrix(pred_unpruned, test$class)

cat("\n--- Unpruned Tree Performance ---\n")
print(conf_unpruned)

# Extract important metrics
acc_unpruned <- conf_unpruned$overall["Accuracy"]
prec <- conf_unpruned$byClass["Pos Pred Value"]
rec  <- conf_unpruned$byClass["Sensitivity"]
f1   <- 2 * ((prec * rec) / (prec + rec))

cat("\nF1 Score:", round(f1, 4), "\n")

# ================================
# 7. Pruning to Improve Performance
# ================================
cat("\n--- Complexity Parameter Table ---\n")
printcp(dt_model)

optimal_cp <- dt_model$cptable[which.min(dt_model$cptable[,"xerror"]), "CP"]
cat("\nOptimal CP:", optimal_cp, "\n")

# Prune tree
dt_pruned <- prune(dt_model, cp = optimal_cp)

# Plot pruned tree
rpart.plot(dt_pruned, extra = 101, main = "Pruned Decision Tree")

# ================================
# 8. Evaluate Pruned Model
# ================================
pred_pruned <- predict(dt_pruned, test, type = "class")
conf_pruned <- confusionMatrix(pred_pruned, test$class)

cat("\n--- Pruned Tree Performance ---\n")
print(conf_pruned)

acc_pruned <- conf_pruned$overall["Accuracy"]

cat("\nAccuracy (Unpruned):", round(acc_unpruned, 4))
cat("\nAccuracy (Pruned):", round(acc_pruned, 4), "\n")