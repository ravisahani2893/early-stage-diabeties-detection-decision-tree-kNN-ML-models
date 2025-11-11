# Author: RaviKumar Sahani
# Email: ravisahani2893@gmail.com

install.packages("corrplot")
install.packages("caret")
install.packages("rpart plot")
install.packages("RWeka")
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(rpart)
library(rpart.plot)
library(caret)
library(RWeka)

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


# Visualizing the distribution of the 'class' variable using a bar plot to check the balance between the two classes (Negative and Positive). If the Negative class is greater than the Positive class, it indicates an imbalanced dataset. If class is Imbalanced, it may have required special handling during model training to avoid bias.
ggplot(diabeties_data_set, aes(x = class)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Diabetes Class", x = "Diabetes (Yes=1 / No=0)", y = "Total Count")

# Exploring binary variables in the diabeties data set. Binary variables are those that have only two possible values i.e Yes/No, True/False, 0/1 etc.

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

convert_to_factor <- function(var) {
  is.factor(diabeties_data_set[[var]])

  as.factor(diabeties_data_set[[var]])
}
convert_to_factor("Gender")

# Plot all binary variables like Polyuria, Polydipsia etc to visualize their distributions.

# Distribution of Polyuria variable in the  data set.
ggplot(diabeties_data_set, aes(x = Polyuria)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Distribution of Polyuria", x = "Polyuria", y = "Count")

# Distribution of Polydipsia variable in the  data set.
ggplot(diabeties_data_set, aes(x = Polydipsia)) +
  geom_bar(fill = "lightcoral") +
  labs(title = "Distribution of Polydipsia", x = "Polydipsia", y = "Count")

# Distribution of Sudden.Weight.Loss variable in the  data set.
ggplot(diabeties_data_set, aes(x = sudden.weight.loss)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Distribution of Sudden Weight Loss", x = "Sudden Weight Loss", y = "Count")

# Distribution of Weakness variable in the  data set.
ggplot(diabeties_data_set, aes(x = weakness)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribution of Weakness", x = "Weakness", y = "Count")

# Distribution of Polyphagia variable in the  data set.
ggplot(diabeties_data_set, aes(x = Polyphagia)) +
  geom_bar(fill = "purple") +
  labs(title = "Distribution of Polyphagia", x = "Polyphagia", y = "Count")

# Distribution of Genital.Thrush variable in the  data set.
ggplot(diabeties_data_set, aes(x = Genital.thrush)) +
  geom_bar(fill = "orange") +
  labs(title = "Distribution of Genital Thrush", x = "Genital Thrush", y = "Count")

# Distribution of Visual blurring in the  data set.
ggplot(diabeties_data_set, aes(x = visual.blurring)) +
  geom_bar(fill = "orange") +
  labs(title = "Distribution of Visual Blurring", x = "Visual Blurring", y = "Count")

# Distribution of Itching variable in the data sets
ggplot(diabeties_data_set, aes(x = Itching)) +
  geom_bar(fill="red") +
  labs(title = "Distribution of Itching", x = "Itching", y = "Count")

# Distribution of Irritability variable in the data sets
ggplot(diabeties_data_set, aes(x = Irritability)) +
  geom_bar(fill="red") +
  labs(title = "Distribution of Irritability", x = "Irritability", y = "Count")


# Distribution of delayed.healing variable in the data sets
ggplot(diabeties_data_set, aes(x = delayed.healing)) +
  geom_bar(fill="red") +
  labs(title = "Distribution of Delayed Healing", x = "Delayed Healing", y = "Count")

# Distribution of Partial Paresis variable in the data sets
ggplot(diabeties_data_set, aes(x = partial.paresis)) +
  geom_bar(fill="red") +
  labs(title = "Distribution of Partial Paresis", x = "Partial Paresis", y = "Count")


# Distribution of Muscle stiffness in the data sets
ggplot(diabeties_data_set, aes(x = muscle.stiffness)) +
  geom_bar(fill="red") +
  labs(title = "Distribution of Muscle Stiffness", x = "Muscle Stiffness", y = "Count")

# Distribution of Alopecia variable in the data sets
ggplot(diabeties_data_set, aes(x = Alopecia)) +
  geom_bar(fill="red") +
  labs(title = "Distribution of Alopecia", x = "Alopecia", y = "Count")


# Distribution of Obesity in the data sets
ggplot(diabeties_data_set, aes(x = Itching)) +
  geom_bar(fill="red") +
  labs(title = "Distribution of Obesity", x = "Obesity", y = "Count")

# Creating a combined plot for all binary variable using facets for better visualization of their distributions.
plot_data_long <- diabeties_data_set %>%
  # Select all binary features
  select(all_binary_features) %>%
  
  # Reshape the data from wide to long format
  pivot_longer(
    cols = everything(), # Take all selected columns
    names_to = "Symptom", # Name the new column holding the variable names
    values_to = "Status" # Name the new column holding the status (Yes/No, 1/0, etc.)
  )

# 2. Create the combined ggplot using faceting
combined_symptom_plot <- ggplot(plot_data_long, aes(x = Status)) +
  geom_bar(fill = "red") +
  
  # Use facet_wrap to create a separate plot for each unique 'Symptom'
  facet_wrap(~ Symptom, scales = "free_x") +
  
  # Set overall labels and titles
  labs(
    title = "Distribution of Key Symptoms in the Dataset",
    x = "Symptom Status (e.g., Yes/No)",
    y = "Count"
  ) +
  
  # Optional: Theme adjustment for better appearance
  theme_minimal()

# Save the combined plot to a file
ggsave(
  filename = "output/symptom_distribution_plot.png", # The name of your output file
  plot = combined_symptom_plot,              # The plot object you want to save
  width = 8,                                 # Width of the image in inches
  height = 5,                                # Height of the image in inches
  dpi = 300                                 # Resolution of the image
)

# Explore non binary variables i.e Age, Gender in the data set.


# Plot histogram for Age variable to visualize its distribution. And identify outliers value if any. It helps us to visualize the age distribution of individuals in the dataset. Since Age is continuous variable, decision tree can split nodes based on age ranges to improve classification accuracy.
ggplot(diabeties_data_set, aes(x = Age)) +
  geom_histogram(fill = "orange", color = "black", bins = 30) +
  labs(title = "Distribution of Age", x = "Age", y = "Count")


# Check summary statistics for Age variable to identify outliers if any.
summary(diabeties_data_set$Age)

# Explore Gender variable in the data set. Plot bar plot to visualize its distribution.
table(diabeties_data_set$Gender)
ggplot(diabeties_data_set, aes(x = Gender)) +
  geom_bar(fill = "purple") +
  labs(title = "Gender Distribution", x = "Gender", y = "Count")

# Plot all binary variables like Polyuria, Ploydipsia etc vs 'class' target variable
plot_vs_class <- function(var) {
  ggplot(diabeties_data_set, aes_string(x = var, fill = "class")) +
    geom_bar(position = "dodge") +
    labs(title = paste(var, "vs Diabetes Class"), x = var, y = "Count")
}

table(diabeties_data_set$class)


# Apply plot_vs_class function to all binary variables
for (individual_feature in all_binary_features) {
  print(plot_vs_class(individual_feature))
}

# Check for correlation between numeric variables like Age and Gender
diabeties_data_set$Gender_numeric <- ifelse(diabeties_data_set$Gender == "Male", 1, 0)
diabeties_data_set

numneric_variables <- c("Age","Gender_numeric")
numneric_variables

# Correlation between Age and Gender. The correlation matrix shows how strongly numeric variables are related to each other. And in decisionn tree, variables which are uncorrelated are preferred for splitting nodes to improve model performance.
cor_matrix <- cor(diabeties_data_set[, numneric_variables])
cor_matrix
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)



# Summary counts for each binary variable by class
feature_class_summary <- diabeties_data_set %>%
  select(all_binary_features, class) %>%
  pivot_longer(cols = all_binary_features, names_to = "Feature", values_to = "Value") %>%
  group_by(Feature, Value, class) %>%
  summarise(Count = n()) %>%
  arrange(Feature)


feature_class_summary


set.seed(12345)



check_class_percentage_in_test_train_data_set <- function(dataset) {
  class_count <- table(dataset$class)
  total_sum <- sum(class_count)
  class_proportions <- (class_count / total_sum) * 100
  return(class_proportions)
}

total_data_count <- nrow(diabeties_data_set)
total_data_count
diabeties_data_set_rand <- diabeties_data_set[order(runif(total_data_count)), ]

View(diabeties_data_set_rand)
View(diabeties_data_set)


summary(diabeties_data_set$Polyuria)
summary(diabeties_data_set_rand$Polyuria)

head(diabeties_data_set)
head(diabeties_data_set_rand)


# Lets split the data into training and testing sets. Using 80% of the data for training and 20% for testing.



diabeties_data_train <- diabeties_data_set_rand[1:416, ]
diabeties_data_test <- diabeties_data_set_rand[417:520, ]

count(diabeties_data_train)
count(diabeties_data_test)

print(check_class_percentage_in_test_train_data_set(diabeties_data_train))
print(check_class_percentage_in_test_train_data_set(diabeties_data_test))


model_training <- rpart(class ~ ., 
                  data = diabeties_data_train, 
                  method = "class", 
                  parms = list(split = "gini"),   # Using Gini Index
                  control = rpart.control(cp = 0.01))

model_training
summary(model_training)
rpart.plot(model_training, digits = 4)

rpart.plot(model_training, digits = 4, fallen.leaves = TRUE,
type = 3, extra = 101)

# Make Predictions on Test Data ----
model_prediction <- predict(model_training, diabeties_data_test, type = "class")

summary(model_prediction)

# Evaluate Model Performance ----
conf_matrix <- confusionMatrix(model_prediction, diabeties_data_test$class)
print(conf_matrix)

# Extract and Print Metrics ----
accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
f1_score <- 2 * ((precision * recall) / (precision + recall))
f1_score



# Improving Model Performance
printcp(model_training)



optimal_cp_value <- model_training$cptable[which.min(model_training$cptable[,"xerror"]),"CP"]
cat("Optimal CP:", optimal_cp_value, "\n")

pruned_tree <- prune(model_training, cp = optimal_cp_value)

rpart.plot(pruned_tree, extra = 106, main = "Pruned Decision Tree")

pred_class_pruned <- predict(pruned_tree, diabeties_data_test, type = "class")

conf_mat_pruned <- confusionMatrix(pred_class_pruned, diabeties_data_test$class)
print(conf_mat_pruned)
print(conf_matrix)