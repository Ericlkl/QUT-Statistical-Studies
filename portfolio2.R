# Import Library
library(ggplot2)
library(dplyr)

# Setting up the working directory
# So that It can import external file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("data_visualization.R")

# Import Data 
uniData <- read.csv("datasets/Portfolio_2_data.csv", header = TRUE) %>%
  select(2:15)

# Task 1  ----------------------------------------------------------------------
# Summary the information in each variable (expect case ID) using a table
uniData %>%
  summary()

# Appropriate way to display categorical Data
visualize_categorical_data(uniData)
# Appropriate way to display numeric Data
visualize_numerical_data(uniData)

# Task 2 ----------------------------------------------------------
# Compare average GPA between Male and Female 
# Conduct a statistical Test
# Interpret its results
visualize_boxplot_gpa_vs_gender(uniData)

# Task 3 ----------------------------------------------------------
# Explore the relationship between OP Score and GPA using Graph 
visualize_relationship_op_and_gpa(uniData)

# Task 4 ----------------------------------------------------------
# Linear Regression

# Visualize Scatterplot by each numerical vs GPA
# and calculate the correlation
visualize_scatterplots_Vs_GPA(uniData)

set.seed(2)
library(caTools)

predict_df <- uniData %>%
  select(c("Age", "Failed_Credit_Points", "OP_Score", "Achieved_Credit_Points", "GPA"))

split <- sample.split(predict_df, SplitRatio = 0.7)
train <- subset(predict_df, split==TRUE)
test <- subset(predict_df, split==FALSE)

linear_model_age <- lm(GPA ~ Age, data=train)
summary(linear_model_age)

linear_model_Achieved_Credit_Points <- lm(GPA ~ Achieved_Credit_Points, data=train)
summary(linear_model_Achieved_Credit_Points)
