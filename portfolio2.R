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

split <- sample.split(uniData, SplitRatio = 0.7)
train <- subset(uniData, split==TRUE)
test <- subset(uniData, split==FALSE)

linear_model <- lm(GPA ~ Achieved_Credit_Points, data=train)
summary(linear_model)

plot(linear_model)

# Task 5 -------------------------------------------------------------
# Logistic Regression

# Cross Table
xtabs(~ uniData$Attrition + uniData$Socio_Economic_Status)

# logistic Regression Model
log_model <- glm(Attrition ~ Socio_Economic_Status, data=uniData, family = "binomial")
summary(log_model)

ll.null <- log_model$null.deviance / -2
ll.proposed <- log_model$deviance / -2

# McFadden's Pseudo R^2 = [ LL(Null) - LL (Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

# p-value
p_value <- 1 - pchisq( 2*(ll.proposed - ll.null), df=1 )
p_value

predicted.data <- data.frame(
  probability.of.Attrition = log_model$fitted.values,
  Socio_Economic_Status = uniData$Socio_Economic_Status
)

xtabs(~ probability.of.Attrition + Socio_Economic_Status ,data=predicted.data)

# Logistic Regression with all predictors
log_model <- glm(Attrition ~ ., data=uniData, family = "binomial")
summary(log_model)

ll.null <- log_model$null.deviance / -2
ll.proposed <- log_model$deviance / -2

# McFadden's Pseudo R^2 = [ LL(Null) - LL (Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

# p-value
p_value <- 1 - pchisq( 2*(ll.proposed - ll.null), df=1 )
p_value

library(DAAG)
vif(log_model)

predicted.data <- data.frame(
  probability.of.Attrition = log_model$fitted.values,
  Attrition = uniData$Attrition
)

# Sort 
predicted.data <- predicted.data[order(predicted.data$probability.of.Attrition, decreasing = FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

library(cowplot)

ggplot(data=predicted.data, aes(x=rank, y=probability.of.Attrition) ) +
  geom_point(aes(color=Attrition), alpha = 1, shape = 4, stroke = 2) +
  xlab("Index") +
  ylab("Predicted probability of Attrition")

library(InformationValue)

predicted.data$actuals <- factor(predicted.data$Attrition, labels = c(0,1))

plotROC(actuals = predicted.data$actuals, predictedScores = predicted.data$probability.of.Attrition)

optCutOff = optimalCutoff(predicted.data$actuals, predicted.data$probability.of.Attrition)[1]

misClassError(
  predicted.data$actuals,
  predicted.data$probability.of.Attrition, 
  threshold = optCutOff 
)

sensitivity(predicted.data$actuals,
            predicted.data$probability.of.Attrition, 
            threshold = optCutOff )

specificity(predicted.data$actuals,
            predicted.data$probability.of.Attrition, 
            threshold = optCutOff )

confusionMatrix(predicted.data$actuals,
                predicted.data$probability.of.Attrition, 
                threshold = optCutOff )




