# Import Library
library(ggplot2)
library(dplyr)

# Import Data 
uniData <- read.csv(file.choose(), header = TRUE)

# Global Variable
category_cols_names <- select(uniData,2:15) %>%
  select_if(is.factor) %>% 
  colnames()

# Task 1  ----------------------------------------------------------------------
# Summary the information in each variable (expect case ID) using a table
select(uniData,2:15) %>%
  summary()

# using an appropriate graph
displayBarChart <- function(){
  # Fill = The color of the area
  # Color = The border color
  for (colname in category_cols_names ){
    barChart = ggplot(uniData, aes(x = uniData[,colname], fill= uniData[,colname] )) + 
        geom_bar() + 
        labs( x = colname, 
              y = "Number of Students", 
              fill = colname,
              title = paste(colname,"Bar Chart"))
    
    print(barChart)
  }
}

# using an appropriate graph
displayNumericGraphs <- function(){
  
  # Density Graph
  print ( 
    ggplot(uniData, aes(x = Age )) + 
    geom_density() + 
    labs(x = "Age", y = "Frequency", title = "Density Graph of Age") + 
    xlim(18,35) 
  )
  
  print ( 
    ggplot(uniData, aes(x = Achieved_Credit_Points )) + 
    geom_density() + 
    xlim(0, 220) + 
    labs(x = "Achieved_Credit_Points" , y = "Frequency", title = "Density Graph of Achieved_Credit_Points" ) 
  )
  
  # Histogram
  print (
    ggplot(uniData, aes(x = GPA )) + 
    geom_histogram(bins = 14, color="darkblue", fill="lightblue") + 
    labs( x = "GPA" , y = "Number of Students", title = "Histogram of GPA") 
  )
  
  print (
    ggplot(uniData, aes(x = OP_Score )) + 
    geom_histogram(bins = 10, color="darkblue", fill="lightblue") + 
    labs( x = "OP_Score" , y = "Number of Students", title = "Histogram of OP Score" ) 
  )
  
  # Without Log
  print (
    ggplot(uniData, aes(x = Failed_Credit_Points )) + 
    geom_histogram(bins = 5, color="darkblue", fill="lightblue") + 
    labs( x = "Failed_Credit_Point" , y = "Number of Students", title = "Histogram of Failed_Credit_Point" ) 
  )
  
}

# Appropriate way to display categorical Data
displayBarChart()
# Appropriate way to display numeric Data
displayNumericGraphs()

# Task 2 ----------------------------------------------------------
# Compare average GPA between Male and Female 
# Conduct a statistical Test
# Interpret its results
attach(uniData)

uniData %>%
  ggplot(aes(x=Gender, y = GPA)) + geom_boxplot() + labs( x = "BoxPlot ( GPA vs Gender )" ) 

# T Test
t.test(GPA ~ Gender)
# Variance
var.test(GPA ~ Gender)

# Task 3 ----------------------------------------------------------
# Explore the relationship between OP Score and GPA using Graph 

uniData %>%
  mutate(GPA_Round = as.factor(round(GPA)) ) %>%
  ggplot( aes(x = OP_Score, fill = GPA_Round) ) + 
  geom_bar(position = "fill") + 
  labs( x = "OP Score ( 1 (highest) -> 25 (lowest) performance )", 
        y = "proportion of students getting Each OP score", 
        title = "OP Score Barchart (colored by Each GPA Group) ") 

uniData %>%
  mutate(GPA_Round = as.factor(round(GPA)) ) %>%
  ggplot( aes(x = OP_Score) ) + 
  geom_histogram(bins = 10, color="darkblue", fill="lightblue") + 
  facet_wrap(~GPA_Round) +
  labs( x = "OP Score ( 1 (highest) -> 25 (lowest) performance )", 
        y = "Number of Students", 
        title = "OP Score histogram (colored by Each GPA Group) ") 

# Task 4 ----------------------------------------------------------
