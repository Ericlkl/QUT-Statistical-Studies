# Import Library
library(ggplot2)
library(dplyr)

# Import Data 
uniData <- read.csv(file.choose(), header = TRUE)

# Global Variable
category_cols_names <- select(uniData,2:15) %>%
  select_if(is.factor) %>% 
  colnames()

numeric_cols_names <- select(uniData,2:15) %>%
  select_if(is.numeric) %>% 
  colnames()

# Task 1
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
        labs( x = colname )
    
    print(barChart)
  }
}

# using an appropriate graph
displayNumericGraphs <- function(){
  
  # Density Graph
  print ( ggplot(uniData, aes(x = Age )) + geom_density() + labs( x = "Age" ) + xlim(18,35) )
  print ( ggplot(uniData, aes(x = Achieved_Credit_Points )) + geom_density() + xlim(0, 220) + labs( x = "Achieved_Credit_Points" ) )
  # Histogram
  print (ggplot(uniData, aes(x = GPA )) + geom_histogram(bins = 7) + labs( x = "GPA" ) )
  print (ggplot(uniData, aes(x = OP_Score )) + geom_histogram(bins = 5) + labs( x = "OP_Score" ) )
  # Without Log
  print (ggplot(uniData, aes(x = Failed_Credit_Points )) + geom_histogram(bins = 5) + labs( x = "Failed_Credit_Point" ) )
}

# Appropriate way to display categorical Data
displayBarChart()
# Appropriate way to display numeric Data
displayNumericGraphs()

