# numeric_cols_names <- select(uniData,2:15) %>%
#   select_if(is.numeric) %>% 
#   colnames()

# using an appropriate graph
visualize_categorical_data <- function(uniData){
  
  # Global Variable
  category_cols_names <- uniData %>%
    select_if(is.factor) %>% 
    colnames()
  
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
visualize_numerical_data <- function(uniData){
  
  # Density Graph
  print ( 
    ggplot(uniData, aes(x = Age )) + 
      geom_density() + 
      labs(x = "Age", y = "Frequency", title = "Density Graph of Age") + 
      xlim(18,35) 
  )
  
  # Histogram
  print ( 
    ggplot(uniData, aes(x = Achieved_Credit_Points )) + 
      geom_histogram(bins = 10, color="darkblue", fill="lightblue") + 
      xlim(0, 220) + 
      labs(x = "Achieved_Credit_Points" , y = "Frequency", title = "Density Graph of Achieved_Credit_Points" ) 
  )
  
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
      geom_histogram(bins = 10, color="darkblue", fill="lightblue") + 
      labs( x = "Failed_Credit_Point" , y = "Number of Students", title = "Histogram of Failed_Credit_Point" ) 
  )
  
}

visualize_boxplot_gpa_vs_gender <- function(uniData){
  
  # Visualize boxplot graph (GPA Vs Gender)
  print(
    uniData %>%
      ggplot(aes(x= Gender, y = GPA, color=Gender)) + 
      geom_boxplot() + 
      labs( x= "Gender", title = "BoxPlot ( GPA vs Gender )" ) +
      ylim(0, 7)
  )
}

visualize_relationship_op_and_gpa <- function(uniData){
  print(
    uniData %>%
      mutate(GPA_Round = as.factor(round(GPA)) ) %>%
      ggplot( aes(x = OP_Score, fill = GPA_Round) ) + 
      geom_bar(position = "fill") + 
      labs( x = "OP Score ( 1 (highest) -> 25 (lowest) performance )", 
            y = "proportion of students getting Each OP score", 
            title = "OP Score Barchart (colored by Each GPA Group) ") 
  )
  
  print(
    uniData %>%
      mutate(GPA_Round = as.factor(round(GPA)) ) %>%
      ggplot( aes(x = OP_Score) ) + 
      geom_histogram(bins = 10, color="darkblue", fill="lightblue") + 
      facet_wrap(~GPA_Round) +
      labs( x = "OP Score ( 1 (highest) -> 25 (lowest) performance )", 
            y = "Number of Students", 
            title = "OP Score histogram (Divided by Each GPA Group) ")  
  )
}

visualize_scatterplots_Vs_GPA <-function(uniData){
  
  par(xpd=FALSE)
  
  # Age vs GPA Scatter plot
  scatter.smooth(
    x=uniData$Age, y=uniData$GPA, main="Age vs GPA", col="blue",
    xlab="Age", ylab="GPA"
  )
  # Add Prediction line
  abline(lm(uniData$GPA ~ uniData$Age), col="red", lty=2, lwd=2)
  
  # Legend Setup
  legend("bottomright", 
         legend = c("Linear Regression Line", "Loess Line"), 
         col = c("red", "black"), lty=2:1, cex=0.8
  )
  
  # OP_Score vs GPA Scatter plot
  scatter.smooth(
    x= uniData$OP_Score, y= uniData$GPA, main="OP_Score vs GPA" , col="blue",
    xlab="Score", ylab="GPA"
  )
  # Add Prediction line
  abline(lm(uniData$GPA ~ uniData$OP_Score), col="red", lty=2, lwd=2)
  
  # Legend Setup
  legend("bottomright", 
         legend = c("Linear Regression Line", "Loess Line"), 
         col = c("red", "black"), lty=2:1, cex=0.8
  )
  
  # Achieved_Credit_Points vs GPA Scatter plot
  scatter.smooth(
    x= uniData$Achieved_Credit_Points, y= uniData$GPA, main="Achieved_Credit_Points vs OP_Score", 
    col="blue", xlab="Achieved Credit Points", ylab="GPA"
  )
  # Add Prediction line
  abline(lm(uniData$GPA ~ uniData$Achieved_Credit_Points), col="red", lty=2, lwd=2)
  # Legend Setup
  legend("bottomright", 
         legend = c("Linear Regression Line", "Loess Line"), 
         col = c("red", "black"), lty=2:1, cex=0.8
  )
  
  # Failed_Credit_Points vs GPA Scatter plot
  scatter.smooth(
    x= uniData$Failed_Credit_Points, y= uniData$GPA, main="Failed_Credit_Points vs OP_Score", 
    col="blue", xlab="Failed Credit Points", ylab="GPA"
  )
  # Add Prediction line
  abline(lm(uniData$GPA ~ uniData$Failed_Credit_Points), col="red", lty=2, lwd=2)
  
  # Move Legend to TopRight
  legend("topright", 
         legend = c("Linear Regression Line", "Loess Line"), 
         col = c("red", "black"), lty=2:1, cex=0.8
  )
  
}

