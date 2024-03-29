print_R2_and_pvalue <- function(null_deviance, deviance) {
  ll.null <- null_deviance / -2
  ll.proposed <- deviance / -2
  
  # McFadden's Pseudo R^2 = [ LL(Null) - LL (Proposed) ] / LL(Null)
  R2 <- (ll.null - ll.proposed) / ll.null
  
  # p-value
  p_value <- 1 - pchisq( 2*(ll.proposed - ll.null), df=1 )
  print( paste("R^2 : ",  R2))
  print( paste("P-value : ",  p_value))
}

print_MCE_Sens_Spec <- function(actuals,probability){
  # Calculate Cut off
  optCutOff = optimalCutoff(actuals, probability)[1]
  # Print Cut off
  print(paste ("Optimal Cut off :", optCutOff))
  # Print MisClassification Error
  print(paste("MisClassification Error : ",  misClassError(actuals, probability, threshold = optCutOff ) ))
  # Print Sensitivity
  print(paste("sensitivity: ",  sensitivity(actuals, probability, threshold = optCutOff ) ))
  # Print Specificity
  print(paste("specificity : ", specificity( actuals, probability,threshold = optCutOff ) ))
}

print_ConfusionMatrix <- function(actuals, probability){
  # Calculate Cut off
  optCutOff = optimalCutoff(actuals, probability)[1]
  # Print Confusion Matrix
  print( confusionMatrix( actuals, probability, threshold = optCutOff ))
}


visualize_ROC_Curve <- function(actuals, probability) {
  # Display ROC Curve On Screen
  library(InformationValue)
  plotROC(actuals = actuals, predictedScores = probability)
}