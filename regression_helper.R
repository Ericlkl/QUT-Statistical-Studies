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

