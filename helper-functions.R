## PredictRSA helper function

predictRSA <- function(object, X, Y, model="full") {
  C <- coef(object$models[[model]])
  if (object$models[[model]]@Options$estimator != "DWLS") {
    b0 <- as.numeric(ifelse(is.na(C[paste0(object$DV, "~1")]), b0, C[paste0(object$DV, "~1")]))
  } else {
    # the threshold is the negative of the intercept ...
    b0 <- -as.numeric(ifelse(is.na(C[paste0(object$DV, "|t1")]), b0, C[paste0(object$DV, "|t1")]))
  }
  x <- as.numeric(ifelse(is.na(C["b1"]), 0, C["b1"]))
  y <- as.numeric(ifelse(is.na(C["b2"]), 0, C["b2"]))
  x2 <- as.numeric(ifelse(is.na(C["b3"]), 0, C["b3"]))
  y2 <- as.numeric(ifelse(is.na(C["b5"]), 0, C["b5"]))
  xy <- as.numeric(ifelse(is.na(C["b4"]), 0, C["b4"]))
  w <- as.numeric(ifelse(is.na(C["w1"]), 0, C["w1"]))
  wx <- as.numeric(ifelse(is.na(C["w2"]), 0, C["w2"]))
  wy <- as.numeric(ifelse(is.na(C["w3"]), 0, C["w3"]))
  
  # cubic parameters
  x3 <- as.numeric(ifelse(is.na(C["b6"]), 0, C["b6"]))
  x2y <- as.numeric(ifelse(is.na(C["b7"]), 0, C["b7"]))
  xy2 <- as.numeric(ifelse(is.na(C["b8"]), 0, C["b8"]))
  y3 <- as.numeric(ifelse(is.na(C["b9"]), 0, C["b9"]))
  
  
  C <- c(x, y, x2, y2, xy, w, wx, wy,x3, x2y, xy2, y3)
  
  # compute predicted value
  Z <- b0 + colSums(C*t(cbind(X, Y, X^2, Y^2, X*Y, 0, 0, 0, X^3, X^2*Y, X*Y^2, Y^3)))
  return(Z)
}

C <- coef(sem_piece$models[[model]])
