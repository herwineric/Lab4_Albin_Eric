#'@title Takes the euclidian algorithm
#'
#'@description An algorithm that computes the greatest common divisor (GDC) of two numeric numbers.
#'@param a shall be a numeric value and a scalar
#'@param b shall be a numeric value and a scalar
#'@export
#'@return Returns a numeric scalar that is the greatest common divisor of a and b.
#'@examples 
#'euclidean(123612, 13892347912)
#'euclidean(100, 1000)
#'@references The info about the algorithm can be found at this link  
#' \href{https://en.wikipedia.org/wiki/Euclidean_algorithm}{wikepedia.}

linreg <- function(formula, data){
  
  require(ggplot2)
  
  exp <-all.vars(expr = formula)[2:length(all.vars(expr = formula))]
  
  var1 <- paste("~",exp[1])
  for( i in 2:length(exp)){
    var1 <- paste(var1, "+", exp[i])
  }
  
  
  X <- model.matrix(as.formula(var1), data)
  Y <- all.vars(expr = formula)
  
  betas <- solve((t(X) %*% X))%*%t(X) %*% data[,Y[1]==names(data)]
  
  fitts <- X%*%betas
  resid <- data$Y - fitts
  df <- nrow(data)- length(colnames(X))
  sigma_2_resid <- (t(resid) %*% resid)/df
  sigma_2_resid <- c(sigma_2_resid)
  
  var_betas <- sigma_2_resid * diag(solve(t(X) %*% X))
  
  t_betas <- betas/sqrt(var_betas)
  
  p_values <- 2*pt(abs(t_betas), df,lower.tail = FALSE)
  
  
  input_data <- as.character(match.call(expand.dots = FALSE))
  
  result <- reglin(Residual = c(resid), Fits = c(fitts), S_t = sigma_2_resid, txt_beta = rownames(betas), 
                   Coefficients = c(betas), Call = formula, sigma_betas = var_betas, Tvalue = c(t_betas), 
                   Pvalues = c(p_values), Input = input_data, Var_residuals = sigma_2_resid, df = df)
  
  return(result)
  
  
}

# 
# 
# formula <- Y ~ x1 + x2 + x3
# 
# formula <- Sepal.length ~ Species
# 
# data <- data.frame(x1=rnorm(120,1,50),x2=rnorm(120,5,50), Y = rnorm(120, 7,35), x3=as.factor(rep(1:12)))
# 
# 
# test <- linreg(Y ~ x1 + x2 + x3, data)
# 
# 
# 
# test$Residual
# test$resid()
# 
# test$summary()
# 
# test$
# 
# test$coef
# 
# 
# 
# test$pred()
# test$print()
