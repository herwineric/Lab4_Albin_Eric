

#linreg


data <- data.frame(x1=rnorm(100,1,50),x2=rnorm(100,5,50), Y = rnorm(100, 7,35))


formulas <- Y ~ x1 + x2



linreg <- function(formula, data){
  
  exp <-all.vars(expr = formula)[2:length(all.vars(expr = formula))]
  
  var1 <- paste("~",exp[1])
  for( i in 2:length(exp)){
    var1 <- paste(var1, "+", exp[i])
  }
  
  
  X <- model.matrix(as.formula(var1), data)
  Y <- all.vars(expr = formula)
  
  betas <- solve((t(X)%*%X))%*%t(X) %*% data[,Y[1]==names(data)]
  
  fitts <- X%*%betas
  resid <- data$Y - fitts
  df <- nrow(data)- length(Y)
  sigma_2_resid <- (t(resid) %*% resid)/df
  sigma_2_resid <- c(sigma_2_resid)
  
  var_betas <- sigma_2_resid * diag(solve(t(X) %*% X))
  
  t_betas <- betas/sqrt(var_betas)
  
  p_values <- pt(q = t_betas,df = df)
  
  reglin <- setRefClass("linreg",fields = c("Call", "Coefficients", "Xterms", "Yterms", "Fitts", 
                                            "Resid", "df", "Sigma_2_resid", 
                                            "Var_betas", "tBetas", "Pvalues") )
  
  result <- reglin(Call = formula, Coefficients = betas, Xterms=X, Yterms=Y, Fitts=fitts, 
                   Resid=resid, df=df, 
                   Sigma_2_resid=sigma_2_resid, Var_betas=var_betas, 
                   tBetas=t_betas, Pvalues = p_values)
  
  return(result)
  
}

linreg(Y ~ x1 + x2, data)

mod_object <- linreg(Y ~ x1 + x2, data)
print(mod_object)

mod_object <- lm(Petal.Length~Species, data = iris)

plot(mod_object)


