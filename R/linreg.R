

#linreg


data <- data.frame(x1=rnorm(100,1,50),x2=rnorm(100,5,50), Y = rnorm(100, 7,35))
X <- model.matrix(~ x1 + x2, data)



Y <- all.vars(expr = Y ~ x1 + x2)
Y[1]



#Beta, fitted values, residuals, df, variance of residuals
betas <- solve((t(X)%*%X))%*%t(X)%*%data$Y


fitts <- X%*%betas

resid <- data$Y - fitts

df <- nrow(data)- length(Y[2:length(Y)])

sigma_2_resid <- (t(resid) %*% resid)/df


var_betas <- c(sigma_2_resid) * diag(solve(t(X) %*% X))


t_betas <- betas/sqrt(var_betas)

pt(q = t_betas,df = df)

formula <- Y ~ x1 + x2


formula(formula[3])


linreg <- function(formula, data){
  
  X <- model.matrix(~ x1 + x2, data)
  Y <- all.vars(expr = formula)
  
  
  
  
}




