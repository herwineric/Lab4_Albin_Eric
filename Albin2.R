require(ggplot2)

obj<-linreg(Petal.Length~Species, data = iris)
summary(lm(Petal.Length~Species, data = iris))
data<-iris
formula<-Petal.Length~Species

obj$print()
obj$pred()
obj$summary()
obj$resid()
obj$coef()
obj$plot()


linreg<-function(formula, data){
  
  
  
  x<-model.matrix(formula,data)
  y<-all.vars(formula)[1]
  y<-as.matrix(data[,names(data)==y])
  
  b_hat<-solve(t(x)%*%x)%*%t(x)%*%y
  y_fits<-x%*%b_hat
  e<-y-y_fits
  df<-length(y)-ncol(x)
  var_e<-(t(e)%*%e)/df
  
  var_b_hat<-as.numeric(var_e)*diag(solve((t(x)%*%x)))
  std_b_hat<-sqrt(var_b_hat)
  t_b_hat<-as.numeric(b_hat)/std_b_hat
  p_b_hat<-(1-(pt(abs(t_b_hat),df = df)))*2
  
  b_hat_numeric<-as.numeric(b_hat)
  names(b_hat_numeric)<-rownames(b_hat)
  
  
  
  input_var<-as.character(match.call(expand.dots = FALSE))
  
                                                
                                                

  result <- make_class_linreg_RC(Call=formula,
                                 Coefficients=b_hat_numeric,
                                 X_terms=x,
                                 Y_terms=y,
                                 Fits=as.numeric(y_fits),
                                 Residuals=as.numeric(e),
                                 df=df,
                                 Var_residuals=as.numeric(var_e),
                                 Std_betas=std_b_hat,
                                 tBetas=t_b_hat,
                                 Pvalues=p_b_hat,
                                 Input=input_var)
  
  
  
  return(result)
}
