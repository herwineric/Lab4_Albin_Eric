

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
  
  reglin <- setRefClass("linreg", fields = list(Call = "formula", Coefficients = "numeric", S_t = "numeric", Residual = "numeric", Fits = "numeric",
                                                plots =  "list", txt_beta = "character" ),
                        
                        methods = list(
                          
                          print = function() {
                            
                            beta<-as.character(round(Coefficients,2))
                            namn<-txt_beta
                            
                            n<-max(c(nchar(beta),20))
                            if(TRUE%in%(nchar(namn)>n)){
                              namn[nchar(namn)>n]<-
                                paste(substr(namn[nchar(namn)>n],1,(n-3)),"...",sep="")
                            }
                            
                            for(i in 1:length(Coefficients)){
                              if((nchar(namn)[i]==nchar(beta)[i])==FALSE){
                                
                                andra<-c(namn[i],beta[i])
                                ny<-andra
                                antal<-max(nchar(andra))
                                
                                lagga_till<-antal-min(nchar(andra))
                                plus<-paste(rep(" ",lagga_till),collapse = "")
                                ny[nchar(andra)<antal]<-paste(plus,andra[nchar(andra)<antal],sep = "")
                                
                                namn[i]<-ny[1]
                                beta[i]<-ny[2]
                              }
                              
                            }
                            cat("Call:",sep="\n")
                            cat(paste(as.character(Call)[2],as.character(Call)[1],as.character(Call)[3]), sep="\n")
                            cat(sep="\n")
                            cat("Coefficients:",sep="\n")
                            cat(namn,sep="\t")
                            cat("","\n")
                            cat(beta,sep="\t")
                          },
                          
                          #########################
                          
                          plot = function() {
                            dataint <- data.frame(residual = Residual, fitos = Fits, std_residual = sqrt(abs(scale(Residual))))
                            #Residuals vs Fitted
                            plots <<- list(
                              ggplot(data = dataint, aes(x = fitos, y = residual) ) +
                              geom_point() + labs(x = "Fitted values", y = "Residuals") +
                              geom_smooth(method="loess", se = FALSE, color = "red") +
                              geom_hline(yintercept = 0) + theme_bw() + ggtitle("Residuals vs Fitted") +
                              theme(plot.title = element_text(hjust = 0.5)),
                            
                            #Standardized residuals vs Fitted
                            ggplot(data = dataint, aes(x = fitos, y = std_residual) ) +
                              geom_point() + labs(x = "Fitted values", y = "Standardized residuals") +
                              geom_smooth(method="loess", se = FALSE, color = "red") +
                              geom_hline(yintercept = 0) + theme_bw() + ggtitle("Standardized residuals vs Fitted") +
                              theme(plot.title = element_text(hjust = 0.5))
                            )
                            
                            plots
                            
                            # cat ("Press [enter] to continue")
                            # line <- readline()
                            # pl_1
                            # Sys.sleep(0.01)
                            # cat ("Press [enter] to continue")
                            # line <- readline()
                            # pl_2
                          }
                          
                        )
  )
  
  
  
  
  result <- reglin(Residual = c(resid), Fits = c(fitts), S_t = sigma_2_resid, txt_beta = rownames(betas), 
                   Coefficients = c(betas), Call = formula)
  
  
  
  return(result)
}

formula <- Y ~ x1 + x2 + x3

data <- data.frame(x1=rnorm(120,1,50),x2=rnorm(120,5,50), Y = rnorm(120, 7,35), x3=as.factor(rep(1:12)))


test <- linreg(Y ~ x1 + x2 + x3, data)

test$plot()

test$print()
