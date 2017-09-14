

#linreg


data <- data.frame(x1=rnorm(120,1,50),x2=rnorm(120,5,50), Y = rnorm(120, 7,35), x3=as.factor(rep(1:12)))


formula <- Y ~ x1 + x2 + x3



linreg <- function(formula, data){
  
  require(ggplot2)
  
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
  
  p_values <- 2*pt(abs(t_betas), df,lower.tail = FALSE)
  
  reglin <- setRefClass("linreg",fields = list(Call = "formula", Coefficients = "numeric", 
                                               Pvalues ="numeric", S_t = "numeric", Tvalue = "numeric",
                                            Residual = "numeric", Fits = "numeric"),
                        methods = list(  
                          
                          print.linreg <<- function(result) {
                            cat(paste(result$Call), "\n")
                            cat(paste(result$Coefficients), "\n")
                           },
                          
                          plot.linreg <<- function(result) {
                            dataint <- data.frame(residual = result$Residual, fitos = result$Fits, std_residual = sqrt(abs(scale(result$Residual))))
                            #Residuals vs Fitted
                            pl_1 <- ggplot(data = dataint, aes(x = fitos, y = residual) ) +
                              geom_point() + labs(x = "Fitted values", y = "Residuals") +
                              geom_smooth(method="loess", se = FALSE, color = "red") + 
                              geom_hline(yintercept = 0) + theme_bw() + ggtitle("Residuals vs Fitted") +
                              theme(plot.title = element_text(hjust = 0.5))
                            
                            #Standardized residuals vs Fitted
                            pl_2 <- ggplot(data = dataint, aes(x = fitos, y = std_residual) ) +
                              geom_point() + labs(x = "Fitted values", y = "Standardized residuals") +
                              geom_smooth(method="loess", se = FALSE, color = "red") + 
                              geom_hline(yintercept = 0) + theme_bw() + ggtitle("Standardized residuals vs Fitted") +
                              theme(plot.title = element_text(hjust = 0.5))
                            
                            func1 <- function()
                            cat ("Press [enter] to continue")
                            line <- readline()
                            pl_1
                            cat ("Press [enter] to continue")
                            line <- readline()
                            pl_2
                        },
                       
                        
                        summary.linreg <<- function(result){
                          cat("Call:", "\n")
                          cat(result$Call, "\n")
                          cat("Coefficients:", sep ="\n")
                          cat(c("", "Estimate", "Std.Error", "t value", "Pr(>|t|)"), sep = "\t")
                          cat("",sep="\n")
                          for(i in 1:length(rownames(betas))){
                            cat(substr(rownames(betas)[i],1,4),"", round(result$Coefficients,5)[i],"", 
                                round(result$S_t[i],5),"" , round(result$Pvalues[i], 5), sep="\t")
                            cat(sep="\n")
                          }
                        },
                        
                        resid.linreg <<- function(result){
                          
                        }
                  )
            )
  
  
  
  result <- reglin(Call = formula, Coefficients = c(betas), Pvalues = c(p_values),
                   S_t = c(var_betas), Tvalue = c(t_betas), Residual = c(resid), Fits = c(fitts))
  
  
  result
}




test <- linreg(Y ~ x1 + x2 + x3, data)

print(test)
plot(test)
summary(test)


resid(lm(Y ~ x1 + x2 + x3, data))




 


