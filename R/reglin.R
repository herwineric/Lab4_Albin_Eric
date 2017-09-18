
#' A Reference Class to represent a bank account.
#'
#' @field balance A length-one numeric vector.


linreg <- setRefClass("linreg", fields = c("formula", "data"),
                      
                      methods = list(
                        
                        ################# Print
                        
                        print = function() {
                          
                          "Hej hej"
                          
                          ##############
                          exp <-all.vars(expr = formula)[2:length(all.vars(expr = formula))]
                          
                          var1 <- paste("~",exp[1])
                          if(length(var1) > 1){
                            for( i in 2:length(exp)){
                              var1 <- paste(var1, "+", exp[i])
                            }
                          }
                          
                          
                          
                          X <- model.matrix(as.formula(var1), data)
                          Y <- all.vars(expr = formula)
                          
                          betas <- solve((t(X) %*% X))%*%t(X) %*% data[,Y[1]==names(data)]
                          ###############
                          
                          beta<-as.character(round(betas,2))
                          namn<-rownames(betas)
                          
                          n<-max(c(nchar(beta),20))
                          if(TRUE%in%(nchar(namn)>n)){
                            namn[nchar(namn)>n]<-
                              paste(substr(namn[nchar(namn)>n],1,(n-3)),"...",sep="")
                          }
                          
                          for(i in 1:length(betas)){
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
                          cat(paste(as.character(formula)[2],as.character(formula)[1],as.character(formula)[3]), sep="\n")
                          cat(sep="\n")
                          cat("Coefficients:",sep="\n")
                          cat(namn,sep="\t")
                          cat("","\n")
                          cat(beta,sep="\t")
                        }
                        
                        ######################### Plots
                        
                      )
)

linreg$print()
linreg_mod <- linreg$new(formula = Petal.Length~Sepal.Width+Sepal.Length, data=iris)
linreg_mod$print()
