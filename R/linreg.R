
#' A Reference Class to represent a bank account.
#'
#' @field balance A length-one numeric vector.


linreg <- setRefClass("linreg", fields = list(formula = "formula", data = "data.frame", Call = "character"),
                      
                      methods = list(
                        
                        initialize = function(formula, data){
                          
                          Call<<- deparse(substitute(data))
                          formula <<- formula
                          data <<- data
                        },
                        
                        ################# Print
                        
                        print = function() {
                          
                          "Hej hej"
                          
                          ##############
                          exp <-all.vars(expr = formula)[2:length(all.vars(expr = formula))]
                          
                          var1 <- paste("~",exp[1])
                          if(length(exp) > 1){
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
                          
                          n<-max(c(nchar(beta), 20))
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
                          
                          Input <- formula
                          

                          cat("Call:",sep="\n")
                          cat(paste("linreg("," formula=",Input[2],", "," data=",Input[3],", data=",Call,")",sep=""), sep="\n")
                          cat(sep="\n")
                          cat("Coefficients:",sep="\n")
                          cat(namn,sep="\t")
                          cat("","\n")
                          cat(beta,sep="\t")
                          cat("",sep="\n")
                          cat("",sep="\n")
                          
                          #paste("linreg("," formula=",Input[2],", "," data=",Input[3], "data=",")",sep="")
                        },
                        
                        ######################### Plots ?
                       
                        
                        plot = function() {
                          
                          ####################
                          exp <-all.vars(expr = formula)[2:length(all.vars(expr = formula))]
                          
                          var1 <- paste("~",exp[1])
                          if(length(exp) > 1){
                            for( i in 2:length(exp)){
                              var1 <- paste(var1, "+", exp[i])
                            }
                          }
                          
                          
                          
                          X <- model.matrix(as.formula(var1), data)
                          Y <- all.vars(expr = formula)
                          
                          betas <- solve((t(X) %*% X))%*%t(X) %*% data[,Y[1]==names(data)]
                          fitts <- X%*%betas
                          resid <- data[,Y[1]==names(data)] - fitts
                          ####################
                          
                          require(ggplot2)
                          
                          dataint <- data.frame(residual = resid, fitos = fitts, std_residual = sqrt(abs(scale(resid))))
                          #Residuals vs Fitted
                          list(
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
                        },
                        
                        
                        ############### Pred KLAR!!
                        
                        pred = function(){
                          
                          ####################
                          exp <-all.vars(expr = formula)[2:length(all.vars(expr = formula))]
                          
                          var1 <- paste("~",exp[1])
                          if(length(exp) > 1){
                            for( i in 2:length(exp)){
                              var1 <- paste(var1, "+", exp[i])
                            }
                          }
                          
                          
                          
                          X <- model.matrix(as.formula(var1), data)
                          Y <- all.vars(expr = formula)
                          
                          betas <- solve((t(X) %*% X))%*%t(X) %*% data[,Y[1]==names(data)]
                          fitts <- X%*%betas
                          
                          ####################
                          fitts <- c(fitts)
                          names(fitts) <- 1:length(fitts)
                          fitts
                          
                        }, 
                        
                        ############### Redisuals KLAR!
                        
                        resid = function(){
                          
                          ####################
                          exp <-all.vars(expr = formula)[2:length(all.vars(expr = formula))]
                          
                          var1 <- paste("~",exp[1])
                          if(length(exp) > 1){
                            for( i in 2:length(exp)){
                              var1 <- paste(var1, "+", exp[i])
                            }
                          }
                          
                          
                          
                          X <- model.matrix(as.formula(var1), data)
                          Y <- all.vars(expr = formula)
                          
                          betas <- solve((t(X) %*% X))%*%t(X) %*% data[,Y[1]==names(data)]
                          fitts <- X%*%betas
                          resid <- data[,Y[1]==names(data)] - fitts
                          
                          ####################
                          resid <- c(resid)
                          names(resid) <- 1:length(resid)
                          resid
                          
                        },
                        
                        
                        ############### Coefficients KLAR!
                       
                        coef = function(){
                          
                          ##############
                          exp <-all.vars(expr = formula)[2:length(all.vars(expr = formula))]
                          
                          var1 <- paste("~",exp[1])
                          if(length(exp) > 1){
                            for( i in 2:length(exp)){
                              var1 <- paste(var1, "+", exp[i])
                            }
                          }
                          
                          
                          
                          X <- model.matrix(as.formula(var1), data)
                          Y <- all.vars(expr = formula)
                          
                          betas <- solve((t(X) %*% X))%*%t(X) %*% data[,Y[1]==names(data)]
                          ###############
                          
                          
                          coefi <- c(betas)
                          names(coefi) <- rownames(betas)
                          coefi
                        },
                        
                        ####################### Summary
                        summary = function(){
                          
                          ##############
                          exp <-all.vars(expr = formula)[2:length(all.vars(expr = formula))]
                          
                          var1 <- paste("~",exp[1])
                          if(length(exp) > 1){
                            for( i in 2:length(exp)){
                              var1 <- paste(var1, "+", exp[i])
                            }
                          }
                          
                          
                          
                          X <- model.matrix(as.formula(var1), data)
                          Y <- all.vars(expr = formula)
                          
                          betas <- solve((t(X) %*% X))%*%t(X) %*% data[,Y[1]==names(data)]
                          
                          fitts <- X%*%betas
                          resid <- data[,Y[1]==names(data)] - fitts
                          df <- nrow(data)- length(colnames(X))
                          sigma_2_resid <- (t(resid) %*% resid)/df
                          sigma_2_resid <- c(sigma_2_resid)
                          
                          var_betas <- sigma_2_resid * diag(solve(t(X) %*% X))
                          
                          t_betas <- betas/sqrt(var_betas)
                          
                          p_values <- 2*pt(abs(t_betas), df,lower.tail = FALSE)
                          
                          ##############
                          
                          namn<-rownames(betas)
                          
                          n<-max(c(nchar(namn),20))
                          if(TRUE%in%(nchar(namn)>n)){
                            namn[nchar(namn)>n]<-
                              paste(substr(namn[nchar(namn)>n],1,(n-3)),"...",sep="")
                          }
                          n<-max(nchar(namn))
                          
                          
                          for(i in 1:length(namn)){
                            if(nchar(namn[i])<n){
                              
                              lagga_till<-n-nchar(namn[i])
                              plus<-paste(rep(" ",lagga_till),collapse = "")
                              namn[i]<-paste(plus,namn[i],sep = "")
                            }
                          }
                          plus<-paste(rep(" ",n),collapse = "")
                          namn<-c(plus,namn)
                          
                          
                          
                          Estimate<-as.character(round(betas,3))
                          n<-max(nchar(Estimate),nchar("Estimate"))
                          for(i in 1:length(Estimate)){
                            if(nchar(Estimate[i])<n){
                              lagga_till<-n-nchar(Estimate[i])
                              plus<-paste(rep(" ",lagga_till),collapse = "")
                              Estimate[i]<-paste(plus,Estimate[i],sep = "")
                            }
                          }
                          Estimate<-c("Estimate",Estimate)
                          
                          
                          Std_Error<-as.character(round(var_betas,3))
                          n<-max(nchar(c(Std_Error,"Std.Error")))
                          for(i in 1:length(Std_Error)){
                            if(nchar(Std_Error[i])<n){
                              lagga_till<-n-nchar(Std_Error[i])
                              plus<-paste(rep(" ",lagga_till),collapse = "")
                              Std_Error[i]<-paste(plus,Std_Error[i],sep = "")
                            }
                          }
                          Std_Error<-c("Std.Error",Std_Error)
                          
                          
                          t_value<-as.character(round(t_betas,3))
                          n<-max(nchar(c(t_value,"t value")))
                          for(i in 1:length(t_value)){
                            if(nchar(t_value[i])<n){
                              lagga_till<-n-nchar(t_value[i])
                              plus<-paste(rep(" ",lagga_till),collapse = "")
                              t_value[i]<-paste(plus,t_value[i],sep = "")
                            }
                          }
                          t_value<-c("t value",t_value)
                          
                          
                          Pr_t<-as.character(round(p_values,4))
                          n<-max(nchar(c(Pr_t,"Pr(>|t|)")))
                          Pr_t[Pr_t=="0"]<-"<0.0001"
                          for(i in 1:length(Pr_t)){
                            if(nchar(Pr_t[i])<n){
                              lagga_till<-n-nchar(Pr_t[i])
                              plus<-paste(rep(" ",lagga_till),collapse = "")
                              Pr_t[i]<-paste(plus,Pr_t[i],sep = "")
                            }
                          }
                          Pr_t<-c("Pr(>|t|)",Pr_t)
                          
                          
                          
                          as.character(match.call(expand.dots = FALSE))
                          Input <- formula
                          
                          
                          cat("Call:", "\n")
                          cat(paste("linreg("," formula=",Input[2],", "," data=",Input[3],")",sep=""), sep="\n")
                          cat(" ",sep="\n")
                          cat("Coefficients:",sep="\n")
                          cat(paste(namn,Estimate,Std_Error,t_value,Pr_t),sep="\n")
                          cat(" ",sep="\n")
                          cat( paste("Residual standard error (\u03C3):",round(sigma_2_resid,5)),sep="\n")
                          cat( paste("df:",df),sep="\n")
                          
                          
                          
                        }
                        
                      )
)




linreg_mod <- linreg$new(formula = formula, data =iris)$resid()
linreg_mod$print()

linreg_mod@.data


formula <- Sepal.Length ~ Species
setClass("Person",
         slots = list(name = "character", age = "numeric"))
setClass("Employee",
         slots = list(boss = "Person"),
         contains = "Person")
data(iris)
alice <- new("Person", name = "Alice", age = 40)
john <- new("Employee", name = "John", age = 20, boss = alice)


