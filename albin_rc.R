

make_class_linreg_RC<-setRefClass("linreg", fields = list(Call="formula",
                                                          Coefficients="numeric",
                                                          X_terms="matrix",
                                                          Y_terms="matrix",
                                                          Fits="numeric",
                                                          Residuals="numeric",
                                                          df="numeric",
                                                          Var_residuals="numeric",
                                                          Std_betas="numeric",
                                                          tBetas="numeric",
                                                          Pvalues="numeric",
                                                          Input="character"),
                                  methods = list(
                                    pred = function(){
                                      "Pred function 1"
                                      retur<-Fits
                                      names(retur)<-1:length(retur)
                                      return(retur)
                                    },
                                    
                                    print = function() {
                                      "Print function 2"
                                      beta<-as.character(round(Coefficients,2))
                                      namn<-names(Coefficients)
                                      
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
                                      cat(paste("linreg("," formula=",Input[2],", "," data=",Input[3],")",sep=""), sep="\n")
                                      cat(sep="\n")
                                      cat("Coefficients:",sep="\n")
                                      cat(namn,sep="\t")
                                      cat("","\n")
                                      cat(beta,sep="\t")
                                      cat("",sep="\n")
                                      cat("",sep="\n")
                                      
                                    },
                                    
                                    summary = function(){
                                      "Summary function 3"
                                      namn<-names(Coefficients)
                                      
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
                                      
                                      
                                      
                                      Estimate<-as.character(round(Coefficients,3))
                                      n<-max(nchar(Estimate),nchar("Estimate"))
                                      for(i in 1:length(Estimate)){
                                        if(nchar(Estimate[i])<n){
                                          lagga_till<-n-nchar(Estimate[i])
                                          plus<-paste(rep(" ",lagga_till),collapse = "")
                                          Estimate[i]<-paste(plus,Estimate[i],sep = "")
                                        }
                                      }
                                      Estimate<-c("Estimate",Estimate)
                                      
                                      
                                      Std_Error<-as.character(round(Std_betas,3))
                                      n<-max(nchar(c(Std_Error,"Std.Error")))
                                      for(i in 1:length(Std_Error)){
                                        if(nchar(Std_Error[i])<n){
                                          lagga_till<-n-nchar(Std_Error[i])
                                          plus<-paste(rep(" ",lagga_till),collapse = "")
                                          Std_Error[i]<-paste(plus,Std_Error[i],sep = "")
                                        }
                                      }
                                      Std_Error<-c("Std.Error",Std_Error)
                                      
                                      
                                      t_value<-as.character(round(tBetas,3))
                                      n<-max(nchar(c(t_value,"t value")))
                                      for(i in 1:length(t_value)){
                                        if(nchar(t_value[i])<n){
                                          lagga_till<-n-nchar(t_value[i])
                                          plus<-paste(rep(" ",lagga_till),collapse = "")
                                          t_value[i]<-paste(plus,t_value[i],sep = "")
                                        }
                                      }
                                      t_value<-c("t value",t_value)
                                      
                                      
                                      Pr_t<-as.character(round(Pvalues,4))
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
                                      
                                      
                                      
                                      
                                      
                                      
                                      
                                      cat("Call:", "\n")
                                      cat(paste("linreg("," formula=",Input[2],", "," data=",Input[3],")",sep=""), sep="\n")
                                      cat(" ",sep="\n")
                                      cat("Coefficients:",sep="\n")
                                      cat(paste(namn,Estimate,Std_Error,t_value,Pr_t),sep="\n")
                                      cat(" ",sep="\n")
                                      cat( paste("Residual standard error (\u03C3):",round(sqrt(Var_residuals),5)),sep="\n")
                                      cat( paste("df:",df),sep="\n")
                                      
                                      
                                      
                                    },
                                    
                                    resid = function(){
                                      result<-as.numeric(Residuals)
                                      names(result)<-1:length(result)
                                      return(result)
                                    },
                                    
                                    coef = function(){
                                      result<-Coefficients
                                      return(result)
                                      
                                    },
                                    
                                    plot = function() {
                                      dataint <- data.frame(residual = Residuals, fits = Fits, std_residual = sqrt(abs(scale(Residuals))))
                                     
                                      
                                      #Residuals vs Fitted
                                      pl_1 <- ggplot(data = dataint, aes(x = fits, y = residual) ) +
                                        geom_point() + 
                                        labs(x = "Fitted values", y = "Residuals") +
                                        geom_smooth(method="loess", se = FALSE, color = "red") + 
                                        geom_hline(yintercept = 0) + theme_bw() + ggtitle("Residuals vs Fitted") +
                                        theme(plot.title = element_text(hjust = 0.5))
                                      
                                      #Standardized residuals vs Fitted
                                      pl_2 <- ggplot(data = dataint, aes(x = fits, y = std_residual) ) +
                                        geom_point() + labs(x = "Fitted values", y = "Standardized residuals") +
                                        geom_smooth(method="loess", se = FALSE, color = "red") + 
                                        geom_hline(yintercept = 0) + 
                                        theme_bw() + 
                                        ggtitle("Standardized residuals vs Fitted") +
                                        theme(plot.title = element_text(hjust = 0.5))
                                      
                                      list(pl_1,pl_2)
                                      
                                      # cat ("Press [enter] to continue")
                                      # line <- readline()
                                      # pl_1
                                      # Sys.sleep(0.01)
                                      # cat ("Press [enter] to continue")
                                      # line <- readline()
                                      # pl_2
                                    }
                                    
                                  ))
