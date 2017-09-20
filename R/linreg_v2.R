#'@title Do you want to make a linjer regression?
#'@description This function make a "Reference Class" with class=linreg for a linjer regression
#'
#'
#'
#' @field formula The formula for the model
#' @field data A object of class data.frame
#' 
#'@examples
#' data(iris)
#' #linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$print()
#' #linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$pred()
#' #linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$summary()
#' #linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$resid()
#' #linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$coef()
#' #linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$plot()
#' @export linreg
#' @export 
#' 
#' 
linreg<-setRefClass("linreg", fields = list(formula="formula", 
                                            data="data.frame",
                                            Call="character",
                                            Coefficients="numeric",
                                            Fits="numeric",
                                            Residuals="numeric",
                                            df="numeric",
                                            Var_residuals="numeric",
                                            Std_betas="numeric",
                                            tBetas="numeric",
                                            Pvalues="numeric"),
                    methods = list(
                      
                      initialize = function(formula,data){
                        
                        Call[1] <<-deparse(substitute(data))
                        Call[2] <<-Reduce(paste,deparse(formula))
                        
                        formula<<-formula
                        data<<-data
                        
                        ########### Beräkningar ########### 
                        x<-model.matrix(formula,data)
                        y<-all.vars(formula)[1]
                        y<-as.matrix(data[,names(data)==y])
                        
                        b_hat<-solve(t(x)%*%x)%*%t(x)%*%y
                        y_fits<-x%*%b_hat
                        e<-y-y_fits
                        df1<-length(y)-ncol(x)
                        var_e<-(t(e)%*%e)/df1
                        
                        var_b_hat<-as.numeric(var_e)*diag(solve((t(x)%*%x)))
                        std_b_hat<-sqrt(var_b_hat)
                        t_b_hat<-as.numeric(b_hat)/std_b_hat
                        p_b_hat<-(1-(pt(abs(t_b_hat),df = df1)))*2
                        
                        b_hat_numeric<-as.numeric(b_hat)
                        names(b_hat_numeric)<-rownames(b_hat)
                        
                        
                        
                        input_var<-as.character(match.call(expand.dots = FALSE))
                        ########### Spara beräkningar ###########
                        Coefficients<<-b_hat_numeric
                        Fits<<-as.numeric(y_fits)
                        Residuals<<-as.numeric(e)
                        df<<-df1
                        Var_residuals<<-as.numeric(var_e)
                        Std_betas<<-std_b_hat
                        tBetas<<-t_b_hat
                        Pvalues<<-p_b_hat
                        ###########
                        
                        
                        
                        
                      },
                      
                      pred = function(){
                        "Give you the predicted values as a numeric vector"
                        
                        svar<-Fits
                        names(svar)<-1:length(svar)
                        return(svar)
                      },
                      
                      print = function() {
                        "Give you a nice view of the calculation"
                        
                        
                        Dataset<-Call[1]
                        Formula<-Call[2]
                        Formula<-paste("linreg(formula = ",Formula,", data = ",Dataset,")",sep="")
                        beta<-Coefficients
                        namn<-names(beta)
                        names(beta)<-NULL
                        beta<-round(beta,4)
                        
                        
                        for(i in 1:length(beta)){
                          beta[i]<-format(beta[i], width=max(nchar(beta[i]),nchar(namn[i])),justify = c("right"))
                          namn[i]<-format(namn[i], width=max(nchar(beta[i]),nchar(namn[i])),justify = c("right"))
                        }
                       
                        cat("Call:",sep="\n")
                        cat(Formula, sep="\n")
                        cat(sep="\n")
                        cat("Coefficients:",sep="\n")
                        cat(namn,sep="\t")
                        cat("","\n")
                        cat(beta,sep="\t")
                        cat("",sep="\n")
                        cat("",sep="\n")
                        
                        
                        
                        
                        
                      },
                      
                      summary = function(){
                        "Give you a nice summary of the calculation"
                        
                        
                        
                        Variable<-as.character(names(Coefficients))
                        Estimate<-round(Coefficients,3)
                        Std_Error<-round(Std_betas,3)
                        t_value<-round(tBetas,3)
                        Pr_t<-round(Pvalues,5)
                        
                        svar<-data.frame(Variable,Estimate,Std_Error,t_value,Pr_t)
                        row.names(svar)<-NULL
                        svar$Variable<-as.character(svar$Variable)
                        
                        min_lista<-list()
                        min_lista[[1]]<-paste(c("Variable","Estimate","Std_Error","t_value","Pr_t"),collapse=" ")
                        for(i in 2:(nrow(svar)+1)){
                          min_lista[[i]]<-paste(svar[(i-1),], collapse=" ")
                        }
                        svar2<-min_lista
                        
                        Residual_skrivaut<-paste("Residual standard error: ",round(sqrt(Var_residuals),5) ," on " ,df, " degrees of freedom",sep="")
                        svar<-list(svar,Residual_skrivaut)
                        svar2[[(length(svar2)+1)]]<-Residual_skrivaut
                        
                        
                        return(svar)
                        
                      },
                      
                      resid = function(){
                        "Give you the residuals as a numeric vector"
                        
                        svar<-Residuals
                        names(svar)<-1:length(svar)
                        return(svar)
                      },
                      
                      coef = function(){
                        "Give you the coef values as a numeric vector"
                        svar<-Coefficients
                        return(svar)
                        
                      },
                      
                      plot = function() {
                        
                        "Printing out two good graph!"
                        dataint <- data.frame(residual = Residuals, fits = Fits, std_residual = sqrt(abs(scale(Residuals))))
                        
                        require(ggplot2)
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
                    )
)
