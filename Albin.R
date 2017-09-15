####  linreg ####
linreg<-function(formula,data){
  
  
  x<-model.matrix(formula,data)
  y<-all.vars(formula)[1]
  y<-as.matrix(data[,names(data)==y])


  b_hat<-solve(t(x)%*%x)%*%t(x)%*%y
  
  y_fits<-x%*%b_hat
  
  e<-y-y_fits
  
  df<-length(y)-(ncol(x)+1)
  
  var_e<-(t(e)%*%e)/df
  
  
 var_b_hat<-as.numeric(var_e)*diag(solve((t(x)%*%x)))
 std_b_hat<-sqrt(var_b_hat)
 
 t_b_hat<-as.numeric(b_hat)/std_b_hat
  
 p_b_hat<-(1-(pt(abs(t_b_hat),df = df)))*2
 
 
 b_hat_numeric<-as.numeric(b_hat)
 names(b_hat_numeric)<-rownames(b_hat)
 
 
 
 input_var<-as.character(match.call(expand.dots = FALSE))
 
 result <- list(Call = formula, Coefficients = b_hat_numeric, Xterms=x, Yterms=y, Fitts=y_fits,
                  Resid=e, df=df,
                  Var_residuals=var_e, Std_betas=std_b_hat,
                  tBetas=t_b_hat, Pvalues = p_b_hat,Input=input_var)
 
 class(result)<-"linreg"
 
 
 
 print.linreg <<- function(obj) {
   
   beta<-as.character(round(obj$Coefficients,2))
   namn<-names(obj$Coefficients)
   
   n<-max(c(nchar(beta),20))
   if(TRUE%in%(nchar(namn)>n)){
     namn[nchar(namn)>n]<-
       paste(substr(namn[nchar(namn)>n],1,(n-3)),"...",sep="")
   }
   
   for(i in 1:length(obj$Coefficients)){
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
   cat(paste("linreg("," formula=",obj$Input[2],", "," data=",obj$Input[3],")",sep=""), sep="\n")
   cat(sep="\n")
   cat("Coefficients:",sep="\n")
   cat(namn,sep="\t")
   cat("","\n")
   cat(beta,sep="\t")
   cat("",sep="\n")
   cat("",sep="\n")

 }
 
 summary.linreg <<- function(obj){
   
   namn<-names(obj$Coefficients)
   
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
   
   
   
   Estimate<-as.character(round(obj$Coefficients,3))
   n<-max(nchar(Estimate),nchar("Estimate"))
   for(i in 1:length(Estimate)){
     if(nchar(Estimate[i])<n){
       lagga_till<-n-nchar(Estimate[i])
       plus<-paste(rep(" ",lagga_till),collapse = "")
       Estimate[i]<-paste(plus,Estimate[i],sep = "")
     }
   }
   Estimate<-c("Estimate",Estimate)
   
   
   Std_Error<-as.character(round(obj$Std_betas,3))
   n<-max(nchar(c(Std_Error,"Std.Error")))
   for(i in 1:length(Std_Error)){
     if(nchar(Std_Error[i])<n){
       lagga_till<-n-nchar(Std_Error[i])
       plus<-paste(rep(" ",lagga_till),collapse = "")
       Std_Error[i]<-paste(plus,Std_Error[i],sep = "")
     }
   }
   Std_Error<-c("Std.Error",Std_Error)
   
   
   t_value<-as.character(round(obj$tBetas,3))
   n<-max(nchar(c(t_value,"t value")))
   for(i in 1:length(t_value)){
     if(nchar(t_value[i])<n){
       lagga_till<-n-nchar(t_value[i])
       plus<-paste(rep(" ",lagga_till),collapse = "")
       t_value[i]<-paste(plus,t_value[i],sep = "")
     }
   }
   t_value<-c("t value",t_value)
   
   
   Pr_t<-as.character(round(obj$Pvalues,4))
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
   cat(paste("linreg("," formula=",obj$Input[2],", "," data=",obj$Input[3],")",sep=""), sep="\n")
   cat(" ",sep="\n")
   cat("Coefficients:",sep="\n")
   cat(paste(namn,Estimate,Std_Error,t_value,Pr_t),sep="\n")
   cat(" ",sep="\n")
   cat( paste("Residual standard error (\u03C3):",round(obj$Var_residuals,5)),sep="\n")
   cat( paste("df:",obj$df),sep="\n")
   
   
   
 }
 
 resid.linreg <<- function(obj){
   return(obj$Resid)
 }
 
 ###########
 # reglin <- setRefClass("linreg",fields = c("Call", "Coefficients", "Xterms", "Yterms", "Fitts", 
 #                                           "Resid", "df", "Var_residuals", 
 #                                           "Var_betas", "tBetas", "Pvalues") )
 # 
 # result <- reglin(Call = formula, Coefficients = b_hat, Xterms=x, Yterms=y, Fitts=y_fits, 
 #                  Resid=e, df=df, 
 #                  Var_residuals=var_e, Var_betas=var_b_hat, 
 #                  tBetas=t_b_hat, Pvalues = p_b_hat)
 #########
 
 return(result)

}
############
#Formula + data
formula<-Petal.Length~Species
data(iris)
data<-iris
summary(lm(formula,data)) #lm
linreg(formula, data = data) #linreg

###
# lm
a<-lm(Petal.Length~Species, data = iris)
a
summary(a)
######
# linreg
obj<-linreg(Petal.Length~Species, data = iris)
obj
summary(obj)
resid(obj)
##############

print(obj)

print.linreg(obj)

resid(obj)
resid.linreg(obj)

class(obj)

methods(resid)

#






