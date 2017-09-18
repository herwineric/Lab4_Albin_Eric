#'@title Dijkstra algorithm
#'
#'@description The algorithm takes a graph and an initial node and calculates the shortest path from the initial node to every other node in the graph. 
#'@param graph shall be a data.frame with three variables (v1, v2 and w) that contains the edges of the graph (from v1 to v2) with the weight of the edge (w).
#'@param init_node shall be a numeric scalar that exist in the graph  
#'@export
#'@return Return the shortest path to everyother node from the starting node as a vector. 
#'@examples
#'wiki_graph <-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#'
#'dijkstra(wiki_graph,1)
#'dijkstra(wiki_graph,3)
#' @references You can check how the algorithm work on it's page at 
#'  \href{https://en.wikipedia.org/wiki/List_of_Crayola_crayon_colors}{wikepedia.}


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

  pred <<- function(object, ...){
    UseMethod("predict")
  }
  
  reglin <- setRefClass("linreg", fields = list(Call = "formula", Coefficients = "numeric", 
                                               Pvalues ="numeric", S_t = "numeric", Tvalue = "numeric",
                                            Residual = "numeric", Fits = "numeric", plots =  "list"),
                        methods = list(  
                          #########################
                          print.linreg <<- function(result) {
                            
                            beta<-as.character(round(result$Coefficients,2))
                            namn<-rownames(betas)
                            
                            n<-max(c(nchar(beta),20))
                            if(TRUE%in%(nchar(namn)>n)){
                              namn[nchar(namn)>n]<-
                                paste(substr(namn[nchar(namn)>n],1,(n-3)),"...",sep="")
                            }
                            
                            for(i in 1:length(result$Coefficients)){
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
                            cat(paste(as.character(result$Call)[2],as.character(result$Call)[1],as.character(result$Call)[3]), sep="\n")
                            cat(sep="\n")
                            cat("Coefficients:",sep="\n")
                            cat(namn,sep="\t")
                            cat("","\n")
                            cat(beta,sep="\t")
                            },
                          
                          #########################
                          
                          plot = function() {
                            dataint <- data.frame(residual = result$Residual, fitos = result$Fits, std_residual = sqrt(abs(scale(result$Residual))))
                            #Residuals vs Fitted
                            plots <<- ggplot(data = dataint, aes(x = fitos, y = residual) ) +
                              geom_point() + labs(x = "Fitted values", y = "Residuals") +
                              geom_smooth(method="loess", se = FALSE, color = "red") +
                              geom_hline(yintercept = 0) + theme_bw() + ggtitle("Residuals vs Fitted") +
                              theme(plot.title = element_text(hjust = 0.5))

                            #Standardized residuals vs Fitted
                            plots <<- ggplot(data = dataint, aes(x = fitos, y = std_residual) ) +
                              geom_point() + labs(x = "Fitted values", y = "Standardized residuals") +
                              geom_smooth(method="loess", se = FALSE, color = "red") +
                              geom_hline(yintercept = 0) + theme_bw() + ggtitle("Standardized residuals vs Fitted") +
                              theme(plot.title = element_text(hjust = 0.5))

                            # cat ("Press [enter] to continue")
                            # line <- readline()
                            # pl_1
                            # Sys.sleep(0.01)
                            # cat ("Press [enter] to continue")
                            # line <- readline()
                            # pl_2
                        },
                        #########################
                        
                        summary.linreg <<- function(result){
                          cat("Call:", "\n")
                          cat(paste(result$Call), "\n")
                          cat("Coefficients:", sep ="\n")
                          cat(c("", "Estimate", "Std.Error", "t value", "Pr(>|t|)"), sep = "\t")
                          cat("",sep="\n")
                          for(i in 1:length(rownames(betas))){
                            cat(substr(rownames(betas)[i],1,4),"", round(result$Coefficients,5)[i],"", 
                                round(result$S_t[i],5),"" , round(result$Pvalues[i], 5), sep="\t")
                            cat(sep="\n")
                          }
                        },
                        ######################### INHERIT SHIT
                        resid.linreg <<- function(result){
                          residuaallls <- result$Residual
                          names(residuaallls) <- 1:length(residuaallls)
                          residuaallls
                        },
                        #########################
                        coef.linreg <<- function(result){
                          coeeeefff <- result$Coefficients
                          names(coeeeefff) <- rownames(betas)
                          coeeeefff
                        },
                        #########################
                        pred.predict.linreg <<- function(result){
                          fiiiiiits <- result$Fits
                          names(fiiiiiits) <- 1:length(fiiiiiits)
                          fiiiiiits
                        }
                        
                  )
            )
  
  
  
  
  result <- reglin(Call = formula, Coefficients = c(betas), Pvalues = c(p_values),
                   S_t = c(var_betas), Tvalue = c(t_betas), Residual = c(resid), Fits = c(fitts))
  

  
  return(result)
}

class(pl_1) == c("gg", "ggplot")

residuals(lm(Y ~ x1 + x2 + x3, data))$copy()




data <- data.frame(x1=rnorm(120,1,50),x2=rnorm(120,5,50), Y = rnorm(120, 7,35), x3=as.factor(rep(1:12)))

ls()
formula <- Y ~ x1 + x2 + x3

rm(list = ls()[ls() == "print.linreg"])
methods(print)

test <- linreg(Y ~ x1 + x2 + x3, data)

test$plot()

print(test)
plot(test)
summary(test)
pred(test)
pred(test)
test$field()



test$print
residuals(lm(Y ~ x1 + x2 + x3, data))

methods(print) <- "print.linreg"
# 
# print.linreg <<-function(){
#   paste("hej")
# }
# 
# inherits(predict, what = "pred")
# 
# pred(lm(Y ~ x1 + x2 + x3, data))
# 
# 
# pred <- function(object, ...){
#   UseMethod("predict")
# }
# 
# print(summary(lm(Y ~ x1 + x2 + x3, data)))
# 
# 
# 
# summary(lm(Y ~ x1 + x2 + x3, data))$residuals
# 
# ##Look in to this later!!! (It is a puch to next graph, thing)
# readkeygraph <- function(prompt)
# {
#   getGraphicsEvent(prompt = prompt, 
#                    onMouseDown = NULL, onMouseMove = NULL,
#                    onMouseUp = NULL, onKeybd = onKeybd,
#                    consolePrompt = "[click on graph then follow top prompt to continue]")
#   Sys.sleep(0.01)
#   return(keyPressed)
# }
# 
# onKeybd <- function(key)
# {
#   keyPressed <<- key
# }
# 
# xaxis=c(1:10) # Set up the x-axis.
# yaxis=runif(10,min=0,max=1) # Set up the y-axis.
# plot(xaxis,yaxis)
# 
# for (i in xaxis)
# {
#   # On each keypress, color the points on the graph in red, one by one.
#   points(i,yaxis[i],col="red", pch=19)
#   keyPressed = readkeygraph("[press any key to continue]")
# }
