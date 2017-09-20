library(ggplot2)
library(Lab4ofDOOM)

##### Prova funktionerna #####
linreg_v2$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
linreg_v2$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$print()
linreg_v2$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$pred()
linreg_v2$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$summary()
linreg_v2$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$resid()
linreg_v2$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$coef()
linreg_v2$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$plot()
######## Extra ########
formula<- Petal.Length ~ Sepal.Width + Sepal.Length
data <- iris
Call<-character()
Call[1]<-"iris"
Call[2]<- "Petal.Length ~ Sepal.Width + Sepal.Length"
######## 
library(testthat)

expect_output(cat("linreg(formula = Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)"),
              "linreg\\(formula \\= Petal.Length \\~ Sepal.Width \\+ Sepal.Length, data \\= iris\\)")  

 
expect_output(str("(hej~~ =+)"),"(hej~~ =+)")  

?regex

Jag
