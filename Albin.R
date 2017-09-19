library(ggplot2)

##### Prova funktionerna #####
linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$print()
linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$pred()
linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$summary()
linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$resid()
linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$coef()
linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$plot()


######## Extra
formula<- Petal.Length ~ Sepal.Width + Sepal.Length
data <- iris
Call<-character()
Call[1]<-"iris"
Call[2]<- "Petal.Length ~ Sepal.Width + Sepal.Length"
######## 
library(Lab4ofDOOM)
?Lab4ofDOOM

