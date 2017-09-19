library(ggplot2)
library(Lab4ofDOOM)

##### Prova funktionerna #####
linreg_v2$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$print()
linreg_v2$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$pred()
linreg_v2$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$summary()
linreg_v2$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$resid()
linreg_v2$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$coef()
linreg_v2$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)$plot()



linreg_v2$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

example(linreg_v2)


Lab4ofDOOM::linreg_v2
stats::


######## Extra
formula<- Petal.Length ~ Sepal.Width + Sepal.Length
data <- iris
Call<-character()
Call[1]<-"iris"
Call[2]<- "Petal.Length ~ Sepal.Width + Sepal.Length"
######## 
library(Lab4ofDOOM)
?Lab4ofDOOM

