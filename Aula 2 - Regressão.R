# Antonio Carlos Porto, 12.09.24
# Aula 2


##### Setting the directory #########################################################
setwd("D:\\Desktop\\Curso R\\3? Aula")

# Reading the file inside the directory
dados <- read.csv("Iris.csv", header=TRUE, na.string = 'NA')

########################################################################################
######################### linear regression #############################################
########################################################################################

####  The "lm" function uses the ordinary least squares method
modelo<-lm(Sepal.Length ~ Petal.Length, data= dados)

summary(modelo)
names(modelo)

# b1 (a) = interceptação da reta com o eixo vertical/  the Y-intercept
# b2 (b) = representa o declive (coeficiente angular) da reta / angular coefficient of the model;


#### Residuals regression

residuo<-modelo$residuals
shapiro.test(residuo)

#### Graphs

## Using default Graph
plot(Sepal.Length ~ Petal.Length, data=dados,
     pch = 3,
     col = "red",
     family="serif")
abline(modelo)


# inserting adjusted model and r?
coeficientes <- modelo$coefficients
texto <- sprintf('y = %.2f + %.2fx, r² = %.2f', coeficientes[1], coeficientes[2], summary(modelo)$r.squared)
text(2.5,7.5, texto, family="serif" )



## ggplot2 graph
library(ggplot2)

equation = function(x) {
  lm_coef <- list(a = round(coef(x)[1], digits = 3),
                  b = round(coef(x)[2], digits = 3),
                  r2 = round(summary(x)$r.squared, digits = 3));
  lm_eq <- substitute(italic(y) == a ~ b %.% italic(x)*","~~italic(R)^2~adj ~"="~ r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}

tiff("regress?o.tiff", width = 6.5, height = 3.5, units = 'in', res = 600)


p <- ggplot(data = dados, aes(x = Petal.Length, y = Sepal.Length)) +
  geom_smooth(method = "lm", se=T, color="black", formula = y ~ x) +
  geom_point() +
  annotate("text", x = 5, y = 3.5, label = equation(modelo), parse = TRUE) + 
  xlab("Sepal. Length") + ylab("Petal.Length") + theme_bw() 

p


dev.off()

