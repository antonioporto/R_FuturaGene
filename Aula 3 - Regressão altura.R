# Antonio Carlos Porto, 19.09.24
# Aula 3


# Pacotes a serem utilizados
library(tidyverse)
library(readxl)


##### Configurando o diretório #########################################################

# Lendo o arquivo dentro do diretório
dados <- read_excel('Aula 3 - dados_altura.xlsx', na = c("NA","M","-"), skip = 2)


# Verificando os dados
str(dados)
dados <- transform(dados, filho = as.numeric(filho), mãe = as.numeric(mãe),
                   pai = as.numeric(pai))

str(dados) # estrutura dos dados

# convertendo de centimetros para metros (opção 1)
dados$filho <- dados$filho/100
dados$mãe <- dados$mãe/100
dados$pai <- dados$pai/100

# outra maneira usando pacote "dplyr" (opção 2)

#dados <-
#  dados %>%
#  mutate(filho = filho/100,
#         mãe = mãe/100,
#         pai = pai/100)
  


# cálculos e manipulação de dados

#média dos pais 
dados$mean_pais <- (dados$mãe + dados$pai)/2

#correlação entre filhos e média dos pais
cor(dados$filho, dados$mean_pais) 

#correlação entre filhos e pais
cor(dados$filho, dados$pai) 

#correlação entre filhos e mães
cor(dados$filho, dados$mãe) 


# histograma de altura de filhos
dados %>%
  ggplot(aes(x = filho)) +
  geom_histogram()

# separando por sexo
dados %>%
  ggplot(aes(x = filho, fill = sexo)) +
  geom_histogram() 

# outra alternativa de visualização por sexo
dados %>%
  ggplot(aes(x = filho)) +
  geom_histogram() +
  facet_grid(~sexo)

# capturando números máximos e minimos por sexo
dados_range <- 
dados %>%
  group_by(sexo) %>%
  summarise(range_sexo = range(filho))



########################################################################################
######################### Regressão linear #############################################
########################################################################################

# A função "lm" usa o método dos mínimos quadrados ordinários
modelo<-lm(filho ~ mean_pais, data= dados)

summary(modelo)
names(modelo)

# b0 (a) = interceptação da reta com o eixo vertical/  the Y-intercept
# b1 (b) = representa o declive (coeficiente angular) da reta / angular coefficient of the model;


# Residuos da regressão
residuo<-modelo$residuals
shapiro.test(residuo)



### Análises gráficas 

## Using gráfico defaut do R
plot(filho ~ mean_pais, data=dados,
     pch = 3,
     col = "red",
     family="serif")
abline(modelo)


# inserindo modelo ajustado e coeficiente de determinação do modelo (R²)
coeficientes <- modelo$coefficients
texto <- sprintf('y = %.2f + %.2fx, r² = %.2f', coeficientes[1], coeficientes[2], summary(modelo)$r.squared)
text(2.5,7.5, texto, family="serif" )


## Gráfico utilizando ggplot2
equation = function(x) {
  lm_coef <- list(a = round(coef(x)[1], digits = 3),
                  b = round(coef(x)[2], digits = 3),
                  r2 = round(summary(x)$r.squared, digits = 3));
  lm_eq <- substitute(italic(y) == a ~ b %.% italic(x)*","~~italic(R)^2~adj ~"="~ r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}



ggplot(data = dados, aes(x = filho, y = mean_pais)) +
  geom_smooth(method = "lm", se=T, color="black", formula = y ~ x) +
  geom_point() +
  annotate("text", x = 1.6, y = 1.8, label = equation(modelo), parse = TRUE) + 
  xlab("Sepal. Length") + ylab("Petal.Length") + theme_bw()


ggplot(data = dados, aes(x = filho, y = mean_pais, fill = sexo)) +
  geom_smooth(method = "lm", se=T, color="black", formula = y ~ x) +
  geom_point() +
  annotate("text", x = 1.6, y = 1.8, label = equation(modelo), parse = TRUE) + 
  xlab("Sepal. Length") + ylab("Petal.Length") + theme_bw()



### Obsservação interessante ########################

dados$diff <- dados$filho - dados$mean_pais


# gráfico da diferença entre filhos e média dos pais (m)
dados %>%
  ggplot(aes(x = diff)) +
  geom_histogram() +
  labs(x = "diferença entre filhos e média dos pais (m)")

# separando por sexo
dados %>%
  ggplot(aes(x = diff, fill = sexo)) +
  geom_histogram() +
  labs(x = "deferença entre filhos e média dos pais (m)")

