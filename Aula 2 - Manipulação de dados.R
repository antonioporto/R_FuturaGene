# Antonio Carlos Porto, 12.09.24
# Aula 2

#######################################################################################
########################## Manipulação de dados #######################################
#######################################################################################

# Remover todos os objetos
rm(list = ls(all=T)) 

# Configurar o terminal (diretório)
setwd("D:/Desktop/Curso R/Curso R/2? aula")

# Lendo um arquivo dentro do diretóio
data <- read.csv("Iris.csv", header=TRUE, na.string = 'NA')
data <- read.table("Iris.txt", header=TRUE, na.string = 'NA')




##################### Funções de mineração e reconhecimento ##########################

### Reconhecimento dos elementos do data frame

summary(data)
head(data)
str(data) 
length(data)
table(data)
list(data)

### Localizaçaoo e seleção

data[,1]
data[2,1]
data[,1] < 5
data[,1] == 5
data[,1] != 5
data[data$Sepal.Length > 5,] 
data[data$Sepal.Length > 5 & data$Sepal.Width > 3.5, ] 

### Estatistica descritiva

mean(data$Sepal.Length)          # média
median(data$Sepal.Length)        # mediana
var(data$Sepal.Length)           # variância
sd(data$Sepal.Length)            # desvio padr?o
length(data$Sepal.Length)        # numero de elementos
range(data$Sepal.Length)         # menores e maiores valores
unique(data$Sepal.Length)        # remove elementos duplicados
diff(data$Sepal.Length)          # (m?dia - observa??o)
rev(data$Sepal.Length)           # elementos reversos
summary(data$Sepal.Length)       # estatistica descritiva dos dados

summary(data)




#######################################################################################
########################## Exploração Gráfica #########################################
#######################################################################################

## gráfico Básico
plot(data)

### Ligação de reconhecimento dos vetores dentro do dataframe (tabela)
plot(data$Sepal.Length , data$Sepal.Width)

### Exemplo de um gráfico de caixas (boxplot)
boxplot(data$Petal.Length ~ data$Species,
        col= c("green","orange","black"),
        ylab = "Petal.Length",
        main = "Petal.Length Boxplot")



### Alguns exemplos de gráficos do ggplot2

library(ggplot2)


#### Gráfico de dispersão
ggplot(data=data,
      aes(y=Sepal.Length,x=Petal.Length))+geom_point()+geom_smooth(method="lm")

#### Separando por espécie
ggplot(data=data,
      aes(y=Sepal.Length, x=Petal.Length, colour = Species))+geom_point()

#### Separando por espécie
ggplot(data=data,
       aes(y=Sepal.Length, x=Petal.Length))+geom_point() + facet_grid(~Species)

##### Gráfico de caixas (boxplot)
ggplot(data, 
      aes(x = Species, y = Petal.Length, fill = Species)) + geom_boxplot()


