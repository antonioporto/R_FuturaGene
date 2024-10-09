# Treinamento Jacareí
#Crédito (Antônio Carlos Porto - Anselmo A Santos)



################################################################################
########################## Pacotes #############################################
################################################################################

#### Instalando pacotes 

rm(list=ls()) 

#install.packages("tidyverse") 
library("tidyverse") 

#install.packages("readxl") 
library("readxl") 

#install.packages("openxlsx")
library("openxlsx")

#install.packages("fBasics")
library("fBasics")

#install.packages("ggplot2")
library("ggplot2")

#install.packages("car")
library("car")

#install.packages("ExpDes")
library("ExpDes")

#install.packages("agricolae")
library("agricolae")

#install.packages("asbio")
library("asbio")

#install.packages("latticeExtra")
library("latticeExtra")

#install.packages("readxl") 
library("readxl") 

#install.packages("openxlsx")
library("openxlsx")


######################################################################################
####################### R como calculadora/ R as a Calculator ########################
######################################################################################

#sum
4+4

#subtraction
4-3

#multiplication
4*2

#division
4/2

#Exponencial
4^2
4**2

#Root

16^(1/2)
sqrt(16)   #square root
16^(1/3)   # Cube root 
16**(1/4)


# Constante
# constant number
a = 5
b <- 5



# Constante pré-definida
# predefined constant 

Pi <- pi
Pi

pi

#######################################################################################
########################### Vetores/ Vectors #########################################
#######################################################################################

# Concatenar elementos ou sub-vetores
# Generate vectors
vetor=c(1,2,3,4) 
vetor2 = c(1:200)

# repetir elementos
# repeat
rep(1,200) 

# Gerar sequências repetidas
# Generate repeat sequences 
a <- seq(2,100000000)
a
#Vetor numérico
# Numeric Vector

c<-c(1,2,3,4,5,6)   

# Vetor alfanumérico
# String
d<-c("vq","b2","c3","d","e","f")  


# Vector calculate
sqrt(c)


#Matriz 
f<-matrix(c(1,2,3,4,5,6))
h<-matrix(c(1,2,3,4,5,6),nrow=2, ncol=3)   #nrow = linha   ncol= coluna
j<-matrix(c(1,2,3,4,5,6), 2,3)
l<-matrix(c("a","b","c","d","e","f"),2,3)


######################################################################################
####################### Algebra de matrizes/ Matrix Algebra ##########################
######################################################################################

u <- matrix(c(1,2,3,4,5,6,7,8,9),3,3)
v <- matrix(c(2,3,1,2,4,1,1,1,1),3,3)

# sum 
u+v

#subtration
u-v

# multiplication
u*v

# matrix multiplication
u%*%v

# division
u/v


# dataframe create
am <- data.frame(c,d)
ao <- cbind(c,d)


ao2 <- data.frame(ao) 
str(ao2)
mean(ao2$c)


str(am)
mean(am$c)

####################################Manipulação de Dados#####################################################


setwd("C:/Users/anselmos/Desktop/CursoR/Jacareí")

Dados <- read_excel("DadosCopies.xlsx")  


Dados # Vizualiza o conteúdo da varaável dados


dim(Dados) # ver dimensões (Número de Linhas e Colunas da Tabela)
nrow(Dados) # número de linhas
ncol(Dados) # número de colunas
names(Dados) # Informa o nome das colunas
str(Dados)  # Informa os tipos de dados contidos na Tabela


Dados$Event <- as.factor(Dados$Event) # Transforma coluna Year em fator
Dados$Clone <- as.factor(Dados$Clone) # Transforma coluna Clone em fator
Dados$Construct <- as.factor(Dados$Construct) # Transforma coluna construct em fator
Dados$Year <- as.factor(Dados$Year) # Transforma coluna Year em fator
Dados$Report <- as.factor(Dados$Report) # Transforma coluna Report em fator


summary(Dados) 


#filter() - filtra linhas

AC224 <- filter(Dados, Dados$Clone == "AC224")

Goldens <- filter(Dados, Dados$CP4 <= 2 & Dados$NPTII <= 2)

Diferente <- filter(Dados, Dados$Clone != "SP1048")



#select() - seleciona colunas

StartC <- select(Dados, starts_with('C'))

Genes <- select(Dados, CP4, NPTII)

Inicio <- select(Dados, Event:Year)

#mutate() - cria/modifica colunas

Soma <- mutate(Dados, Soma = CP4 + NPTII)

Multi <- mutate(Dados, Multi = CP4 * NPTII)

#arrange() - ordena a base

Crescente <- arrange(Dados, Year) # Organiza dados de ordem crescente por ano (Coluna Year)

Decrescente <- arrange(Dados, -Year) # Organiza dados de ordem decrescente por ano ((Coluna Year)


Resumo <- summary(Dados, maxsum = 30) 


write.xlsx(Resumo, file = "MinhaTabela.xlsx")

write.xlsx(AC224, file = "MinhaTabeladeAC224.xlsx")



ggplot(Dados, aes(Year)) +
  geom_bar(stat = "count", show.legend = TRUE, position="dodge", colour = "black") 

ggplot(Dados, aes(Year, fill = Clone)) +
  geom_bar(stat = "count", show.legend = TRUE, position="dodge", colour = "black") 
  
ggplot(Dados, aes(Year, fill = Construct)) +
  geom_bar(stat = "count", show.legend = TRUE, position="stack", colour = "black") +
  theme_classic() + 
  labs(x = "Anos", y = "Amostras")


ggplot(Dados, aes(Clone, fill = Construct)) +
  geom_bar(stat = "count", show.legend = TRUE, position="stack", colour = "black") +
  theme_classic() + 
  labs(x = "Anos", y = "Amostras")



ggplot(Dados, aes(Clone, fill = Construct)) +
  geom_bar(stat = "count", show.legend = TRUE, position="stack", colour = "black") +
  theme_classic() + 
  labs(x = "Anos", y = "Amostras") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(Dados, aes(Clone, fill = Construct)) +
  geom_bar(stat = "count", show.legend = TRUE, position="stack", colour = "black") +
  theme_classic() + 
  labs(x = "Anos", y = "Amostras") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks=seq(0, 2000, 100), limits = c(0, 2000)) +
  labs(title = "Live 3", subtitle = "Meu gráfico") + 
  annotate("text", x = 3, y = 1500, label = "Top", size = 3.5, angle = 45)


ggplot(Dados, aes(CP4, NPTII, col = Construct )) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = lm, alpha = 0.5, se = FALSE) +
  theme_classic() + 
  theme(legend.title=element_blank()) +
  labs(x = "CN CP4", y = "CN NPTII") +
  labs(title = "Análise de Copy Number", subtitle = "Primeiro Dia") +
  facet_wrap( ~ Construct)



ggplot(Dados, aes(Construct, NPTII, col = Construct )) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Análise de Copy Number", subtitle = "Primeiro Dia") +
  labs(x = "Construções", y = "CN NPTII")

ggplot(Dados, aes(Construct, CP4, col = Construct )) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Análise de Copy Number", subtitle = "Primeiro Dia") +
  labs(x = "Construções", y = "CN CP4") +
  theme(legend.title=element_blank())

ggplot(Dados, aes(Construct, CP4, col = Construct )) +
  geom_boxplot() +
  labs(title = "Análise de Copy Number", subtitle = "Primeiro Dia") +
  labs(x = "Construções", y = "CN CP4") +
  theme(legend.title=element_blank()) +
  theme_classic()



#######################################################################################
########################## Manipulação de dados #######################################
#######################################################################################

# Remover todos os objetos
rm(list = ls(all=T)) 

# Configurar o terminal (diretório)
setwd("C:/Users/anselmos/Desktop/CursoR/Jacareí")

# Lendo um arquivo dentro do diretóio
data <- read.csv("Iris.csv", header=TRUE, na.string = 'NA')
data <- read.table("Iris.txt", header=TRUE, na.string = 'NA')

data <- iris


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
# Antonio Carlos Porto, 12.09.24
# Aula 2


##### Setting the directory #########################################################
setwd("D:\\Desktop\\Curso R\\3? Aula")

# Reading the file inside the directory
dados <- read.csv("Iris.csv", header=TRUE, na.string = 'NA')

dados <- iris

########################################################################################
############################## ANOVA ###################################################
########################################################################################


##### model
Petal <- aov(Petal.Length ~ Species, data=dados)
Anava <- anova(Petal)

#### evaluating the assumptions
names(Petal)
residuo <- Petal$residuals

#### Normality of residuals

# Histogram
hist(residuo)
plot(density(residuo))
plot(density(dados$Petal.Length))  #  because don't can used observation data

library(fBasics)
histPlot(as.timeSeries(residuo))


# qqplot
qqnorm(residuo)

qqnorm(dados$Petal.Length) #  because don't can used observation data

library(fBasics)
qqnormPlot(residuo)

# Shapiro-Wilk test
shapiro.test(residuo)

#### homogeneity of variances 




# Bartlett Test
bartlett.test(Petal.Length ~ Species, data=dados)

# Boxplot
boxplot(Petal.Length ~ Species, dados)

library(lattice)
bwplot(Petal.Length ~ Species, dados)



##################### Bar Graphs with means ###########################################
library(ggplot2)
library(dplyr)


##### Some calculations required

#Erro <- function(x)  sd(x)/sqrt(length(x))
dados2 <- dados %>%
  group_by(Species) %>%
  select(Petal.Length) %>%
  summarise(Petal_Lenght = mean(Petal.Length),
            Error = sd(Petal.Length)/sqrt(50))


g <- ggplot(dados2, 
            aes(x = Species , y = Petal_Lenght, fill= Species)) + geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=Petal_Lenght - Error, ymax=Petal_Lenght + Error), width=.2,
                position=position_dodge(.9)) 

g  



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

