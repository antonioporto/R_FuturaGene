rm(list=ls()) #Isso é um comando para limpar o R

#install.packages("tidyverse") # Isso é um comando para instalar um pacote
library("tidyverse") # Isso é um comando para abrir um pacote

#install.packages("readxl") #Instala pacotes necessários para a rodar o script
library("readxl") #Carrega pacotes necessários para a rodar o script

#install.packages("openxlsx")
library(openxlsx)

setwd("C:/Users/anselmos/Desktop/CursoR/Jacareí")

Dados <- read_excel("DadosCopies.xlsx")  

Dados$Event <- as.factor(Dados$Event) # Transforma coluna Year em fator
Dados$Clone <- as.factor(Dados$Clone) # Transforma coluna Clone em fator
Dados$Construct <- as.factor(Dados$Construct) # Transforma coluna construct em fator
Dados$Year <- as.factor(Dados$Year) # Transforma coluna Year em fator
Dados$Report <- as.factor(Dados$Report) # Transforma coluna Report em fator


Dados # Vizualiza o conteúdo da varaável dados

dim(Dados)



5 < 3

1 == 1

7 >= 2

z <- "Cachorro"

z != "Gato"

Dados$Event[10]

w <- 4

Dados$Event[w]

##################Informa se a amostra tem Muitas ou poucas cópias####################
i <- 1

Dados$Quant <- NA

while(i <= 2259) {
  
  if(Dados$CP4[i] < 3){
    Dados$Quant[i] <- "Poucas"
  } 
  else if(Dados$CP4[i] >= 3 & Dados$CP4[i] <= 4){
    Dados$Quant[i] <- "Media"
  }
  
  else {
    Dados$Quant[i] <- "Muitas"
  }
  
  i <- i + 1
  
}


#######################Cria uma coluna informando se o Clone está em transformação ou não################

AtWork <- c("AC144","AC224","BA869")

Dados$Trabalho <- NA


j <- 1

while(j <= 2259) {
  
  if(Dados$Clone[j] %in% AtWork){
    Dados$Trabalho[j] <- "Sim"
  } else {
    Dados$Trabalho[j] <- "Não"
  }
  
  j <- j + 1
  
}


################# Cria uma coluna informando se é Golden ou não####################

Golden <- c("751","955")

Dados$Goldens <- NA


for(k in 1:length(Dados$Construct)) {
  
  if(Dados$Construct[k] %in% Golden){
    Dados$Goldens[k] <- "Sim"
  } else {
    Dados$Goldens[k] <- "Não"
  }
  
}





########## Cria Relatórios para cada um dos clones############################# 


ListaClone <- c("AC0144", "AC144", "AC224", "BA1175", "BA1922", "BA2004", "BA869", "SP1048", "SP530", "UR08")


for(l in 1:length(ListaClone)){
  
  NomeRel <- paste(ListaClone[l],".xlsx", sep = "")
  
  
  
  Filtrado <- filter(Dados, Dados$Clone == ListaClone[l])
  
  Resumo <- summary(Filtrado, maxsum = 30) 
  
  
  write.xlsx(Resumo, file =  NomeRel)
  
}

