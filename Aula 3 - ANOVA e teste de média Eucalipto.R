# Antonio Carlos Porto, 19.09.24
# Aula 3


# Pacotes a serem utilizados
library(tidyverse)
library(readxl)
library(agricolae)
library(ExpDes)
library(ScottKnott)

##### Configurando o diretório #########################################################
setwd("D:\\Desktop\\Curso R\\3? Aula")

# Lendo arquivo de dados dentro do diretório
dados <- read_excel("Aula 3 - dados_campo_eucalipto.xlsx", na = c("NA","0")) 


# Verificando os dados
str(dados) # clone e bloco devem ser fatores

dados$Clone <- factor(dados$Clone)
dados$Bloco <- factor(dados$Bloco)

# histograma diâmetro
dados %>% 
  ggplot(aes(x = CAP)) +
  geom_histogram()

# histograma altura
dados %>% 
  ggplot(aes(x = Altura)) +
  geom_histogram()

# verificando relação entre variáveis
dados %>% 
ggplot(aes(x = CAP, y = Altura)) +
  geom_point() +
  labs(y = "altura (m)", x = "diâmetro (cm)") +
  facet_wrap(~Bloco)


# calculando volume cilindrico das arvores e extrapolando para ha 
# chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.ipef.br/publicacoes/scientia/nr41-42/cap08.pdf
dados$volume = (((dados$CAP/pi)**2 * dados$Altura)/ (4000 * pi)) * 0.5
dados$volume = (dados$volume * (10000/9))
dados$volume = (dados$volume/2)


# verificando dados de volume

# histograma altura
dados %>% 
  ggplot(aes(x = volume)) +
  geom_histogram() +
  labs(x = "volume (m³/ha/ano)")



########################################################################################
############################## ANOVA ###################################################
########################################################################################


##### model
euc <- aov(volume ~ Bloco + Clone, data=dados)
anava <- anova(euc)

#### avaliando pressupostos
residuo <- euc$residuals

hist(residuo) # histograma
shapiro.test(residuo) # teste de normalidade
boxplot(volume ~ Clone, dados) # homocedasticidade



########################################################################################
#################### Teste de médias ###################################################
########################################################################################

# teste tukey (algoritmo com alta taxa de ambiguidade)
 
ttukey <- HSD.test(euc,"Clone", alpha = 0.05, group=TRUE,console=TRUE)

# teste scott-knott (agrupamento de média, sem ambiguidade)

tsKnott <- SK(x = euc, which = "Clone")
table_sk <- tsKnott$out[["Result"]]
table_sk$Clone <- row.names(table_sk)
table_sk <- 
table_sk %>%
  unite("group", 2:8, remove = FALSE, sep = "") %>%
  mutate(Means = as.numeric(Means),
         group = factor(group))

str(table_sk)



########################################################################################
#################### Análise gráfica ###################################################
########################################################################################


plot_euc <-
ggplot(table_sk, 
       aes(y = Means, x = reorder(Clone, desc(Means)))) + 
       geom_bar(stat="identity", color = "black") +
       theme_bw() +
       theme(axis.text.x = element_text(color = "black", size=10, angle=90, vjust=1, hjust=1)) +
       labs(y = "volume(m³/ha/ano)", x = "") +
       geom_text(aes(label = group), vjust = -0.5)  

plot_euc


# salvando gráfico em alta resolução 
jpeg("Grafico_euc.jpeg", width = 7, height = 4, units = 'in', res = 400)
plot_euc
dev.off()


# exportando análise do teste de médias (scott-knott)
write.csv2(table_sk, "resultados_euc.csv")


      
      


     
