rm(list=ls()) #Isso é um comando para limpar o R

#install.packages("tidyverse") # Isso é um comando para instalar um pacote
library("tidyverse") # Isso é um comando para abrir um pacote

#install.packages("readxl") #Instala pacotes necessários para a rodar o script
library("readxl") #Carrega pacotes necessários para a rodar o script

#install.packages("agricolae")
library("agricolae")

#install.packages("corrgram")
library("corrgram")

library("car")


setwd("C:/Users/anselmos/Desktop/CursoR/Jacareí")


Experimento <- read_excel("Experimento.xlsx")



mean(Experimento$Crescimento)


sd(Experimento$Crescimento)




T1 <- filter(Experimento, Tratamento == "T1")


mean(T1$Crescimento)
sd(T1$Crescimento)



mExperimento <- Experimento %>% group_by(Tratamento) %>% summarise(Media = mean(Crescimento), Desvio = sd(Crescimento))
mExperimento




ggplot(mExperimento, aes(Tratamento, Media, fill = Tratamento)) +  
  geom_bar(stat = "identity", show.legend = TRUE) +
  
  geom_errorbar(aes(ymin = Media - Desvio,
                    ymax = Media + Desvio), width = 0.2)


#################################Teste Tukey#####################################################

Anova <- aov(Crescimento ~ Tratamento, data=Experimento)

residuo <- Anova$residuals


hist(residuo)
plot(density(residuo))



shapiro.test(residuo)


leveneTest(Crescimento ~ Tratamento, data=Experimento)

bartlett.test(Crescimento ~ Tratamento, data=Experimento)




anova <-  aov(Crescimento ~ Tratamento, data=Experimento)

tukey <- HSD.test(anova, "Tratamento")



tukey$groups <- tukey$groups %>% rownames_to_column(var = "Tratamento") # Problemas com M
tukey$means <- tukey$means %>% rownames_to_column(var = "Tratamento")



final <- merge(tukey$means, tukey$groups, by = "Tratamento")


cores <- c("#004800", "#343deb" , "#970000", "#32a889", "#28323d", "#9f0fd4")

ggplot(final, aes(Tratamento, Crescimento.x, fill = Tratamento)) +  
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.95, color = "#000000") +
  
  geom_errorbar(aes(ymin = Crescimento.x - std,
                    ymax = Crescimento.x + std), width = 0.2) + 
  geom_text(aes(label = groups, y = (Crescimento.x + std)), vjust = -1, size = 3, color = "black") +
  scale_fill_manual(values = cores) +
  theme_classic() + labs(x = "Tratamentos", y = "Crescimento (cm)") +
  scale_y_continuous(breaks=seq(0, 40, 5), limits = c(0, 40)) +
  labs(title = "Meu teste de hipótese", subtitle = "Teste Tukey")



#############Correlograma########################################################


Expercorr <- dplyr::select(Experimento, Crescimento, Dose)


Expercorr$Crescimento <- as.numeric(Expercorr$Crescimento)
Expercorr$Dose <- as.numeric(Expercorr$Dose)


Expercorr <- as.matrix(Expercorr)


corrgram(Expercorr)


corrgram(Expercorr,lower.panel = panel.shade,  upper.panel = panel.conf, text.panel = NULL, main="Correlação entre Dose e Crescimento")
