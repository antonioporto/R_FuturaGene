# Antonio Carlos Porto, 12.09.24
# Aula 2


##### Setting the directory #########################################################
setwd("D:\\Desktop\\Curso R\\3? Aula")

# Reading the file inside the directory
dados <- read.csv("Iris.csv", header=TRUE, na.string = 'NA')


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
     
