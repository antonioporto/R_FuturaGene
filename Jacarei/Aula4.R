## Configurar ambiente

# Carregar as bibliotecas necessárias para visualização e manipulação de dados
library('ggplot2')      # Criação de gráficos
library('tidyverse')    # Conjunto de pacotes para manipulação e visualização de dados
library('knitr')        # Para exibir tabelas formatadas
library('flextable')    # Para estilizar tabelas
library('officer')      # Para estilizar tabelas
library('ggsignif')     # Para adicionar anotações de significância aos gráficos

# Carregar o conjunto de dados Iris no R
iris <- readxl::read_excel('./iris.xlsx')

# Gerar estatísticas descritivas para o conjunto de dados Iris
summary(iris)  # Exibe um resumo das estatísticas básicas do conjunto de dados Iris

# Calcular média e desvio padrão para cada característica por espécie
iris_stats <- iris %>%
  group_by(Species) %>%  # Agrupar os dados por espécie
  summarise(
    # Calcular e formatar média e desvio padrão para cada característica
    S.Length_mean_sd = paste0(round(mean(Sepal.Length), 2), " (+/- ", round(sd(Sepal.Length), 2), ")"),
    S.Width_mean_sd = paste0(round(mean(Sepal.Width), 2), " (+/- ", round(sd(Sepal.Width), 2), ")"),
    P.Length_mean_sd = paste0(round(mean(Petal.Length), 2), " (+/- ", round(sd(Petal.Length), 2), ")"),
    P.Width_mean_sd = paste0(round(mean(Petal.Width), 2), " (+/- ", round(sd(Petal.Width), 2), ")")
  )

# Exibir a tabela de estatísticas com formatação usando flextable
flextable(iris_stats) %>% 
  fontsize(part = 'header', size = 8) %>% 
  fontsize(part = 'body', size = 8) %>% 
  set_caption('Tabela 1: Estatísticas descritivas (média e desvio padrão) das características do conjunto de dados Iris, agrupadas por espécie.',
              fp_p = fp_text(font.size = 8)) %>%
  autofit()


# Realizar análise estatística no conjunto de dados Iris
# Inicializar um dataframe vazio para armazenar os resultados do teste de Tukey HSD
tukey_results_all <- data.frame()

# Inicializar um dataframe vazio para armazenar os resultados da ANOVA
anova_results_all <- data.frame()

# Loop sobre as primeiras 4 colunas (Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
for (c in colnames(iris[, 1:4])) {
  
  # Realizar ANOVA para a característica atual (c) para verificar diferenças entre as espécies
  aov_result <- aov(as.formula(paste(c, "~ Species")), data = iris)
  
  # Extrair o resumo da ANOVA
  anova_summary <- summary(aov_result)[[1]]
  
  # Converter o resumo da ANOVA para um dataframe para melhor manipulação e exibição
  anova_df <- as.data.frame(anova_summary)
  
  # Arredondar os valores numéricos (soma dos quadrados, quadrado médio e valor de F) para 2 casas decimais
  anova_df$`Sum Sq` <- round(anova_df$`Sum Sq`, 2)
  anova_df$`Mean Sq` <- round(anova_df$`Mean Sq`, 2)
  anova_df$`F value` <- round(anova_df$`F value`, 2)
  
  # Formatar os valores-p em notação científica para facilitar a leitura
  anova_df$`Pr(>F)` <- formatC(anova_df$`Pr(>F)`, format = "e", digits = 2)
  
  # Adicionar o nome da característica (Sepal.Length, Sepal.Width, etc.) como uma nova coluna
  anova_df$Characteristic <- c
  
  # Adicionar os nomes das linhas (Species e Residuals) como uma nova coluna para identificá-los no dataframe
  anova_df$Source <- rownames(anova_df)
  
  # Redefinir os nomes das linhas para evitar problemas de indexação no dataframe
  rownames(anova_df) <- NULL
  
  # Anexar o resultado desta característica ao dataframe principal de ANOVA
  anova_results_all <- rbind(anova_results_all, anova_df)
  
  # Realizar o teste post-hoc de Tukey HSD para comparar os pares de espécies
  tukey_result <- TukeyHSD(aov_result)
  
  # Converter o resultado do Tukey HSD em um dataframe para melhor manipulação e exibição
  tukey_df <- as.data.frame(tukey_result$Species)
  
  # Arredondar as colunas numéricas (diferença, limites inferior e superior) para 2 casas decimais
  tukey_df$diff <- round(tukey_df$diff, 2)
  tukey_df$lwr <- round(tukey_df$lwr, 2)
  tukey_df$upr <- round(tukey_df$upr, 2)
  
  # Formatar os valores-p ajustados em notação científica
  tukey_df$`p adj` <- formatC(tukey_df$`p adj`, format = "e", digits = 2)
  
  # Adicionar o nome da característica como uma nova coluna
  tukey_df$Characteristic <- c
  
  # Adicionar os nomes das comparações de espécies (ex: 'versicolor-setosa') como uma nova coluna
  tukey_df$Comparison <- rownames(tukey_df)
  
  # Redefinir os nomes das linhas para evitar problemas de indexação
  rownames(tukey_df) <- NULL
  
  # Anexar o resultado desta característica ao dataframe principal de Tukey
  tukey_results_all <- rbind(tukey_results_all, tukey_df)
}

# Visualizar o dataframe combinado da ANOVA com as características organizadas
flextable(anova_results_all) %>% 
  fontsize(part = 'header', size = 8) %>% 
  fontsize(part = 'body', size = 8) %>% 
  autofit()

# Visualizar o dataframe combinado de Tukey HSD com as comparações organizadas
flextable(tukey_results_all) %>% 
  fontsize(part = 'header', size = 8) %>% 
  fontsize(part = 'body', size = 8) %>% 
  autofit()

# Reformatar os dados para o formato longo (necessário para plotar com ggplot)
iris_long <- iris %>%
  pivot_longer(cols = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
               names_to = "Characteristic",  # Nome das características
               values_to = "Value")          # Valores correspondentes


# Filtrar os resultados de Tukey para exibir apenas comparações significativas (p adj < 0.05)
comparisons <- tukey_results_all %>%
  filter(as.numeric(`p adj`) < 0.05) %>%  # Seleciona apenas comparações significativas
  mutate(sp1 = sapply(strsplit(Comparison, "-"), "[", 1),  # Extrai a primeira espécie
         sp2 = sapply(strsplit(Comparison, "-"), "[", 2))  # Extrai a segunda espécie


# Função para calcular o limite superior do boxplot (whisker) para uma dada característica e espécie
get_upper_whisker <- function(characteristic, species) {
  # Filtrar os dados para a espécie e característica especificadas
  filtered_data <- iris_long %>%
    filter(Species == species, Characteristic == characteristic) %>%
    pull(Value)
  
  # Calcular o primeiro quartil (Q1), terceiro quartil (Q3) e intervalo interquartil (IQR)
  Q1 <- quantile(filtered_data, 0.25)
  Q3 <- quantile(filtered_data, 0.75)
  IQR <- Q3 - Q1
  
  # Calcular o limite superior (whisker) como Q3 + 1.5 * IQR
  upper_whisker <- Q3 + 1.5 * IQR
  return(upper_whisker)
}

# Aplicar a função para cada linha no dataframe de comparações
comparisons <- comparisons %>%
  rowwise() %>%
  mutate(
    upper_whisker_sp1 = get_upper_whisker(Characteristic, sp1),  # Limite superior da 1ª espécie
    upper_whisker_sp2 = get_upper_whisker(Characteristic, sp2),  # Limite superior da 2ª espécie
    y_position = max(upper_whisker_sp1, upper_whisker_sp2)  # Define o y_position com base no maior whisker
  ) %>%
  ungroup()

# Função para ajustar os valores de y_position para evitar sobreposição de anotações
adjust_y_position <- function(df, increment = 0.5) {
  # Ordenar e ajustar o y_position para cada grupo de características
  df <- df %>%
    arrange(Characteristic, y_position) %>%
    group_by(Characteristic) %>%
    mutate(
      # Ajustar o y_position para evitar sobreposição
      y_position = purrr::accumulate(y_position, function(prev, current) {
        if (current <= prev) {
          return(prev + increment)  # Adiciona um incremento se houver sobreposição
        } else {
          return(current)  # Mantém o valor original se não houver sobreposição
        }
      })
    ) %>%
    ungroup()  # Remover o agrupamento
  
  return(df)
}

# Aplicar a função ao dataframe de comparações
comparisons <- adjust_y_position(comparisons)

# Criar o boxplot com as anotações de significância
p <- ggplot(iris_long, aes(x = Species, y = Value)) +
  geom_boxplot(position = 'dodge', aes(fill=Species)) +
  geom_signif(
    data = comparisons,
    aes(xmin = sp1, 
        xmax = sp2, 
        annotations = `p adj`, 
        y_position = y_position*1.1),
    textsize = 3, vjust = 0,
    manual = TRUE) + 
  facet_wrap(~ Characteristic, scales = "free_y") +
  theme_minimal() +
  labs(title = "Boxplot de Dados Iris com Significância de Tukey HSD",
       x = "Espécies",
       y = "Valor",
       caption = "Figura 1: Boxplot das características do conjunto de dados Iris,\ncom significância estatística (Tukey HSD) entre as espécies") +
  theme(plot.caption = element_text(hjust = 0.5))

plot(p)
