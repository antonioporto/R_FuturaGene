# Carregar as bibliotecas necessárias
# Certifique-se de instalar esses pacotes, caso ainda não estejam instalados
# install.packages(c('ggplot2', 'dplyr', 'tidyr', 'pheatmap', 'readr', 'RColorBrewer', 'ComplexHeatmap'))

library(ggplot2)    # Para criar gráficos de dispersão e outras visualizações
library(dplyr)      # Para manipulação de dados
library(tidyr)      # Para reestruturar dados
library(pheatmap)   # Para criar heatmaps
library(readr)      # Para leitura de arquivos delimitados
library(RColorBrewer) # Para gerar paletas de cores
library(ComplexHeatmap) # Alternativa para heatmaps mais avançados (opcional)
library(viridisLite)
library(triangle)
library(tibble)

# Ajuste o diretório de trabalho
setwd('/home/fciamponi/Aula4/')

# 1. Leitura dos Dados ---------------------------------------------------------

# Leitura dos arquivos de metadata, relatedness e PCA
# Usar read_delim para ler arquivos delimitados por tabulação com um delimitador personalizado
metadata <- read_delim('./Aula4_metadata.txt', delim = "\t")
relat <- read_delim('./Aula4_relatedness.txt', delim = "\t")
pca <- read_delim('./Aula4_PCA.txt', delim = "\t")

# 2. Gráfico de Dispersão da PCA ------------------------------------------------

# Mesclar os dados da PCA com o metadata usando a coluna 'Amostra' (Sample)
pcaGen <- merge(pca, metadata, by = "Amostra")

# Criar um gráfico de dispersão dos dois primeiros componentes principais (PC1 vs PC2)
# Colorir os pontos de acordo com a coluna 'Cruzamento' (categoria de cruzamento)
pca_plot <- ggplot(pcaGen, aes(x = PC1, y = PC2, color = Cruzamento)) +
  geom_point() +  # Adicionar pontos de dispersão
  theme_minimal() +  # Usar um tema minimalista
  labs(color = "Cruzamento") +  # Definir o título da legenda
  theme(
    legend.position = "top",  # Colocar a legenda no topo
    legend.title = element_text(size = 8)  # Definir o tamanho da fonte do título da legenda
  )

# Exibir o gráfico de dispersão da PCA
print(pca_plot)

# 3. Heatmap de Relatedness (Similaridade) --------------------------------------

# Reestruturar os dados de relatedness para criar uma matriz quadrada (amostras vs amostras)
# A função spread reorganiza a tabela para criar essa matriz
relat_matrix <- relat %>%
  spread(key = AM2, value = RELAT) %>%
  column_to_rownames(var = "AM1") %>%  # Use the first column as row names
  as.matrix()  # Convert to matrix

reflect_triangle <- function(m, from=c("lower", "upper")) {
  ix <- switch(match.arg(from), lower=upper.tri, upper=lower.tri)(m, diag=FALSE)
  m[ix] <- t(m)[ix]
  m
}

relat_matrix <- reflect_triangle(relat_matrix, from = 'upper')

relat_matrix <- t(apply(relat_matrix, 1, function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
}))

# Convert the matrix to numeric (in case it contains non-numeric values)
relat_matrix <- apply(relat_matrix, 2, as.numeric)

# 4. Preparar as Anotações de Linha --------------------------------------------

progenies <- metadata %>%
  select(Amostra, Cruzamento)

# Exemplo com anotações simplificadas
row_annotations_simple <- data.frame(
  Cruzamento = factor(progenies$Cruzamento)
)

annotation_colors_simple <- list(
  Cruzamento = setNames(viridis(n = length(unique(row_annotations_simple$Cruzamento))), levels(row_annotations_simple$Cruzamento))
)

# 6. Criar o Heatmap -----------------------------------------------------------

pheatmap(
  relat_matrix,                      # A matriz de relatedness
#  annotation_row = row_annotations_simple,   # Anotações simplificadas
#  annotation_colors = annotation_colors_simple, # Mapeamento de cores simplificado
  color = colorRampPalette(c("steelblue", 'white',"goldenrod"))(100), # Gradiente de cores para o heatmap
  main = "Heatmap de Similaridade",    # Título do heatmap
  cluster_rows = TRUE,                # Clusterizar as linhas
  cluster_cols = TRUE                 # Clusterizar as colunas
)

# 7. Fim do Script -------------------------------------------------------------
# Este script pode ser estendido ou automatizado convertendo-o em um documento RMarkdown.
# O arquivo RMarkdown permite uma melhor automação, reprodutibilidade e relatórios.
