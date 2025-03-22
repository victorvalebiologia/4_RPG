# Apresentação
Análises e gráficos da campanha de Pokémon

## Início
Primeiro, vamos indicar as pastas corretas.
```
getwd()
setwd("/home/valev/Área de Trabalho/R/RPG/arton") 
```

Agora baixar e ler alguns pacotes básicos.

```
if(!require(pacman, quietly = TRUE))(install.packages("pacman")) #agrupador de funções
pacman::p_load(magrittr,dplyr,reshape2) #magrittr para operações de pipe/dplyr para manipulador de dados
pacman::p_load(ggplot2, ggrepel, graphics,lubridate,gghighlight) #devtools, 
pacman::p_load(forcats,iNEXT,tidyr,tibble,iNEXT)
pacman::p_load(vegan,ggside)  #vegan para estatística ecológica/graphics para os gráficos


#sudo apt-get install r-cran-devtools

```
Agora vamos adicionar a planilha.
```
pacman::p_load(openxlsx)
caminho.do.arquivo <- "/home/valev/Área de Trabalho/Planilhas/25_2_16_arton.xlsx"
planilhatotal <- read.xlsx(caminho.do.arquivo, #local do arquivo
                           sheet = 1, # em qual planilha estão os dados
                           colNames = T, # as colunas dos dados possuem nomes?
                           na.strings = "NA") # como estão identificados os dados omissos?

#tail(planilhatotal)
```
Ou pelo Google drive.
```
# Carregar pacotes necessários
pacman::p_load(googledrive, googlesheets4, readxl, dplyr)

# Autenticar no Google Drive
drive_auth()

# 1. Acessar a pasta pelo ID
pasta_id <- "1yVHEzmcHc5GnwoFqcdT60eSlvX-QEn3l"
pasta <- drive_get(as_id(pasta_id))

# 2. Listar os arquivos na pasta
arquivos_na_pasta <- drive_ls(pasta)

# 3. Filtrar pelo nome do arquivo
arquivo <- arquivos_na_pasta %>% filter(name == "25_2_16_arton")

# 4. Verificar se encontrou exatamente um arquivo
if (nrow(arquivo) == 1) {
  # Baixar o arquivo
  drive_download(file = as_id(arquivo$id), path = "25_2_16_arton", overwrite = TRUE)
  message("Arquivo baixado com sucesso!")

  # 5. Ler o arquivo Excel e corrigir nomes de colunas
  planilhatotal <- read_excel("25_2_16_arton.xlsx", .name_repair = "minimal")

  # Exibir os nomes das colunas para verificação
  print(names(planilhatotal))

  message("Arquivo lido com sucesso!")

} else if (nrow(arquivo) == 0) {
  stop("Erro: Arquivo '25_2_16_arton' não encontrado na pasta.")
} else {
  stop("Erro: Mais de um arquivo com o mesmo nome encontrado. Verifique manualmente.")
}


```
E filtrar e montar a pespectiva de data.
```
p2 <- subset(planilhatotal, !is.na(Jogador)) #tirar n/a da ano
p2 <- subset(p2, !is.na(Personagem)) #tirar n/a da pontos
pbase2 <- p2 #sem data

colnames(p2) <- make.names(colnames(p2), unique = TRUE)

p2 <- p2 %>%
  mutate(
    Mês_A = ifelse(is.na(Mês_A) | Mês_A == "", 1, Mês_A),
    Dia_A = ifelse(is.na(Dia_A) | Dia_A == "", 1, Dia_A)
  )

p2 <- p2 %>%
  mutate(
    Mês = ifelse(is.na(Mês) | Mês == "", 1, Mês),
    Dia = ifelse(is.na(Dia) | Dia == "", 1, Dia),
    Ano = ifelse(is.na(Ano) | Ano == "", 2025, Ano)
  )

p2 <- subset(p2, !is.na(Ano_A))
p3 <- subset(p2, !is.na(Mês_A))
p3 <- subset(p3, !is.na(Dia_A))
pbase <- p3

Data <- p3 %>% 
  select(Ano_A,Mês_A,Dia_A) %>% 
  mutate(Data = make_date(Ano_A,Mês_A,Dia_A))
Data <- data.frame(p3,Data)

```
E outra data
```

p2 <- subset(planilhatotal, !is.na(Jogador)) #tirar n/a da ano
p2 <- subset(p2, !is.na(Personagem)) #tirar n/a da pontos
pbase2 <- p2 #sem data

colnames(p2) <- make.names(colnames(p2), unique = TRUE)

p2 <- subset(p2, !is.na(Ano))
p3 <- subset(p2, !is.na(Mês))
p3 <- subset(p3, !is.na(Dia))
pbase <- p3

Data2 <- Data %>% 
  select(Ano,Mês,Dia) %>% 
  mutate(Data2 = make_date(Ano,Mês,Dia))
Data2 <- data.frame(Data,Data2)
```

#### Principal 
### Diversidade

Acumulação por data e treinador
```
p2 <- Data
p3 <- subset(p2, N_categoria == "20") 
p3 <- subset(p3, !is.na(Combate))


df <- p3 %>% arrange(Data)

df <- df %>%
  group_by(Personagem) %>%
  mutate(Acumulado = row_number())

# Criando o gráfico
R <- ggplot(df, aes(x = Data, y = Acumulado, color = Personagem)) +
  geom_line() +
  geom_point() +
  labs(title = '',
       x = 'Data do Combate',
       y = 'Número de Adversários') +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7, 8))  # Até 9 formas distintas

R

# Salvando o gráfico
ggsave(filename = "2025_3_Acum_spps.pdf", plot = R, width = 20, height = 10, device = "pdf")

```
Agora o acumualdo por espécie
```
# Carregar pacotes necessários
pacman::p_load(dplyr, tidyr, vegan, ggplot2, purrr, reshape2)

# Filtrar os dados
p3 <- Data %>% filter(N_categoria == "20", !is.na(Combate))

# Lista de treinadores
treinadores <- c("Bryn", "Zirk", "Baltazar Sauvage", "Darius Ravnus")

# Função para calcular a curva de acumulação de espécies
calculate_specaccum <- function(df) {
  # Verifica se há mais de 1 amostra
  if (nrow(df) < 2) return(NULL)  
  
  # Converte os dados para formato largo (wide) com a quantidade total por espécie (Oponente)
  df_wide <- dcast(df, Data ~ Oponente, value.var = "Quantidade", fun.aggregate = sum, fill = 0)
  
  # Verifica se há pelo menos 2 espécies (colunas além de "Data")
  if (ncol(df_wide) < 3) return(NULL)  
  
  # Remove a coluna "Data", que não é necessária para o cálculo da curva
  df_wide <- df_wide %>% select(-Data)
  
  # Verifica se há pelo menos uma coluna com dados válidos (não zero ou NA)
  if (all(colSums(df_wide, na.rm = TRUE) == 0)) {
    message("Todas as espécies possuem contagem zero ou dados ausentes.")
    return(NULL)
  }
  
  # Calcula a curva de acumulação de espécies
  specaccum_result <- specaccum(df_wide, method = "collector")
  
  return(specaccum_result)
}

# Calcular curvas para cada treinador (somente se houver dados suficientes)
curvas_acumulacao <- map(setNames(treinadores, treinadores), function(treinador) {
  df_treinador <- filter(p3, Personagem == treinador)
  
  # Verifica se há dados suficientes (mínimo 2 amostras e 2 oponentes)
  curva <- calculate_specaccum(df_treinador)
  if (!is.null(curva)) return(curva) else return(NULL)
})

# Remover treinadores sem dados suficientes
curvas_acumulacao <- discard(curvas_acumulacao, is.null)

# Criar um data frame combinando os resultados
df_combined <- bind_rows(
  map_dfr(names(curvas_acumulacao), 
          ~ tibble(Treinador = .x, 
                   Samples = curvas_acumulacao[[.x]]$sites, 
                   Species = curvas_acumulacao[[.x]]$richness))
)

# Verificar se há dados suficientes para plotagem
if (nrow(df_combined) > 0) {
  df_combined <- df_combined %>%
    filter(!is.na(Samples) & !is.na(Species))  # Remover valores ausentes

  ggplot(df_combined, aes(x = Samples, y = Species, color = Treinador)) +
    geom_line(linewidth = 1) +  # Substituído 'size' por 'linewidth'
    labs(title = "Curva de Acumulação de Espécies por Treinador",
         x = "Número de Amostras",
         y = "Riqueza de Espécies") +
    theme_minimal() +
    scale_color_manual(values = rainbow(length(unique(df_combined$Treinador)))) +
    theme(axis.title = element_text(size = 18), 
          axis.text = element_text(size = 14), 
          legend.position = "bottom")
} else {
  message("Nenhum treinador tem dados suficientes para gerar uma curva de acumulação.")
}

ggsave(filename = "2025_3_Acum_pok.pdf", width = 20, height = 10, device = "pdf")
```
### Similaridade
Agora um jacard
```
pacman::p_load("ade4", "NbClust")

p2 <- pbase2
p3 <- subset(p2, !is.na(Personagem)) 
p3 <- subset(p2, !is.na(Classe)) 
p3 <- subset(p2, T_Atributo == "Classe") 

p3 <- p3[!duplicated(p3), ]

#p3[-1][p3[-1] < 0] <- 0 #transformar negativo em 0

local<-reshape2::dcast(p3, Personagem ~ Classe + Raça, value.var = "Nível", fun = NULL)
local=data.frame(local, row.names=1)
local <- local %>%
  mutate_all(~ ifelse(. > 0, 1, 0))

d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) #method 1Rota is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
plot(hc, labels=local$ID)    # plot the dendrogram

```
## PCA

PCA para identificar a paisagem mais importante por família de Pokémon
```
pacman::p_load(ggfortify, cluster, reshape2, dplyr)

p2 <- pbase2
colnames(p2) <- make.names(colnames(p2), unique = TRUE)

# Filtrando os dados
p3 <- p2 %>%
  filter(!is.na(Personagem), 
         !is.na(Bonus), 
         !is.na(T_bonus), 
         !is.na(Quantidade),
         N_categoria == "20", 
         Equipado == "1")

# Reshaping do dataframe
local <- reshape2::dcast(p3, Bonus + T_bonus ~ Personagem, value.var = "Quantidade", fun.aggregate = length)

# Certificando-se de que não há linhas sem dados
local <- local[complete.cases(local), ]
rownames(local) <- local$Bonus  # Usando Bonus como nome das linhas
local <- local[, -1]  # Remover a coluna 'Bonus' agora usada como nome das linhas

# Selecionar apenas colunas numéricas para o PCA
local_numeric <- local %>% select(where(is.numeric))

# Realizar PCA
pca_res <- prcomp(local_numeric, scale. = TRUE)

# Visualizar o PCA
pca <- autoplot(pca_res, data = local, colour = 'T_bonus', label = TRUE, label.size = 4, 
                frame = TRUE, frame.type = NULL, frame.color = 'T_bonus', 
                loadings = TRUE, loadings.colour = 'blue', loadings.label = TRUE, 
                loadings.label.size = 3) +
   theme_minimal() +
   theme(legend.position = "none")

pca

#ggsave(width = 20, height = 10, device = "pdf", filename = "2024_10_PCA", plot = pca)
#path = "/home/user/Área de Trabalho/Serviços/ES - Rio Bananal/2021_03_03_Grancol/R"

```

PCA para identificar a paisagem mais importante por Pokémon
```
p2 <- pbase2
colnames(p2) <- make.names(colnames(p2), unique = TRUE)

# Filtrando os dados
p3 <- p2 %>%
  filter(!is.na(Personagem), 
         !is.na(Bonus), 
         !is.na(T_bonus), 
         !is.na(Quantidade),
         N_categoria == "20", 
         Equipado == "1")

# Criar matriz de dados com Personagem por Bonus
local <- reshape2::dcast(p3, Personagem ~ Bonus, value.var = "Quantidade", fun.aggregate = length)
local <- local[complete.cases(local), ]
rownames(local) <- local$Personagem  # Usando Personagem como nome das linhas
local <- local[, -1]  # Remover a coluna 'Personagem' agora usada como nome das linhas

local_numeric <- local %>% select(where(is.numeric))

# Realizar PCA
pca_res <- prcomp(local_numeric, scale. = TRUE)

# Visualizar PCA com a variável T_bonus
pca <- autoplot(pca_res, data = local_numeric, label = TRUE, label.size = 4, 
                label.colour = 'black', # Cor dos rótulos para os personagens
                loadings = TRUE, loadings.colour = 'blue', loadings.label = TRUE, 
                loadings.label.size = 3) +                   
   theme_minimal() +
   theme(legend.position = "none")

# Exibir gráfico PCA
pca


#ggsave(width = 20, height = 10, device = "pdf", filename = "2024_10_PCA2", plot = pca)
#path = "/home/user/Área de Trabalho/Serviços/ES - Rio Bananal/2021_03_03_Grancol/R"



```
### Análises eploratórias

## Rota

Capturar por Tempo
``` 
p2 <- Data2
colnames(p2) <- make.names(colnames(p2), unique = TRUE)

# Filtrando os dados
p3 <- p2 %>%
  filter(!is.na(Personagem), 
         N_categoria == "20", 
         Equipado == "1", 
         #T_Atributo != "Base", 
         Ano_A >= 1330)

p4 <- p3
p4 <- p4 %>% arrange(Data)
p4 <- p4 %>% arrange(Data2)

ggplot(p4, aes(x = Data, y = Data2)) + 
  geom_jitter(aes(colour = Personagem, size = Nível, shape = T_Atributo), alpha = 0.6) + 
  geom_smooth(method = lm, se = FALSE, alpha = 0.6, aes(colour = Personagem)) + 
  scale_shape_manual(values = 0:10) +
  scale_size(range = c(5, 18), name = "Nível") +
  stat_ellipse(geom="polygon", aes(fill = Personagem), alpha = 0.2, show.legend = TRUE, level = 0.25) + 
  # Adicionando boxplots laterais
  geom_ysideboxplot(aes(fill = Personagem), alpha = 0.5) + 
  geom_xsidedensity(aes(y = after_stat(count), group = Personagem, fill = Personagem), alpha = 0.5, size = 0.5, position = "stack", outline.type = "full") + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14)) +
  scale_fill_manual(values = rainbow(length(unique(p4$Personagem)))) +
  theme_minimal()

#ggsave(width = 20, height = 13, device = "pdf", filename = "2025_3_tempo")

```
Destaque gráfico lateral
```

p2 <- Data2
colnames(p2) <- make.names(colnames(p2), unique = TRUE)

# Filtrando os dados
p3 <- p2 %>%
  filter(!is.na(Personagem), 
         N_categoria == "20")

ggplot(p4, aes(x = Data2, y = T_Atributo, colour = Personagem)) +
  #geom_boxplot() +
  #geom_smooth(aes(color = Personagem), se = TRUE, method = "loess") +
  stat_density(aes(y = after_stat(count), fill = Personagem), alpha = 0.5, size = 1, position = "stack") + #position = fill
  scale_fill_manual(values = rainbow(length(unique(p4$Personagem)))) +
  theme_minimal() +
  labs(title = "",
       subtitle = "",
       x = "Tempo",
       y = "Personagem") +
  theme(ggside.panel.scale.x = 0.2,
    ggside.panel.scale.y = 0.2,
    legend.position = "bottom",  # Posiciona a legenda à direita para melhor clareza
    axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos do eixo X para melhor leitura
  
#ggsave("2025_3_overlap2.png",width = 12, height = 8, dpi = 600)

```

MST (Minimum Spanning Tree): É uma técnica de análise de rede que encontra a árvore de menor custo que conecta todos os vértices de um grafo ponderado. Em biogeografia, a MST pode ser usada para identificar padrões de conectividade entre diferentes áreas geográficas com base em dados de distância ou similaridade.

No gŕafico, as linhas representas as comunidades com menor cursto de coenxão e os pontos sem linhas as comunidades isoladas.
```
pacman::p_load("ggplot2", "spaa", "recluster", "analogue", "ape", "vegan")

p2 <- Data2
colnames(p2) <- make.names(colnames(p2), unique = TRUE)

# Filtrando os dados
p3 <- p2 %>%
  filter(!is.na(Personagem),
         !is.na(Estrutura),
         N_categoria >= 19 & N_categoria <= 29)
         
local<-reshape2::dcast(p3, Estrutura ~ Personagem, value.var = "Código",fun.aggregate = NULL)
local <- local[complete.cases(local), ]
local=data.frame(local, row.names=1)

dist_matrix <- vegdist(local, method = "jaccard")
dist_matrix[is.na(dist_matrix)] <- 0  # Por exemplo, substituir NA por 0
mst_tree <- mst(dist_matrix)
nomes_localidades <- rownames(local)

mst_coordinates <- cmdscale(dist_matrix)

# Converter os nomes das linhas em números inteiros
indices_nos <- seq_len(nrow(mst_coordinates))
rownames(mst_coordinates) <- indices_nos

# Criar uma matriz de coordenadas das arestas
edges <- which(mst_tree != 0, arr.ind = TRUE)
edges <- cbind(edges, Value = mst_tree[edges])

# Converter para dataframe
df_arestas <- as.data.frame(edges)
colnames(df_arestas) <- c("X1", "X2", "Value")

# Criar dataframe vazio para as linhas
df_lines <- data.frame()

# Iterar sobre as arestas e adicionar as coordenadas ao dataframe df_lines
for (i in seq_len(nrow(df_arestas))) {
  indice_X1 <- df_arestas$X1[i]
  indice_X2 <- df_arestas$X2[i]
  
  # Obter as coordenadas dos nós
  coord_X1 <- mst_coordinates[indice_X1, 1]
  coord_Y1 <- mst_coordinates[indice_X1, 2]
  coord_X2 <- mst_coordinates[indice_X2, 1]
  coord_Y2 <- mst_coordinates[indice_X2, 2]
  
  # Adicionar as coordenadas ao dataframe df_lines
  df_lines <- rbind(df_lines, data.frame(X1 = coord_X1, Y1 = coord_Y1, X2 = coord_X2, Y2 = coord_Y2))
}

# Convertendo as coordenadas da matriz mst_coordinates em dataframe
df_mst_coordinates <- as.data.frame(mst_coordinates)
colnames(df_mst_coordinates) <- c("X", "Y")

# Plotar a MST com ggplot2
ggplot() +
  geom_segment(data = df_lines, aes(x = X1, y = Y1, xend = X2, yend = Y2), color = "blue") +
  geom_point(data = df_mst_coordinates, aes(x = X, y = Y), color = "red") +
  geom_text_repel(data = df_mst_coordinates, aes(x = X, y = Y, label = nomes_localidades), vjust = -0.5) +
  labs(title = "Minimum Spanning Tree", x = "Comunidade", y = "Comunidade") +
  theme_minimal() 


#ggsave(width = 20, height = 10, device = "pdf", filename = "2024_5_mst")
```
E Personagens

```
p2 <- Data2
colnames(p2) <- make.names(colnames(p2), unique = TRUE)

# Filtrando os dados
p3 <- p2 %>%
  filter(!is.na(Personagem),
         !is.na(Estrutura),
         N_categoria >= 20 & N_categoria <= 23)
         
local<-reshape2::dcast(p3, Personagem ~ Bairro, value.var = "Código",fun.aggregate = NULL) #ou Estrutura
local <- local[complete.cases(local), ]
local=data.frame(local, row.names=1)

dist_matrix <- vegdist(local, method = "jaccard")
dist_matrix[is.na(dist_matrix)] <- 0  # Por exemplo, substituir NA por 0
mst_tree <- mst(dist_matrix)
nomes_localidades <- rownames(local)

mst_coordinates <- cmdscale(dist_matrix)

# Converter os nomes das linhas em números inteiros
indices_nos <- seq_len(nrow(mst_coordinates))
rownames(mst_coordinates) <- indices_nos

# Criar uma matriz de coordenadas das arestas
edges <- which(mst_tree != 0, arr.ind = TRUE)
edges <- cbind(edges, Value = mst_tree[edges])

# Converter para dataframe
df_arestas <- as.data.frame(edges)
colnames(df_arestas) <- c("X1", "X2", "Value")

# Criar dataframe vazio para as linhas
df_lines <- data.frame()

# Iterar sobre as arestas e adicionar as coordenadas ao dataframe df_lines
for (i in seq_len(nrow(df_arestas))) {
  indice_X1 <- df_arestas$X1[i]
  indice_X2 <- df_arestas$X2[i]
  
  # Obter as coordenadas dos nós
  coord_X1 <- mst_coordinates[indice_X1, 1]
  coord_Y1 <- mst_coordinates[indice_X1, 2]
  coord_X2 <- mst_coordinates[indice_X2, 1]
  coord_Y2 <- mst_coordinates[indice_X2, 2]
  
  # Adicionar as coordenadas ao dataframe df_lines
  df_lines <- rbind(df_lines, data.frame(X1 = coord_X1, Y1 = coord_Y1, X2 = coord_X2, Y2 = coord_Y2))
}

# Convertendo as coordenadas da matriz mst_coordinates em dataframe
df_mst_coordinates <- as.data.frame(mst_coordinates)
colnames(df_mst_coordinates) <- c("X", "Y")

# Plotar a MST com ggplot2
ggplot() +
  geom_segment(data = df_lines, aes(x = X1, y = Y1, xend = X2, yend = Y2), color = "blue") +
  geom_point(data = df_mst_coordinates, aes(x = X, y = Y), color = "red") +
  geom_text_repel(data = df_mst_coordinates, aes(x = X, y = Y, label = nomes_localidades), vjust = -0.5) +
  labs(title = "Minimum Spanning Tree", x = "Comunidade", y = "Comunidade") +
  theme_minimal() 


#ggsave(width = 20, height = 10, device = "pdf", filename = "2024_5_mst")
```

## Tabela de atributos

E filtrar e montar a pespectiva de data.
```
# Filtrando os dados
p3 <- subset(Data2, N_categoria == 20)
#p3 <- p2 %>%   filter(N_categoria >= 20 & N_categoria <= 23)
p3 <- p3[!is.na(p3$Atributos), ]

# Ordenando p3 pela data
p3 <- p3[order(p3$Data2),]

# Calculando a variável acumulado
p3 <- transform(p3, acumulado = ave(Atributos, Personagem, FUN = cumsum))

# Criando o gráfico com p3 (onde acumulado foi calculado)
ggplot(p3, aes(x = Data2, y = acumulado, color = Personagem)) +
  geom_line() +
  labs(title = "Acúmulo de atributos por data",
       x = "Data", y = "Acúmulo do Atributo") +
  scale_color_manual(values = rainbow(length(unique(p3$Personagem)))) +  # Definir cores com base nos personagens
  theme_minimal() +
  theme(legend.position = "bottom")  # Posicionar a legenda na parte inferior


#ggsave(width = 20, height = 10, device = "pdf", filename = "2025_3_tratotal")
```
Um gráfico de nuvem

```
# Carregar pacotes
pacman::p_load(tm, xml2, SnowballC, readr, dplyr, wordcloud, wesanderson)

# Filtrar dados para remover NAs na coluna Evento
p2 <- pbase %>% filter(!is.na(Evento))

# Preparar o corpus de palavras
words <- p2 %>% select(Evento) %>% pull()  # Usando pull() para extrair a coluna como vetor
word.corpus <- Corpus(VectorSource(words)) 

# Transformações no corpus
word.corpus <- word.corpus %>%
  tm_map(removePunctuation) %>%   # Eliminar pontuação
  tm_map(removeNumbers) %>%       # Eliminar números
  tm_map(stripWhitespace) %>%     # Eliminar espaços extras
  tm_map(content_transformer(tolower)) %>% # Converter para minúsculas
  tm_map(stemDocument)            # Aplicar stemming (radicalização)

# Criar a matriz de termos e frequências
word.counts <- as.matrix(TermDocumentMatrix(word.corpus))
word.freq <- sort(rowSums(word.counts), decreasing = TRUE)

# Exibir as 10 palavras mais frequentes
head(word.freq, 10)

# Gerar o gráfico de nuvem de palavras com mais palavras e fonte maior
set.seed(32)  # Definir a semente para reprodutibilidade
wordcloud(words = names(word.freq), 
          freq = word.freq, 
          scale = c(6, 0.5),  # Aumentar o tamanho da fonte (valor maior no primeiro número)
          max.words = 200,  # Aumentar o número de palavras para 200
          random.order = FALSE, 
          color = wes_palette("Darjeeling1"), 
          rot.per = 0.7)



```
