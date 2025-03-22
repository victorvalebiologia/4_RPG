# Apresentação
Análises e gráficos da campanha de Pokémon

## Início
Primeiro, vamos indicar as pastas corretas.
```
getwd()
setwd("/home/valev/Área de Trabalho/R/RPG/pokemon") 
```

Agora baixar e ler alguns pacotes básicos.

```
if(!require(pacman, quietly = TRUE))(install.packages("pacman")) #agrupador de funções
pacman::p_load(magrittr,dplyr,reshape2) #magrittr para operações de pipe/dplyr para manipulador de dados
pacman::p_load(ggplot2, ggrepel, graphics,lubridate,gghighlight) #devtools, 
pacman::p_load(forcats,iNEXT,tidyr,tibble,iNEXT)
pacman::p_load(vegan)  #vegan para estatística ecológica/graphics para os gráficos

#sudo apt-get install r-cran-devtools

```
Agora vamos adicionar a planilha.
```
pacman::p_load(openxlsx)
caminho.do.arquivo <- "/home/valev/Área de Trabalho/Planilhas/2023_03_11_pokemon.xlsx"
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
pasta_id <- "1LsI7be4NjnlHF5cJM_Zul8XpUIkqOJ5J"
pasta <- drive_get(as_id(pasta_id))

# 2. Listar os arquivos na pasta
arquivos_na_pasta <- drive_ls(pasta)

# 3. Filtrar pelo nome do arquivo
arquivo <- arquivos_na_pasta %>% filter(name == "2023_03_11_pokemon.xlsx")

# 4. Verificar se encontrou exatamente um arquivo
if (nrow(arquivo) == 1) {
  # Baixar o arquivo
  drive_download(file = as_id(arquivo$id), path = "2023_03_11_pokemon.xlsx", overwrite = TRUE)
  message("Arquivo baixado com sucesso!")

  # 5. Ler o arquivo Excel e corrigir nomes de colunas
  planilhatotal <- read_excel("2023_03_11_pokemon.xlsx", .name_repair = "minimal")

  # Exibir os nomes das colunas para verificação
  print(names(planilhatotal))

  message("Arquivo lido com sucesso!")

} else if (nrow(arquivo) == 0) {
  stop("Erro: Arquivo '2023_03_11_pokemon.xlsx' não encontrado na pasta.")
} else {
  stop("Erro: Mais de um arquivo com o mesmo nome encontrado. Verifique manualmente.")
}


```
E filtrar e montar a pespectiva de data.
```
p2 <- subset(planilhatotal, !is.na(Região)) #tirar n/a da ano
p2 <- subset(p2, !is.na(Número)) #tirar n/a da pontos
pbase2 <- p2 #sem data

p2 <- subset(p2, !is.na(Altura)) #tirar n/a da pontos
p2 <- subset(p2, !is.na(Ano))
p3 <- subset(p2, !is.na(Mês))
p3 <- subset(p3, !is.na(Dia))
pbase <- p3

Data <- p3 %>% 
  select(Ano,Mês,Dia) %>% 
  mutate(Data = make_date(Ano,Mês,Dia))
Data <- data.frame(p3,Data)

```

#### Principal 
### Diversidade

Acumulação por data e treinador
```
p2 <- Data
p2 <- subset(p2, T.Encontro == "1") 

p3 <- subset(p2, T.Treinador == "Jogador") 

df <- p2 %>% arrange(Data)

df <- df %>%
  group_by(Treinador) %>%
  mutate(Acumulado = row_number())

# Criando o gráfico
R <- ggplot(df, aes(x = Data, y = Acumulado, color = Treinador)) +
  geom_line() +
  geom_point() +
  labs(title = 'Curva de Acumulação de Pokémon por Treinador',
       x = 'Data de Captura',
       y = 'Número Acumulado de Pokémon') +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7, 8))  # Até 9 formas distintas

R

# Salvando o gráfico
ggsave(filename = "2025_3_Acum_spps.pdf", plot = R, width = 20, height = 10, device = "pdf")

```
Agora o acumualdo por espécie
```
pacman::p_load(dplyr, tidyr, vegan, ggplot2, purrr)

p2 <- Data
p2 <- subset(p2, T.Encontro == "1") 
p3 <- subset(p2, T.Treinador == "Jogador") 

# Lista de treinadores
treinadores <- c("Afonso", "Daniel", "Felipe", "Hugo", "Samuel", "Tiago", "Tulipa", "Vinícius") #"Mardem", 

# Função para calcular a curva de acumulação de espécies
calculate_specaccum <- function(p4) {
  acum <- reshape2::dcast(p4, Data ~ Pokémon, value.var = "Total.dex", fun = sum)
  acum <- data.frame(acum, row.names = 1)
  return(specaccum(acum, method = "collector"))
}

# Lista para armazenar as curvas de acumulação
curvas_acumulacao <- list()

# Loop sobre cada treinador
for (treinador in treinadores) {
  # Filtrar dados para o treinador atual
  p4 <- subset(p3, Treinador == treinador)
  # Calcular a curva de acumulação de espécies
  curvas_acumulacao[[treinador]] <- calculate_specaccum(p4)
}

library(ggplot2)

# Criar um data frame combinando as curvas de acumulação de todos os treinadores
df_combined <- data.frame(
  Treinador = rep(treinadores, sapply(curvas_acumulacao, function(x) length(x$sites))),
  Samples = unlist(lapply(curvas_acumulacao, function(x) x$sites)),
  Species = unlist(lapply(curvas_acumulacao, function(x) x$richness))
)

# Plotar as curvas de acumulação de espécies usando ggplot2
ggplot(df_combined, aes(x = Samples, y = Species, color = Treinador)) +
  geom_line() +
  labs(title = "Curva de Acumulação de Espécies por Treinador",
       x = "Número de Amostras",
       y = "Riqueza de Espécies") +
  theme_minimal() +
  scale_fill_manual(values = rainbow(length(unique(df_combined$Treinador)))) +
   theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14), legend.position="bottom")

ggsave(filename = "2025_3_Acum_pok.pdf", width = 20, height = 10, device = "pdf")
```
### Similaridade
Agora um jacard
```
pacman::p_load("ade4", "NbClust")

p2 <- pbase2
p2 <- subset(p2, !is.na(Rota)) 
p3 <- subset(p2, T.Treinador == "Selvagem") 
#p3 <- subset(p2, Oponente == "Oponente") 
#p3 <- subset(p2, Encontro == "Walking") #enquanto não tem pesca
p3 <- p3[!duplicated(p3), ]

p3[-1][p3[-1] < 0] <- 0 #transformar negativo em 0

local<-reshape2::dcast(p3, Rota ~ Pokémon, value.var = "N.0", fun = NULL)
local=data.frame(local, row.names=1)
local <- local %>%
  mutate_all(~ ifelse(. > 0, 1, 0))

d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) #method 1Rota is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
plot(hc, labels=local$ID)    # plot the dendrogram

```
Os Pokémons mais representativos
```
num_grupos_automatico <- elbow_point
grupos <- cutree(hc, k = num_grupos_automatico)
local$Grupo <- grupos
media_por_pokemon <- aggregate(. ~ Grupo, data = local, mean)

pokemons_mais_representativos <- colnames(media_por_pokemon)[-1]  # Excluir a coluna Grupo
for (i in 1:num_grupos_automatico) {
  cat("Grupo", i, ": Pokémon mais representativo -", pokemons_mais_representativos[i], "\n")
}

dados_longos <- reshape2::melt(media_por_pokemon, id.vars = "Grupo", variable.name = "Pokemon", value.name = "Media")
#dados_longos <- dados_longos %>% filter(Media > 0.5)

ggplot(dados_longos, aes(x = Pokemon, y = Media, fill = factor(Grupo))) +
  geom_bar(stat = "identity") +
  labs(title = "Média de Presença de Pokémon por Grupo",
       x = "Pokémon",
       y = "Média de Presença") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

ggsave(width = 20, height = 13, device = "pdf", filename = "2024_5_grupos")


```
Cluster de similaridade de Jaccard dos oponentes pelos pokémons treinados
```
pacman::p_load("ade4", "NbClust")

p2 <- pbase2
p2 <- subset(p2, !is.na(Tag_op)) 
#p3 <- subset(p2, T.Treinador == "NPC") 
p3 <- subset(p2, T.Encontro == "4") 
#p3 <- subset(p2, Oponente == "Oponente") 
#p3 <- subset(p2, Encontro == "Walking") #enquanto não tem pesca
#p3 <- p3[!duplicated(p3), ]

p3[-1][p3[-1] < 0] <- 0 #transformar negativo em 0

local<-reshape2::dcast(p3, Tag_op ~ Pokémon, value.var = "N.0", fun = sum)
local=data.frame(local, row.names=1)
local <- local %>%
  mutate_all(~ ifelse(. > 0, 1, 0))

d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) #method 1Rota is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
plot(hc, labels=local$ID)    # plot the dendrogram

```
Cluster de similaridade de pokémons treinados por jogador
```
pacman::p_load("ade4", "NbClust")

p2 <- pbase2
p2 <- subset(p2, !is.na(Treinador)) 
p3 <- subset(p2, T.Treinador == "Jogador") 
p3 <- subset(p2, Jogo > "0") 
#p3 <- subset(p2, Oponente == "Oponente") 
#p3 <- subset(p2, Encontro == "Walking") #enquanto não tem pesca
#p3 <- p3[!duplicated(p3), ]

p3[-1][p3[-1] < 0] <- 0 #transformar negativo em 0

local<-reshape2::dcast(p3, Treinador ~ Pokémon, value.var = "N.0", fun = sum)
local=data.frame(local, row.names=1)

local <- local %>%
  mutate_all(~ ifelse(is.na(.), 0, ifelse(. > 0, 1, 0)))

d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) #method 1Rota is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
plot(hc, labels=local$ID)    # plot the dendrogram

```
Os Pokémons mais representativos
```
grupos <- cutree(hc, k = 8)
local$Grupo <- grupos
media_por_pokemon <- aggregate(. ~ Grupo, data = local, mean)

pokemons_mais_representativos <- colnames(media_por_pokemon)[-1]  # Excluir a coluna Grupo
for (i in 1:num_grupos_automatico) {
  cat("Grupo", i, ": Pokémon mais representativo -", pokemons_mais_representativos[i], "\n")
}

dados_longos <- reshape2::melt(media_por_pokemon, id.vars = "Grupo", variable.name = "Pokemon", value.name = "Media")
#dados_longos <- dados_longos %>% filter(Media > 0.5)

ggplot(dados_longos, aes(x = Pokemon, y = Media, fill = factor(Grupo))) +
  geom_bar(stat = "identity") +
  labs(title = "Média de Presença de Pokémon por Grupo",
       x = "Pokémon",
       y = "Média de Presença") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

ggsave(width = 20, height = 13, device = "pdf", filename = "2025_3_grupos_trenador")

```
## PCA

PCA para identificar a paisagem mais importante por família de Pokémon
```
pacman::p_load(ggfortify, cluster)

p2 <- Data
p2 <- subset(p2, T.Treinador == "Jogador") 
p2 <- subset(p2, !is.na(Local)) #tirar n/a da ano

#p2 <- subset(p2, Grupo == "Mastofauna")

local<-reshape2::dcast(p2, Paisagem ~ Linha.E, value.var = "Total.dex", fun.aggregate = length) #sum ou NULL
#local[is.na(local)] <- 0
local=data.frame(local, row.names=1)
#local[local>0]<-1

pca_res <- prcomp(local, scale. = TRUE)  #TRUE der errado verificar as paisagens. 
#autoplot(pca_res)

local<-reshape2::dcast(p2, Paisagem + Local ~ Linha.E, value.var = "Total.dex", fun.aggregate = length) #sum ou NULL
pca <-autoplot(pca_res, data = local, colour = 'Local', label = TRUE, label.size = 4, 
         frame = TRUE, frame.type = NULL, frame.color = 'Local', #ou frame.type = 't'
         loadings = TRUE, loadings.colour = 'blue',loadings.label = TRUE, loadings.label.size = 3) +                   
         #scale_color_tq() + scale_fill_tq() + 
         theme_minimal() +
         theme(legend.position = "none")  

pca

#ggsave(width = 20, height = 10, device = "pdf", filename = "2024_10_PCA", plot = pca)
#path = "/home/user/Área de Trabalho/Serviços/ES - Rio Bananal/2021_03_03_Grancol/R"

```

PCA para identificar a paisagem mais importante por Pokémon
```

pacman::p_load(ggfortify, cluster)

p2 <- Data
p2 <- subset(p2, T.Treinador == "Jogador") 
p2 <- subset(p2, !is.na(Paisagem)) #tirar n/a da ano

#p2 <- subset(p2, Grupo == "Mastofauna")

local<-reshape2::dcast(p2, Pokémon ~ Local, value.var = "Total.dex", fun.aggregate = length) #sum/length ou NULL
#local[local <= 5z] <- NA
#local[is.na(local)] <- 0
local=data.frame(local, row.names=1)
#local[local>0]<-1

pca_res <- prcomp(local, scale. = TRUE)  #TRUE der errado verificar as paisagens. 
#autoplot(pca_res)

local<-reshape2::dcast(p2, Pokémon + Reino ~ Local, value.var = "Total.dex", fun.aggregate = sum) #sum ou NULL

pca <-autoplot(pca_res, data = local, colour = 'Reino', label = TRUE, label.size = 4, 
         #frame = TRUE, frame.type = NULL, frame.color = 'Grupo', #ou frame.type = 't'
         loadings = TRUE, loadings.colour = 'blue',loadings.label = TRUE, loadings.label.size = 3) +                   
         theme_minimal() 

pca

#ggsave(width = 20, height = 10, device = "pdf", filename = "2024_10_PCA2", plot = pca)
#path = "/home/user/Área de Trabalho/Serviços/ES - Rio Bananal/2021_03_03_Grancol/R"



```
### Análises eploratórias

## Rota

Capturar por Tempo
``` 
pacman::p_load(ggside, stringr) #, tidyverse,tidyquant)

p2 <- Data
p3 <- p2 %>% filter(str_detect(T.Treinador, "Jogador"))
p4 <- p2 %>% filter(str_detect(T.Treinador, "NPC")) 
p3 <- rbind(p3,p4)
#p3 <- subset(p3, Treinador == "Samuel") #escolher artista
p3 <- subset(p3, T.Encontro!="4") #retirar artista
p3<-unique(p3)

p4 <- p3
#p4 <- subset(p4,Tipo!="Grass") 

#p4 <- p4 %>%  subset(Total.Dex > 0.55)

ggplot(p4, aes(x = Data, y = Fase)) + 
  geom_point(aes(colour = Treinador, size = N.0, shape = Contato), alpha = 0.6) + 
  scale_shape_manual(values = 0:10) +
  scale_size(range = c(5, 18), name = "Nível") +
  geom_smooth(method = lm,se = FALSE, alpha = 0.6, aes(colour = Treinador), method.args = list(iter.max = 1000)) +  #method = lm ou loess
  #geom_xsideboxplot(aes(fill = Treinador),alpha = 0.5) +
  geom_xsidedensity(aes(y = after_stat(count), group = Treinador, fill = Treinador),alpha = 0.5, size = 0.5, position = "stack", outline.type = "full") + 
  geom_ysideboxplot(aes(fill = Treinador),alpha = 0.5) + 
  stat_ellipse(geom="polygon", aes(fill = Treinador), alpha = 0.2, show.legend = TRUE,level = 0.25) + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  scale_fill_manual(values = rainbow(length(unique(p4$Treinador)))) +
  theme_minimal() 

#ggsave(width = 20, height = 13, device = "pdf", filename = "2025_3_tempo")

```
Destaque gráfico lateral
```
ggplot(p4, aes(x = Data, y = Pokémon, colour = Treinador)) +
  #geom_boxplot() +
  #geom_smooth(aes(color = Treinador), se = TRUE, method = "loess") +
  stat_density(aes(y = after_stat(count), fill = Treinador), alpha = 0.5, size = 1, position = "stack") + #position = fill
  scale_fill_manual(values = rainbow(length(unique(p4$Treinador)))) +
  theme_minimal() +
  labs(title = "Boxplot temporal para o coletor",
       subtitle = "Acumulação",
       x = "Tempo",
       y = "Treinador") +
  theme(ggside.panel.scale.x = 0.2,
    ggside.panel.scale.y = 0.2,
    legend.position = "bottom",  # Posiciona a legenda à direita para melhor clareza
    axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos do eixo X para melhor leitura
  
#ggsave("2025_3_overlap2.png",width = 12, height = 8, dpi = 600)

```
Destque para as trilhas
```
p4 <- data.frame(p4)

ggplot(p4, aes(x = reorder(Rota, Fase), y = Data, colour = Treinador)) + 
  #geom_boxplot(aes(full = Treinador)) +
  geom_point(aes(size = N.0, shape = Contato), alpha = 0.4) + 
  scale_shape_manual(values = 0:10) +
  scale_size(range = c(5, 18), name = "Nível") +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  labs(x="Rota") +
  scale_fill_manual(values = rainbow(length(unique(p4$Treinador)))) +
  theme_minimal() + coord_flip()

ggsave(width = 20, height = 13, device = "pdf", filename = "2025_3_tempo2")
```
## Adversários
Adversários vencidos
```
pacman::p_load(ggside, stringr) #, tidyverse,tidyquant)

p3 <- subset(pbase2, !is.na(Ano))
p3 <- subset(p3, !is.na(Mês))
p3 <- subset(p3, !is.na(Dia))

Data <- p3 %>% 
  select(Ano,Mês,Dia) %>% 
  mutate(Data = make_date(Ano,Mês,Dia))
Data <- data.frame(p3,Data)

p2 <- Data
p3 <- p2 %>% filter(str_detect(T.Encontro, "5")) 

p4 <- p3
#p4 <- subset(p4,Tipo!="Grass") 

#p4 <- p4 %>%  subset(Fator > 75)

ggplot(p4, aes(x = N.0, y = Tag_op)) + 
  geom_jitter(aes(size = Atributo.T, color = Treinador), alpha = 0.6) + 
  scale_shape_manual(values = 0:10) +
  geom_boxplot(alpha = 0.2) +
  scale_size(range = c(1, 11), name = "Atributo total") +
  labs(title=" ", subtitle="",y="Tipo de treinador",x="Nível do Pokémon", caption="", shape = "", colour = "Treinador", size = "Atributo total") +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  scale_fill_manual(values = rainbow(length(unique(p4$Treinador)))) +
  theme_minimal() 

#ggsave(width = 20, height = 13, device = "pdf", filename = "2025_3_adversários")
```

## Pókemns

Agora, vamos ver os pokémons filogenéticamente

```
pacman::p_load(viridis) #, tidyverse,tidyquant)

p2 <- pbase2
p3 <- p2 %>% filter(str_detect(T.Encontro, "0")) 

p4 <- p3

ggplot(p4, aes(x = Atributo.T+N.0*6, y = Filo)) + 
  geom_jitter(aes(size = N.0, color = Classe), alpha = 0.2) + 
  facet_grid(Reino~., scales = "free_y", space = "free_y") + 
  geom_boxplot(aes(fill = Classe), alpha = 0.6) +
  scale_size(range = c(1, 11), name = "Atributo total") +
  labs(title=" ", subtitle="",y="Reino Pókemon",x="Atributos", caption="", shape = "", colour = "Filo Pókemon", fill = "Filo Pókemon", size = "Nível do Pókemon") +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  scale_fill_manual(values = rainbow(length(unique(p4$Classe)))) +
  theme_minimal() 

#ggsave(width = 20, height = 13, device = "pdf", filename = "2024_5_filo")
``` 
MST (Minimum Spanning Tree): É uma técnica de análise de rede que encontra a árvore de menor custo que conecta todos os vértices de um grafo ponderado. Em biogeografia, a MST pode ser usada para identificar padrões de conectividade entre diferentes áreas geográficas com base em dados de distância ou similaridade.

No gŕafico, as linhas representas as comunidades com menor cursto de coenxão e os pontos sem linhas as comunidades isoladas.
```
pacman::p_load("ggplot2", "spaa", "recluster", "analogue", "ape", "vegan")

p2<-pbase2
p3 <- subset(p2, T.Encontro == "1") #escolher Pokémon
p3 <- subset(p3, T.Treinador == "Jogador") #escolher Pokémon

#p2 <- subset(p3, !is.na(Coletor_tag))
local<-reshape2::dcast(p3, Rota ~ Pokémon, value.var = "Total.dex",fun.aggregate = NULL)
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
E Pokémons

```
pacman::p_load("ggplot2", "spaa", "recluster", "analogue", "ape", "vegan")

p2<-pbase2
p3 <- subset(p2, T.Encontro == "0") #ou 1
p3 <- subset(p3, T.Treinador == "Selvagem") #ou Jogador

#p2 <- subset(p3, !is.na(Coletor_tag))
local<-reshape2::dcast(p3, Linha.E ~ Rota, value.var = "N.0",fun.aggregate = NULL)
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

Tabale de atributos e golpes

``` 
planilhatotal <- read_excel("2023_03_11_pokemon.xlsx", sheet = 7, .name_repair = "minimal")


#head(planilhatotal)
```
E filtrar e montar a pespectiva de data.
```
p2 <- subset(planilhatotal, !is.na(Região)) #tirar n/a da ano
p2 <- subset(p2, !is.na(Número)) #tirar n/a da pontos
pbase2 <- p2 #sem data

p2 <- subset(p2, !is.na(Ano))
p3 <- subset(p2, !is.na(Mês))
p3 <- subset(p3, !is.na(Dia))
pbase <- p3

Data <- p3 %>% 
  select(Ano,Mês,Dia) %>% 
  mutate(Data = make_date(Ano,Mês,Dia))
Data <- data.frame(p3,Data)

```
Ganho e acúmulo de pontos por data
```
p2 <- Data
p2$Data <- as.Date(p2$Data)

#p2 <- subset(p2, !(Treinador %in% c("Felipe", "Vinícius")))
p2 <- subset(p2, !(T.Treinador %in% c("Golpe")))
p2 <- subset(p2, Jogo == 1)
#p2 <- subset(p2, Treinador == "Daniel")

p2 <- p2[order(p2$Data),]
p2 <- transform(p2, acumulado = ave(Atributo.T, Treinador, Nome, FUN = cumsum))

ggplot(p2, aes(x = Data, y = acumulado, color = Treinador, group = interaction(Treinador, Nome))) +
  geom_line() +
  geom_text_repel(data = subset(p2, !duplicated(paste(Treinador, Pokémon))), aes(label = Nome), vjust = -0.5, nudge_y = 0.5, check_overlap = TRUE) +
  labs(title = "Acúmulo de atributos por data",
       x = "Data", y = "Acúmulo do Atributo") +
  #facet_wrap(~Treinador, scales = "free_x", ncol = 4) +  # 'ncol' define o número de colunas no grid
  scale_color_manual(values = rainbow(length(unique(p2$Treinador)))) +  # Definir cores com base nos treinadores
  theme_minimal() +
  theme(legend.position = "bottom")  # Posicionar a legenda na parte inferior

#ggsave(width = 20, height = 10, device = "pdf", filename = "2025_3_tratotal")
```
Por treinador
```
p2 <- Data
p2$Data <- as.Date(p2$Data)

#p2 <- subset(p2, !(Treinador %in% c("Felipe", "Vinícius")))
p2 <- subset(p2, !(T.Treinador %in% c("Golpe")))
#p2 <- subset(p2, Jogo == 1)
p2 <- subset(p2, Treinador == "Daniel")

p2 <- p2[order(p2$Data),]
p2 <- transform(p2, acumulado = ave(Atributo.T, Linha.E, Nome, FUN = cumsum))

ggplot(p2, aes(x = Data, y = acumulado, color = Linha.E, group = interaction(Linha.E, Nome))) +
  geom_line() +
  geom_text_repel(data = subset(p2, !duplicated(paste(Linha.E, Pokémon))), aes(label = Nome), vjust = -0.5, nudge_y = 0.5, check_overlap = TRUE) +
  labs(title = "Acúmulo de atributos por data",
       x = "Data", y = "Acúmulo do Atributo") +
  #facet_wrap(~Treinador, scales = "free_x", ncol = 4) +  # 'ncol' define o número de colunas no grid
  scale_color_manual(values = rainbow(length(unique(p2$Linha.E)))) +  # Definir cores com base nos treinadores
  theme_minimal() +
  theme(legend.position = "none")  # Posicionar a legenda na parte inferior

ggsave(width = 20, height = 10, device = "pdf", filename = "2025_3_dan")
```

Escala de força
```
p2 <- pbase

p3 <- subset(p2, T.Treinador!="Golpe") #retirar artista
p3 <- subset(p3, Jogo!="10") #retirar artista
p3 <- subset(p3, Jogo!="5") #retirar artista
p4 <- p3

local<-reshape2::dcast(p4, Nome + Treinador + Linha.E ~ Região, value.var = "Atributo.T", fun.aggregate = sum) #sum

ggplot(local, aes(x = reorder(Treinador, Kanto), y = Kanto, fill= Nome)) +
  geom_bar(stat = "identity") +
  labs(title = "Força dos Pokémon",
       x = "Nome do Pokémon",
       y = "Soma") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = rainbow(length(unique(local$Nome))))  # Escolher cores diferentes para cada Pokémon

ggsave(width = 20, height = 10, device = "pdf", filename = "2025_3_treinadoforça")
```
Um gráfico de nuvem

```
install.packages("tm")
install.packages("xml2")
install.packages("SnowballC")

pacman::p_load(readr, tm, dplyr, wordcloud, wesanderson, SnowballC)

p2 <- pbase

p2 <- subset(p2, T.Treinador == "Base")


words <- p2 %>% 
  select(Pokémon)
str(words)  
words <- as.character(words)       

word.corpus <- Corpus(VectorSource(words)) 

#word.corpus<-word.corpus %>%
  tm_map(removePunctuation) %>% ##eliminar pontuacao
  tm_map(removeNumbers) %>% #sem numeros
  tm_map(stripWhitespace) #sem espacos
  
word.corpus<-word.corpus %>%
  tm_map(content_transformer(tolower)) 

#word.corpus <- tm_map(word.corpus, removeWords, c("the", "vol", "dos", "para", "das"))
    
word.corpus <- tm_map(word.corpus, stemDocument)  

word.counts <- as.matrix(TermDocumentMatrix(word.corpus))
word.freq <- sort(rowSums(word.counts), decreasing = TRUE)
head(word.freq)
         
set.seed(32)  #be sure to set the seed if you want to reproduce the same again

wordcloud(words = names(word.freq), freq = word.freq, scale = c(4, 0.3), max.words = 50, 
    random.order = FALSE, color = wes_palette("Darjeeling1"), rot.per = 0.7)    

```
