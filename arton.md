# Apresentação
Análise das campanhas de RPG de Arton - Tormenta RPG

## Início
Indicar as pastas corretas.
```
getwd()
#setwd("/home/valev/Área de Trabalho/R/RPG/arton") 
setwd("/home/victor-vale/Área de trabalho/R/RPG")

gc()  # Executa o garbage collector
```

Agora baixar e ler alguns pacotes básicos.


# Todos os pacotes do seu original (exceto graphics que é base)

```
pacotes <- c("magrittr", "dplyr", "reshape2", "ggplot2", "ggrepel",
             "lubridate", "gghighlight", "forcats", "iNEXT", "tidyr",
             "tibble", "vegan", "ggside")

# Instalar faltantes
novos <- pacotes[!pacotes %in% installed.packages()]
if(length(novos) > 0) install.packages(novos)

# Carregar todos
invisible(lapply(pacotes, require, character.only = TRUE))

```
Adicionar a planilha
```

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
pbase <- p2

p2 <- subset(pbase, Grupo == "G5.1") 
#p2 <- subset(p2, Grupo == "G5.2") 


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

#E outra data

p2 <- subset(p2, !is.na(Jogador)) #tirar n/a da ano
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
p2 <- Data2
p3 <- subset(p2, N_categoria == "20") 
p3 <- subset(p3, !is.na(Combate))

df <- p3 %>% arrange(Data2)

df <- df %>%
  group_by(Personagem) %>%
  mutate(Acumulado = cumsum(ifelse(is.na(Quantidade), 0, Quantidade)))

# Criando o gráfico
R <- ggplot(df, aes(x = Data2, y = Acumulado, color = Personagem)) +
  geom_line() +
  geom_jitter(aes(size = Quantidade)) +
  labs(title = '',
       x = 'Data do Combate',
       y = 'Número de Adversários',
       size = "Grupo de adversários") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7, 8))  # Até 9 formas distintas

R

# Salvando o gráfico
ggsave(filename = "2025_10_Acum_spps.pdf", plot = R, width = 20, height = 10, device = "pdf")


```
## PCA

PCA para identificar a paisagem mais importante por família de Pokémon
```
pacman::p_load(dplyr, ggplot2, ggfortify, ggrepel)


p3 <- tidyr::separate_rows(p2, Esfera, sep = "/")
p3 <- tidyr::separate_rows(p3, Patrono, sep = "/")

#p3 <- p3 %>% filter(Jogador == "Afonso A.")

p3 <- p3 %>%
  filter(!is.na(Personagem), 
         !is.na(Patrono), 
         !is.na(Esfera), 
         N_categoria == "20")


p3 <- p3 %>% filter(Personagem %in% c("Bryn", "Artemisia Asteracea"))
  
#p3 <- p3 %>% filter(!Personagem %in% c("Bryn", "Artemisia Asteracea"))

# Reshaping do dataframe
local <- reshape2::dcast(p3, Patrono + Personagem ~ Esfera, 
                         value.var = "N_categoria", 
                         fun.aggregate = sum,
                         na.rm = TRUE)

local <- local[complete.cases(local), ]

# PCA
local_numeric <- local %>% select(where(is.numeric))
pca_res <- prcomp(local_numeric, scale. = TRUE)

# Preparar dados
pca_scores <- as.data.frame(pca_res$x)
pca_scores$Personagem <- local$Personagem
pca_scores$Patrono <- local$Patrono

# Polígonos por personagem
hull_por_personagem <- pca_scores %>%
  group_by(Personagem) %>%
  slice(chull(PC1, PC2)) %>%
  ungroup()

# Definir mais shapes manualmente (até 25 shapes)
shapes_custom <- c(16, 17, 15, 18, 8, 3, 4, 0, 1, 2, 5, 6, 7, 9, 10, 11, 12, 13, 14, 19, 20, 21, 22, 23, 24, 25)

# Visualização com mais shapes
pca <- ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  # Polígonos
  geom_polygon(data = hull_por_personagem, 
               aes(color = Personagem, group = Personagem), 
               fill = NA, size = 1, alpha = 0.8) +  # fill = NA remove o preenchimento 
  # Pontos com muitos shapes diferentes
  geom_jitter(aes(color = Personagem, shape = Patrono), size = 4, alpha = 0.8) +
  # Labels simples
  geom_text_repel(aes(label = Patrono, color = Personagem), 
                  size = 3, max.overlaps = 20, show.legend = FALSE) +
  # Loadings simplificados
  geom_segment(data = as.data.frame(pca_res$rotation * 3),
               aes(x = 0, y = 0, xend = PC1, yend = PC2)) +
  geom_text(data = as.data.frame(pca_res$rotation * 3),
            aes(x = PC1, y = PC2, label = rownames(pca_res$rotation)), size = 3) +
  # Escala de shapes personalizada
  scale_shape_manual(values = shapes_custom) +
  theme_minimal() +
  labs(title = "PCA - Patronos por Personagem",
       subtitle = "N_categoria = 20",
       shape = "Patrono",
       color = "Personagem",
       fill = "Personagem")

pca

```
O contrário

```
pacman::p_load(dplyr, ggplot2, ggfortify, ggrepel)

p3 <- tidyr::separate_rows(p2, Esfera, sep = "/")
p3 <- tidyr::separate_rows(p3, Patrono, sep = "/")

#p3 <- p3 %>% filter(Jogador == "Afonso A.")

p3 <- p3 %>%
  filter(!is.na(Personagem), 
         !is.na(Patrono), 
         !is.na(Esfera), 
         N_categoria == "20")

p3 <- p3 %>% filter(Personagem %in% c("Bryn", "Artemisia Asteracea"))
  
#p3 <- p3 %>% filter(!Personagem %in% c("Bryn", "Artemisia Asteracea"))
  
# Reshaping do dataframe
local <- reshape2::dcast(p3, Esfera + Personagem ~ Patrono, 
                         value.var = "N_categoria", 
                         fun.aggregate = sum,
                         na.rm = TRUE)

local <- local[complete.cases(local), ]

# PCA
local_numeric <- local %>% select(where(is.numeric))
pca_res <- prcomp(local_numeric, scale. = TRUE)

# Preparar dados
pca_scores <- as.data.frame(pca_res$x)
pca_scores$Personagem <- local$Personagem
pca_scores$Esfera <- local$Esfera

# Polígonos por personagem
hull_por_personagem <- pca_scores %>%
  group_by(Personagem) %>%
  slice(chull(PC1, PC2)) %>%
  ungroup()

# Definir mais shapes manualmente (até 25 shapes)
shapes_custom <- c(16, 17, 15, 18, 8, 3, 4, 0, 1, 2, 5, 6, 7, 9, 10, 11, 12, 13, 14, 19, 20, 21, 22, 23, 24, 25)

# Visualização com mais shapes
pca <- ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  # Polígonos
  geom_polygon(data = hull_por_personagem, 
               aes(color = Personagem, group = Personagem), 
               fill = NA, size = 1, alpha = 0.8) +  # fill = NA remove o preenchimento 
  # Pontos com muitos shapes diferentes
  geom_jitter(aes(color = Personagem, shape = Esfera), size = 4, alpha = 0.8) +
  # Labels simples
  geom_text_repel(aes(label = Esfera, color = Personagem), 
                  size = 3, max.overlaps = 20, show.legend = FALSE) +
  # Loadings simplificados
  geom_segment(data = as.data.frame(pca_res$rotation * 3),
               aes(x = 0, y = 0, xend = PC1, yend = PC2)) +
  geom_text(data = as.data.frame(pca_res$rotation * 3),
            aes(x = PC1, y = PC2, label = rownames(pca_res$rotation)), size = 3) +
  # Escala de shapes personalizada
  scale_shape_manual(values = shapes_custom) +
  theme_minimal() +
  labs(title = "PCA - Esfera por Personagem",
       subtitle = "N_categoria = 20",
       shape = "Esfera",
       color = "Personagem",
       fill = "Personagem")

pca

```
Contelação de personagens

```
pacman::p_load(patchwork)


p3 <- tidyr::separate_rows(p2, Esfera, sep = "/")
p3 <- tidyr::separate_rows(p3, Patrono, sep = "/")

p3 <- p3 %>%
  filter(!is.na(Personagem), 
         !is.na(Patrono), 
         !is.na(Esfera), 
         N_categoria == "20")

# Função para criar gráficos PCA
criar_pca_clean <- function(data, var_setas, titulo) {
  local <- reshape2::dcast(data, Personagem ~ get(var_setas), 
                          value.var = "N_categoria", 
                          fun.aggregate = sum,
                          na.rm = TRUE)
  
  local <- local[complete.cases(local), ]
  local_numeric <- local %>% select(where(is.numeric))
  
  if(nrow(local_numeric) < 2) return(NULL)
  
  pca_res <- prcomp(local_numeric, scale. = TRUE)
  pca_scores <- as.data.frame(pca_res$x)
  pca_scores$Personagem <- local$Personagem
  
  ggplot(pca_scores, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = Personagem), size = 2.5) +
    geom_text(aes(label = Personagem), size = 2.8, check_overlap = TRUE, 
              vjust = -0.8) +
    geom_segment(data = as.data.frame(pca_res$rotation * 2.8),
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.08, "cm"))) +
    geom_text(data = as.data.frame(pca_res$rotation * 3),
              aes(x = PC1, y = PC2, label = rownames(pca_res$rotation)),
              size = 2.5) +
    theme_bw(base_size = 9) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none",
      panel.grid = element_line(color = "gray95"),
      plot.margin = margin(20, 20, 20, 20)  # Aumenta as margens
    ) +
    coord_cartesian(clip = "off") +  # IMPEDE QUE TEXTO SEJA CORTADO
    labs(title = titulo, x = "PC1", y = "PC2")
}

# Criar gráficos
q1 <- criar_pca_clean(p3, "Patrono", "Personagens × Deuses")
q2 <- criar_pca_clean(p3, "Esfera", "Personagens × Esferas")

# Mostrar gráficos
if(!is.null(q1) && !is.null(q2)) {
  q1 + q2
} else if(!is.null(q1)) {
  q1
} else if(!is.null(q2)) {
  q2
}

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
         #Equipado == "1", 
         #T_Atributo != "Base", 
         Ano_A >= 1330) %>%
  distinct(Evento, .keep_all = TRUE)

p4 <- p3
p4 <- p4 %>% arrange(Data)
p4 <- p4 %>% arrange(Data2)

ggplot(p4, aes(x = Data2, y = Data)) + 
  geom_jitter(aes(colour = Personagem, shape = Categoria), alpha = 0.6) + 
  geom_smooth(method = loess, se = FALSE, alpha = 0.6, aes(colour = Personagem)) + 
  scale_shape_manual(values = 0:10) +
  scale_size(range = c(5, 18), name = "Nível") +
  scale_x_date(date_labels = "%B/%Y") +
  scale_y_date(date_labels = "%B/%Y") +
  #stat_ellipse(geom="polygon", aes(fill = Personagem), alpha = 0.2, show.legend = TRUE, level = 0.25) + 
  # Adicionando boxplots laterais
  geom_ysideboxplot(aes(fill = Personagem), alpha = 0.5) + 
  #geom_xsidedensity(aes(y = after_stat(count), group = Personagem, fill = Personagem), alpha = 0.5, size = 0.5, position = "stack", outline.type = "full") + 
  geom_xsidebar(aes(fill = Personagem), 
              position = "stack", 
              stat = "count",
              alpha = 0.5, 
              orientation = "x",
              width = 1) +  # Barras verticais 
  labs(x = "Data jogo", y = "Data Arton") +  
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14)) +
  scale_fill_manual(values = rainbow(length(unique(p4$Personagem)))) +
  theme_minimal()
  
  

ggsave(width = 20, height = 13, device = "pdf", filename = "2025_10_tempo")

```
Destaque gráfico lateral
```

p2 <- Data2
colnames(p2) <- make.names(colnames(p2), unique = TRUE)

# Filtrando os dados
p3 <- p2 %>%
  filter(!is.na(Personagem), 
         N_categoria == "20") %>%
  distinct(Evento, .keep_all = TRUE)

ggplot(p3, aes(x = Data2, y = T_Atributo, colour = Personagem)) +
  #geom_boxplot() +
  #geom_smooth(aes(color = Personagem), se = TRUE, method = "loess") +
  stat_density(aes(y = after_stat(count), fill = Personagem), alpha = 0.5, size = 1, position = "stack") + #position = fill
  scale_fill_manual(values = rainbow(length(unique(p4$Personagem)))) +
  scale_x_date(date_labels = "%B/%Y") +
  theme_minimal() +
  labs(title = "",
       subtitle = "",
       x = "Tempo",
       y = "Personagem") +
  theme(ggside.panel.scale.x = 0.2,
    ggside.panel.scale.y = 0.2,
    legend.position = "bottom",  # Posiciona a legenda à direita para melhor clareza
    axis.text.x = element_text(angle = 45, hjust = 1))  # Rotaciona os rótulos do eixo X para melhor leitura
  
ggsave("2025_10_overlap2.png",width = 12, height = 8, dpi = 600)

```
Ano do jogo
```
library(ggside)

p2 <- Data
colnames(p2) <- make.names(colnames(p2), unique = TRUE)

# Filtrar dados
p3 <- p2 %>%
  filter(!is.na(Personagem), 
         N_categoria == "20",
         Ano_A >= 1330) %>%
  distinct(Evento, .keep_all = TRUE) %>%
  mutate(Data = as.Date(Data)) %>%
  filter(!is.na(Data))

# Gráfico com barra lateral de densidade
if(nrow(p3) > 0) {
  ggplot(p3, aes(x = Data, fill = Personagem)) +
    geom_bar(position = "dodge", alpha = 0.8) +
    geom_xsidedensity(aes(y = after_stat(count), fill = Personagem), 
                      alpha = 0.5, position = "stack") +  # Densidade no topo
    scale_fill_manual(values = rainbow(length(unique(p3$Personagem)))) +
    scale_x_date(date_labels = "%B/%Y") +
    labs(x = "Data", y = "Número de Eventos") +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1),
          ggside.panel.scale.x = 0.3)  # Ajusta tamanho da barra lateral
}  
        
```
E de pizza
```
p2 <- Data2
colnames(p2) <- make.names(colnames(p2), unique = TRUE)

# Filtrando os dados
p3 <- p2 %>%
  filter(!is.na(Personagem), 
         N_categoria == "20") %>%
  distinct(Evento, .keep_all = TRUE)
  
# Calcular a proporção de cada Personagem
p3_pie <- p3 %>%
  group_by(Personagem) %>%
  summarise(Frequencia = n()) %>%
  mutate(Proporcao = Frequencia / sum(Frequencia),
         Label = paste0(Personagem, " (", round(Proporcao*100, 1), "%)"))

# Gráfico de pizza
ggplot(p3_pie, aes(x = "", y = Proporcao, fill = Personagem)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = rainbow(length(unique(p3_pie$Personagem)))) +
  geom_text_repel(aes(label = Label), position = position_stack(vjust = 0.5), size = 3) +
  theme(legend.position = "none") +
  labs(title = "Distribuição de Personagens", subtitle = "Categoria 20")

ggsave("2025_10_pizza.png",width = 12, height = 8, dpi = 600)

```


Verificar habilidades
```

p2 <- subset(planilhatotal, !is.na(Jogador)) #tirar n/a da ano
p2 <- subset(p2, Grupo == "G5.1") 
colnames(p2) <- make.names(colnames(p2), unique = TRUE)


# Filtrar dados
p3 <- p2 %>%
  filter(!is.na(Personagem), 
         N_categoria == "20",
         Categoria == "Combate",
         T_Atributo != "Dano")

# Gráfico
# Definir 10 shapes diferentes
shapes_personalizados <- c(16, 17, 15, 18, 8, 3, 4, 0, 1, 2, 5, 6, 7, 9, 10)

ggplot(p3, aes(x = N_Hab, y = Bonus, color = Personagem, shape = Classe)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text_repel(aes(label = D_bonus), size = 2.2, 
                  box.padding = 0.3, max.overlaps = 30) +
  facet_wrap(~ T_bonus, ncol = 1, scales = "free_y") +
  labs(x = "Nível da Habilidade", y = "Habilidade") +
  scale_shape_manual(values = shapes_personalizados) +
  theme_minimal() +
  theme(legend.position = "right",
        strip.text = element_text(face = "bold"))  

```
Itens especiais

```

p2 <- subset(planilhatotal, !is.na(Jogador)) #tirar n/a da ano
p2 <- subset(p2, Grupo == "G5.1") 
colnames(p2) <- make.names(colnames(p2), unique = TRUE)


p3 <- p3 %>%
  group_by(Personagem) %>%
  mutate(label_unico = ifelse(duplicated(D_bonus) | duplicated(D_bonus, fromLast = TRUE), 
                               NA, D_bonus)) %>%
  ungroup()
  
# Filtrar dados
p3 <- p2 %>%
  filter(!is.na(Personagem), 
         N_categoria == "20",
         Categoria == "Item",
         Equipado == 1,
         Classe_I != "Item")  


# Gráfico
# Definir 10 shapes diferentes
shapes_personalizados <- c(16, 17, 15, 18, 8, 3, 4, 0, 1, 2, 5, 6, 7, 9, 10)

ggplot(p3, aes(x = N_Hab, y = Bonus, color = Personagem, shape = Classe_M, size = Quantidade)) +
  geom_point(alpha = 0.7) +
  geom_text_repel(aes(label = D_bonus), size = 2.2, box.padding = 0.3, max.overlaps = 30) +
  facet_wrap(~ Classe_I, ncol = 1, scales = "free_y") +
  labs(x = "Nível da Habilidade", y = "Habilidade") +
  scale_shape_manual(values = shapes_personalizados) +
  theme_minimal() +
  theme(legend.position = "right",
        strip.text = element_text(face = "bold"))
        
        
```
Outros Itens

```

p2 <- subset(planilhatotal, !is.na(Jogador)) #tirar n/a da ano
p2 <- subset(p2, Grupo == "G5.1") 
colnames(p2) <- make.names(colnames(p2), unique = TRUE)

p3 <- p2 %>%
  filter(!is.na(Personagem), 
         N_categoria == "20",
         Categoria == "Item",
         Equipado == 1,
         Classe_I == "Item")  

ggplot(p3, aes(x = N_Hab, y = Bonus, color = Personagem, shape = T_bonus, size = Quantidade)) +
  geom_point(alpha = 0.7) +
  geom_text_repel(aes(label = D_bonus), size = 2.2, 
                  box.padding = 0.3, max.overlaps = 30) +
  #facet_wrap(~ Classe_I, ncol = 1, scales = "free_y") +
  labs(x = "Nível da Habilidade", y = "Habilidade") +
  scale_shape_manual(values = shapes_personalizados) +
  theme_minimal() +
  theme(legend.position = "right",
        strip.text = element_text(face = "bold"))         
       
```

## Tabela de atributos

E filtrar e montar a pespectiva de data.
```
# Filtrando os dados
p2 <- Data2

p3 <- subset(Data2, N_categoria == 20)
#p3 <- p2 %>%   filter(N_categoria >= 20 & N_categoria <= 23)
p3 <- p3[!is.na(p3$Atributos) & p3$Atributos > 0.01, ]

# Ordenando p3 pela data
p3 <- p3[order(p3$Data2),]

# Calculando a variável acumulado
p3 <- transform(p3, acumulado = ave(Atributos, Personagem, FUN = cumsum))

# Criando o gráfico com p3 (onde acumulado foi calculado)
ggplot(p3, aes(x = Data2, y = acumulado, color = Personagem)) +
  geom_line(size = 1) +
  labs(title = "Acúmulo de atributos por data",
       x = "Data", y = "Acúmulo do Atributo") +
  scale_color_manual(values = rainbow(length(unique(p3$Personagem)))) +  # Definir cores com base nos personagens
  theme_minimal() +
  theme(legend.position = "bottom")  # Posicionar a legenda na parte inferior


ggsave(width = 20, height = 10, device = "pdf", filename = "2025_10_tratotal")

```
Um gráfico de nuvem

```
# Carregar pacotes
pacman::p_load(tm, xml2, SnowballC, readr, dplyr, wordcloud, wesanderson, wordcloud2)

# Filtrar dados para remover NAs na coluna Evento
p2 <- pbase %>% 
  filter(!is.na(Evento)) %>%
  #filter(Grupo == "G5") %>%  # Filtra apenas observações onde Grupo é G5
  distinct(Evento, Personagem, .keep_all = TRUE)

# Preparar o corpus de palavras
words <- p2 %>% select(Evento) %>% pull() %>% paste(collapse = " ")  # Junta todos os textos
word.corpus <- Corpus(VectorSource(words)) 

# Transformações no corpus com tratamento de erros
word.corpus <- word.corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation, preserve_intra_word_dashes = TRUE) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("portuguese")) %>%  # Remover stopwords em português
  tm_map(stripWhitespace)

# Criar a matriz de termos e frequências com filtro mínimo
word.counts <- as.matrix(TermDocumentMatrix(word.corpus, 
                          control = list(wordLengths = c(3, Inf))))
word.freq <- sort(rowSums(word.counts), decreasing = TRUE)

# Filtrar palavras muito raras (aparecem menos de 3 vezes)
word.freq <- word.freq[word.freq >= 3]

# Exibir as 20 palavras mais frequentes
head(word.freq, 20)



# OPÇÃO 1: Wordcloud2 interativo (recomendado para muitos termos)
wordcloud2(data.frame(word = names(word.freq), freq = word.freq),
           size = 1.2,
           color = wes_palette("Darjeeling1", n = length(word.freq), type = "continuous"),
           backgroundColor = "white",
           shape = 'circle')
           

# OPÇÃO 2: Wordcloud tradicional com parâmetros otimizados
#png("wordcloud.png", width = 1200, height = 1200, res = 300)  # Maior resolução
set.seed(32)
wordcloud(words = names(word.freq), 
          freq = word.freq, 
          scale = c(5, 0.8),
          min.freq = 3,
          max.words = 150,
          random.order = FALSE,
          colors = wes_palette("Darjeeling1", n = length(word.freq), type = "continuous"),
          rot.per = 0.35,
          fixed.asp = TRUE)
#dev.off()
```
Um mapa local

Por personagem

```

pacman::p_load(ggplot2, dplyr, ggrepel)

p2 <- Data2
colnames(p2) <- make.names(colnames(p2), unique = TRUE)

p3 <- p2 %>%
  filter(!is.na(Reino),
         !is.na(Personagem),
         !is.na(Bairro),
         !is.na(E_2),
         N_categoria %in% c("20","23"),
         N_categoria == "20",
         #Reino == "Zacharov",
         is.finite(Longitude),
         is.finite(Latitude)) %>%
  arrange(Data2)

#p3 <- p3 %>% filter(Jogador == "Vitor D.")
#p3 <- p3 %>% filter(Jogador %in% c("Eloisa C.", "Ricardo P."))

# Subconjunto para rótulos
p3_labels <- p3 %>%
  group_by(Bairro) %>%
  slice(1)

# Gráfico por personagem
ggplot(p3, aes(x = Longitude, y = Latitude, shape = Reino)) + 
  geom_path(aes(colour = Reino, group = Reino), size = 1, alpha = 0.6) + 
  geom_jitter(aes(colour = Reino), size = 4, alpha = 0.6) + 
  scale_color_manual(values = rainbow(length(unique(p3$Reino)))) + 
  geom_text_repel(data = p3_labels, aes(label = Bairro), 
                size = 4, color = "black", box.padding = 0.5,
                max.overlaps = 20) +
  labs(title = "Mapa dos Personagens", 
       x = "Longitude", 
       y = "Latitude") +
  facet_wrap(~ Personagem, drop = TRUE, scales = "free")+ #, ncol = 3
  theme_minimal() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))
  
```
Caminhos

MST (Minimum Spanning Tree): É uma técnica de análise de rede que encontra a árvore de menor custo que conecta todos os vértices de um grafo ponderado. Em biogeografia, a MST pode ser usada para identificar padrões de conectividade entre diferentes áreas geográficas com base em dados de distância ou similaridade.

No gŕafico, as linhas representas as comunidades com menor cursto de coenxão e os pontos sem linhas as comunidades isoladas.
```
pacman::p_load("ggplot2", "spaa", "recluster", "analogue", "ape", "vegan", "dplyr", "reshape2", "ggrepel")

# Carregar e ajustar os dados
p2 <- Data2

p3 <- p2 %>%
  filter(!is.na(Estrutura),
         !is.na(Personagem))

colnames(p2) <- make.names(colnames(p2), unique = TRUE)
#p2 <- subset(p2, Jogador != "Raphael S.")

# Filtrar os dados
p3 <- p2 %>%
  filter(!is.na(Personagem), N_categoria >= 20 & N_categoria <= 20)

# Criar a matriz local e calcular a distância
local <- reshape2::dcast(p3, Personagem ~ Bonus, value.var = "N_categoria", fun.aggregate = NULL) %>%
  na.omit() %>%
  data.frame(row.names = 1)

dist_matrix <- vegdist(local, method = "jaccard")
mst_tree <- mst(dist_matrix)

# Calculando as coordenadas usando cmdscale
mst_coordinates <- cmdscale(dist_matrix)
df_mst_coordinates <- as.data.frame(mst_coordinates)
colnames(df_mst_coordinates) <- c("X", "Y")

# Gerar as arestas da árvore
edges <- which(mst_tree != 0, arr.ind = TRUE)
df_lines <- data.frame(
  X1 = mst_coordinates[edges[, 1], 1],
  Y1 = mst_coordinates[edges[, 1], 2],
  X2 = mst_coordinates[edges[, 2], 1],
  Y2 = mst_coordinates[edges[, 2], 2]
)

# Plotando a MST com ggplot2
ggplot() +
  geom_segment(data = df_lines, aes(x = X1, y = Y1, xend = X2, yend = Y2), color = "blue") +
  geom_point(data = df_mst_coordinates, aes(x = X, y = Y), color = "red") +
  geom_text_repel(
    data = df_mst_coordinates,
    aes(x = X, y = Y, label = rownames(df_mst_coordinates)),
    vjust = -0.5,
    max.overlaps = 25
  ) +
  labs(title = "Minimum Spanning Tree", x = "Proximidade", y = "Proximidade") +
  theme_minimal()



ggsave(width = 20, height = 10, device = "pdf", filename = "2024_10_mst_estrutura")

```
Um mapa total para cada reino
```
pacman::p_load(ggplot2, dplyr, ggrepel)

p2 <- Data2
colnames(p2) <- make.names(colnames(p2), unique = TRUE)

p3 <- p2 %>%
  filter(!is.na(Vila),
         !is.na(Personagem),
         !is.na(Estrutura), 
         N_categoria == "20",
         Reino == "Portsmouth", #Deheon / Yuden / Zacahrov
         !is.na(E_2))

#p3 <- p3 %>% filter(N_categoria %in% c("20"),, between(Latitude, -0.15, 0.15), between(Longitude, -0.15, 0.15))


p3 <- p3 %>% filter(is.finite(Longitude) & is.finite(Latitude))

#p3 <- p3 %>% filter(Jogador == "Afonso A.")
         
# Criando um subconjunto com um único ponto por Estrutura
p3_labels <- p3 %>%
  group_by(Estrutura) %>%
  slice(1)  # Pega apenas o primeiro ponto de cada grupo

# Criando um subconjunto com um único ponto por Estrutura
p3_labels2 <- p3 %>%
  group_by(E_2) %>%
  slice(1)  # Pega apenas o primeiro ponto de cada grupo

p3 <- p3 %>% filter(is.finite(Longitude) & is.finite(Latitude))

# Ordenando os dados pela coluna Data2
p3 <- p3 %>% arrange(Data2)

# Criando o gráfico
ggplot(p3, aes(x = Longitude, y = Latitude, group = Personagem)) + 
  # Linha conectando os pontos
  geom_path(aes(colour = Personagem), size = 1, alpha = 0.6) + 
  # Pontos (Personagens)
  geom_jitter(aes(colour = Personagem, shape = Vila), size = 4, alpha = 0.6) + 
  scale_color_manual(values = rainbow(length(unique(p3$Personagem)))) +  
  scale_shape_manual(values = c(0:20)) +  # Usa 21 símbolos diferentes
  labs(title = "Mapa dos Personagens", 
       x = "Longitude", 
       y = "Latitude") +
  # Rótulos das estruturas (apenas um ponto por Estrutura)
  #geom_label_repel(data = p3_labels, aes(label = Estrutura), size = 4, color = "black", box.padding = 0.5) + 
  # Rótulos das estruturas (apenas um ponto por detalhe)
  geom_text_repel(data = p3_labels, aes(label = Estrutura), 
                size = 4, color = "black", box.padding = 0.5,
                max.overlaps = 25)  + 
  #stat_ellipse(geom="polygon", aes(fill = Personagem), alpha = 0.2, show.legend = TRUE, level = 0.75) + 
  theme_minimal() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))

```
Mapa total
```
p2 <- subset(planilhatotal, !is.na(Jogador)) #tirar n/a da ano
colnames(p2) <- make.names(colnames(p2), unique = TRUE)

p3 <- p2 %>%
  filter(!is.na(Personagem),
         #N_categoria == "20",
         !is.na(Reino))  # Corrigi a condição do filtro

p3 <- p3 %>% filter(is.finite(Longitude) & is.finite(Latitude))

# Ordenando os dados pela coluna Data2
#p3 <- p3 %>% arrange(Data2)

# Criando um subconjunto com um único ponto por Vila
p3_labels <- p3 %>%
  group_by(Vila) %>%
  slice(1)  # Pega apenas o primeiro ponto de cada grupo

ggplot(p3, aes(x = Longitude, y = Latitude, group = Reino)) + 
  # Linha conectando os pontos
  geom_path(aes(colour = Reino), size = 1, alpha = 0.6) + 
  # Pontos (Personagens)
  geom_jitter(aes(colour = Reino, shape = Jogador), size = 4, alpha = 0.6) + 
  scale_color_manual(values = rainbow(length(unique(p3$Reino)))) +  
  labs(title = "Mapa dos Personagens", 
       x = "Longitude", 
       y = "Latitude") +
  # Rótulos das estruturas (apenas um ponto por Vila)
  geom_text_repel(data = p3_labels, aes(label = Vila), 
                size = 4, color = "black", box.padding = 0.5,
                max.overlaps = 25) + 
  scale_shape_manual(values = c(0:20)) + 
  theme_minimal() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))

```
#######################################



E Personagens
```

pacman::p_load("ggplot2", "spaa", "recluster", "analogue", "ape", "vegan", "dplyr", "reshape2", "ggrepel")

# Carregar e ajustar os dados
p2 <- Data2
colnames(p2) <- make.names(colnames(p2), unique = TRUE)

# Filtrar os dados
p3 <- p2 %>%
  filter(!is.na(Personagem), !is.na(Bonus), N_categoria >= 20 & N_categoria <= 23)

# Criar a matriz local e calcular a distância
local <- reshape2::dcast(p3, Personagem ~ Bonus, value.var = "N_categoria", fun.aggregate = NULL) %>%
  na.omit() %>%
  data.frame(row.names = 1)

dist_matrix <- vegdist(local, method = "jaccard")
mst_tree <- mst(dist_matrix)

# Calculando as coordenadas usando cmdscale
mst_coordinates <- cmdscale(dist_matrix)
df_mst_coordinates <- as.data.frame(mst_coordinates)
colnames(df_mst_coordinates) <- c("X", "Y")

# Gerar as arestas da árvore
edges <- which(mst_tree != 0, arr.ind = TRUE)
df_lines <- data.frame(
  X1 = mst_coordinates[edges[, 1], 1],
  Y1 = mst_coordinates[edges[, 1], 2],
  X2 = mst_coordinates[edges[, 2], 1],
  Y2 = mst_coordinates[edges[, 2], 2]
)

# Plotando a MST com ggplot2
ggplot() +
  geom_segment(data = df_lines, aes(x = X1, y = Y1, xend = X2, yend = Y2), color = "blue") +
  geom_point(data = df_mst_coordinates, aes(x = X, y = Y), color = "red") +
  geom_text_repel(
    data = df_mst_coordinates,
    aes(x = X, y = Y, label = rownames(df_mst_coordinates)),
    vjust = -0.5,
    max.overlaps = 25
  ) +
  labs(title = "Minimum Spanning Tree", x = "Comunidade", y = "Comunidade") +
  theme_minimal()


#ggsave(width = 20, height = 10, device = "pdf", filename = "2025_3_mst")
```
Agora o acumualdo por espécie, vitória
```
# Carregar pacotes necessários
pacman::p_load(dplyr, ggplot2)

# Filtrar os dados
#p3 <- Data %>% filter(N_categoria == "20", !is.na(Combate))
p3 <- Data %>% filter(N_categoria == "20", Combate == "Vitória")

# Criar um data frame com a quantidade total de espécies por treinador e oponente
df_ponteiros <- p3 %>%
  filter(Personagem %in% Personagem) %>%
  group_by(Personagem, Oponente, Combate, Classe.op) %>%
  summarise(Total_Quantidade = sum(Quantidade, na.rm = TRUE)) %>%
  ungroup()

df_ponteiros <- df_ponteiros %>% filter(!is.na(Combate) & Combate != "")

# Criar gráfico de barras com os eixos invertidos
ggplot(df_ponteiros, aes(x = Total_Quantidade, y = Oponente, fill = Personagem)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Total_Quantidade),  # Adiciona rótulos com os valores
            position = position_dodge(width = 0.9),  # Garante que os rótulos fiquem alinhados às barras
            vjust = 0.5,  # Ajusta a posição vertical dos rótulos
            hjust = -0.2,  # Ajusta a posição horizontal para evitar sobreposição
            size = 5) +  # Define o tamanho do texto
  labs(title = "Total de Espécies por Oponente e Treinador",
       x = "Total de Espécies Acumuladas",
       y = "Oponente") +
  theme_minimal() +
  facet_wrap(~Classe.op, scales = "free") +  # Cada tipo de combate será um painel separado
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14), 
        legend.position = "bottom") #+  scale_fill_manual(values = rainbow(length(Personagem)))



ggsave(filename = "2025_10_Acum_vit.pdf", width = 20, height = 10, device = "pdf")
```

Excluindo vitória
```
# Carregar pacotes necessários
pacman::p_load(dplyr, ggplot2)

# Filtrar os dados, excluindo "Vitória"
p3 <- Data %>% filter(N_categoria == "20", Combate != "Vitória")

# Criar um data frame com a quantidade total de espécies por treinador e oponente
df_ponteiros <- p3 %>%
  filter(Personagem %in% Personagem) %>%
  group_by(Personagem, Oponente, Combate) %>%
  summarise(Total_Quantidade = sum(Quantidade, na.rm = TRUE)) %>%
  ungroup()

df_ponteiros <- df_ponteiros %>% filter(!is.na(Combate) & Combate != "")

# Criar gráfico de barras com os eixos invertidos
ggplot(df_ponteiros, aes(x = Total_Quantidade, y = Oponente, fill = Personagem)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Total_Quantidade),  # Adiciona rótulos com os valores
            position = position_dodge(width = 0.9),  # Garante que os rótulos fiquem alinhados às barras
            vjust = 0.5,  # Ajusta a posição vertical dos rótulos
            hjust = -0.2,  # Ajusta a posição horizontal para evitar sobreposição
            size = 5) +  # Define o tamanho do texto
  labs(title = "Total de Espécies por Oponente e Treinador",
       x = "Total de Espécies Acumuladas",
       y = "Oponente") +
  theme_minimal() +
  facet_wrap(~Combate, scales = "free") +  # Cada tipo de combate será um painel separado
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14), 
        legend.position = "bottom") #+  scale_fill_manual(values = rainbow(length(Personagem)))



ggsave(filename = "2025_10_Acum_s_vit.pdf", width = 20, height = 10, device = "pdf")
```

Por dia da semana
```
pacman::p_load(dplyr, ggplot2, lubridate, forcats)


# Supondo que Data2 seja sua coluna de datas
p4 <- p4 %>%
  mutate(Dia_Semana = wday(Data2, label = TRUE, abbr = FALSE, week_start = 1)) %>%
  mutate(Dia_Semana = fct_relevel(Dia_Semana, 
                                 "segunda", "terça", "quarta", 
                                 "quinta", "sexta", "sábado", "domingo"))

# Alternativa: facet por personagem
ggplot(p4, aes(x = Dia_Semana, fill = Personagem)) +
  geom_bar() +
  facet_wrap(~ Personagem) +
  labs(title = "Distribuição de jogadas por dia da semana",
       x = "Dia da Semana",
       y = "Contagem") +
  scale_fill_manual(values = rainbow(length(unique(p4$Personagem)))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Remove legenda redundante

```

### Similaridade
Agora um jacard
```
pacman::p_load("ade4", "NbClust", "reshape2", "dplyr")

# Carregar os dados
p2 <- pbase2
p2 <- p2[, !(names(p2) == "" | is.na(names(p2)))]

# Filtrar dados
p3 <- p2 %>%
  filter(
    !is.na(Personagem),
    !is.na(Patrono),
    N_categoria >= 20 & N_categoria <= 24)

# Remover duplicatas
p3 <- p3[!duplicated(p3), ]

# Criar a tabela pivotante
local <- dcast(p3, Personagem ~ Patrono, value.var = "Atributos", fun.aggregate = NULL, fill = 0)

# Definir Personagem como nome das linhas e remover a coluna
local <- data.frame(local, row.names = 1)

# Remover linhas com NA (se houver)
local <- na.omit(local)

# Calcular a matriz de distâncias usando o índice de Jaccard
d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE)

# Substituir NA e Inf na matriz de distâncias
d[is.na(d)] <- 0
d[is.infinite(d)] <- max(d[!is.infinite(d)])

# Aplicar hierarchical clustering
hc <- hclust(d)

# Plotar o dendrograma com rótulos
plot(hc, labels = rownames(local), main = "Dendrograma de Personagens", xlab = "", sub = "")

```
pacman::p_load(ggfortify, cluster, reshape2, dplyr)

p2 <- pbase2
#colnames(p2) <- make.names(colnames(p2), unique = TRUE)

# Filtrando os dados
p3 <- p2 %>%
  filter(!is.na(Personagem), 
         !is.na(Bonus), 
         !is.na(Classe_I), 
         !is.na(Quantidade),
         N_categoria == "20",
         #Equipado == "1",
         Classe_I != "Masmorra")

# Reshaping do dataframe
local <- reshape2::dcast(p3, Bonus + Classe_I ~ Personagem, value.var = "Quantidade", fun.aggregate = length)

# Certificando-se de que não há linhas sem dados
local <- local[complete.cases(local), ]
rownames(local) <- local$Bonus  # Usando Bonus como nome das linhas
local <- local[, -1]  # Remover a coluna 'Bonus' agora usada como nome das linhas

# Selecionar apenas colunas numéricas para o PCA
local_numeric <- local %>% select(where(is.numeric))

# Realizar PCA
pca_res <- prcomp(local_numeric, scale. = TRUE)

# Visualizar o PCA
pca <- autoplot(pca_res, data = local, colour = 'Classe_I', label = TRUE, label.size = 4, 
                frame = TRUE, frame.type = NULL, frame.color = 'Classe_I', 
                loadings = TRUE, loadings.colour = 'blue', loadings.label = TRUE, 
                loadings.label.size = 3) +
   theme_minimal() +
   theme(legend.position = "none")

pca

ggsave(width = 20, height = 10, device = "pdf", filename = "2025_8_PCA", plot = pca)
#path = "/home/user/Área de Trabalho/Serviços/ES - Rio Bananal/2021_03_03_Grancol/R"

```

PCA para identificar a paisagem mais importante 
```
pacman::p_load(ggfortify, cluster, reshape2, dplyr)

p2 <- pbase2
colnames(p2) <- make.names(colnames(p2), unique = TRUE)

# Filtrando os dados
p3 <- p2 %>%
  filter(
    !is.na(Personagem), 
    !is.na(Patrono), 
    !is.na(Influência), 
    between(as.numeric(N_categoria), 20, 22)
  ) %>%
  distinct(Evento, .keep_all = TRUE)
  

local <- reshape2::dcast(p3, Patrono + Influência ~ Personagem, 
                          value.var = "Atributos", fun.aggregate = NULL)

#local <- local %>% mutate(across(where(is.numeric), ~ ifelse(. > 0, 1, .)))
  
# Garantindo que 'Patrono' seja única e usando como nome das linhas
rownames(local) <- local$Patrono  
local <- local[, -1]  # Removendo a coluna 'Patrono' já usada nos nomes das linhas

# Selecionar apenas colunas numéricas para o PCA
local_numeric <- local %>% select(where(is.numeric)) %>% na.omit()

# Realizar PCA
pca_res <- prcomp(local_numeric, scale. = TRUE)

# Gerar o gráfico de PCA manualmente para garantir labels
pca <- autoplot(pca_res, data = local, colour = 'Influência', label = TRUE, label.size = 4, 
                frame = TRUE, frame.type = NULL, frame.color = 'Influência', 
                loadings = TRUE, loadings.colour = 'blue', loadings.label = TRUE, 
                loadings.label.size = 3) +
   theme_minimal() +
   theme(legend.position = "none")

pca



ggsave(width = 20, height = 10, device = "pdf", filename = "2025_8_PCA2", plot = pca)
#path = "/home/user/Área de Trabalho/Serviços/ES - Rio Bananal/2021_03_03_Grancol/R"