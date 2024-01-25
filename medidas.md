# Apresentação
Análises e gráficos da campanha de Pokémon

## Início
Primeiro, vamos indicar as pastas corretas.
```
getwd()
setwd("/home/user/Área de Trabalho/RPG/23_03_11_pokemon") 
```

Agora baixar e ler alguns pacotes básicos.

```
if(!require(pacman, quietly = TRUE))(install.packages("pacman")) #agrupador de funções
pacman::p_load(magrittr,dplyr,reshape2) #magrittr para operações de pipe/dplyr para manipulador de dados
pacman::p_load(ggplot2, ggrepel, graphics,lubridate, gghighlight, tidyquant) #devtools, 
pacman::p_load(forcats,iNEXT,tidyr,tibble,iNEXT)
pacman::p_load(vegan)  #vegan para estatística ecológica/graphics para os gráficos

#sudo apt-get install r-cran-devtools

```
Agora vamos adicionar a planilha.
```
pacman::p_load(openxlsx)
caminho.do.arquivo <- "/home/user/Área de Trabalho/RPG/23_03_11_pokemon/2023_03_11_pokemon.xlsx"
planilhatotal <- read.xlsx(caminho.do.arquivo, #local do arquivo
                           sheet = 1, # em qual planilha estão os dados
                           colNames = T, # as colunas dos dados possuem nomes?
                           na.strings = "NA") # como estão identificados os dados omissos?

#head(planilhatotal)
```
E filtrar e montar a pespectiva de data.
```
p2 <- subset(planilhatotal, !is.na(Região)) #tirar n/a da ano
p2 <- subset(p2, !is.na(Número)) #tirar n/a da pontos
pbase2 <- p2

p2 <- subset(p2, !is.na(Altura)) #tirar n/a da pontos
p2 <- subset(p2, T.Encontro == "1") 
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

Agora um gráfico unificado para Treinador (Rota, Paisagem)
```
p2 <- Data
p3 <- subset(p2, T.Treinador == "Jogador") 

local<-reshape2::dcast(p3, Sub ~ Treinador, value.var = "Total.dex", fun.aggregate = sum)
local=data.frame(local, row.names=1)

#Mude o q para 1 para comparar a diversidade de Shannon e para 2 para Simpson

out <- iNEXT(local, q = 0,
             datatype = "abundance",
             size = seq(0, 350, length.out=20))

R <- ggiNEXT(out, type = 1) +
  theme_bw() +
  labs(fill = "Áreas") +
  #xlab("Riqueza) + 
  #ylab("Tempo") +
  scale_shape_manual(values = 0:19) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14), legend.position="bottom")

R

#ggsave(width = 20, height = 10, 
       device = "pdf", filename = "2024_1_2_Acum", plot = R)
    
```
Cluster de similaridade das rotas por pokémon
```
pacman::p_load("ade4")
p2 <- pbase2
p2 <- subset(p2, !is.na(Rota)) 
p3 <- subset(p2, T.Treinador == "Selvagem") 
#p3 <- subset(p2, Encontro == "Walking") #enquanto não tem pesca
p3[-1][p3[-1] < 0] <- 0 #transformar negativo em 0

local<-reshape2::dcast(p3, Rota ~ Sub) #, value.var = "Raridade", fun = sum)
local=data.frame(local, row.names=1)
d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) #method 1Rota is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
plot(hc, labels=local$ID)    # plot the dendrogram

```
## PCA

PCA para identificar a paisagem mais imortante por família de Pokémon
```
pacman::p_load(ggfortify, cluster)

p2 <- Data
p2 <- subset(p2, T.Treinador == "Jogador") 
p2 <- subset(p2, !is.na(Local)) #tirar n/a da ano

#p2 <- subset(p2, Grupo == "Mastofauna")

local<-reshape2::dcast(p2, Sub ~ Paisagem, value.var = "Total.dex", fun.aggregate = sum) #sum ou NULL
local=data.frame(local, row.names=1)
#local[local>0]<-1

pca_res <- prcomp(local, scale. = TRUE)  #TRUE der errado verificar as paisagens. 
#autoplot(pca_res)

local<-reshape2::dcast(p2, Sub + Linha.E ~ Paisagem, value.var = "Total.dex", fun.aggregate = sum) #sum ou NULL
pca <-autoplot(pca_res, data = local, colour = 'Linha.E', label = TRUE, label.size = 4, 
         frame = TRUE, frame.type = NULL, frame.color = 'Grupo', #ou frame.type = 't'
         loadings = TRUE, loadings.colour = 'blue',loadings.label = TRUE, loadings.label.size = 3) +                   
         #scale_color_tq() + scale_fill_tq() + 
         theme_tq() +
         theme(legend.position = "none")  

pca

#ggsave(width = 20, height = 10, device = "pdf", filename = "2024_1_PCA", plot = pca)
#path = "/home/user/Área de Trabalho/Serviços/ES - Rio Bananal/2021_03_03_Grancol/R"


pacman::p_load(ggfortify, cluster)

p2 <- Data
p2 <- subset(p2, T.Treinador == "Jogador") 
p2 <- subset(p2, !is.na(Paisagem)) #tirar n/a da ano

#p2 <- subset(p2, Grupo == "Mastofauna")

local<-reshape2::dcast(p2, Pokémon ~ Paisagem, value.var = "Total.dex", fun.aggregate = NULL) #sum ou NULL
local=data.frame(local, row.names=1)
#local[local>0]<-1

pca_res <- prcomp(local, scale. = TRUE)  #TRUE der errado verificar as paisagens. 
#autoplot(pca_res)

local<-reshape2::dcast(p2, Pokémon + Classe ~ Paisagem, value.var = "Total.dex", fun.aggregate = NULL) #sum ou NULL

pca <-autoplot(pca_res, data = local, colour = 'Classe', label = TRUE, label.size = 4, 
         frame = TRUE, frame.type = NULL, frame.color = 'Grupo', #ou frame.type = 't'
         loadings = TRUE, loadings.colour = 'blue',loadings.label = TRUE, loadings.label.size = 3) +                   
         scale_color_tq() + scale_fill_tq() + theme_tq() 

pca

#ggsave(width = 20, height = 10, device = "pdf", filename = "2023_12_PCA2", plot = pca)
#path = "/home/user/Área de Trabalho/Serviços/ES - Rio Bananal/2021_03_03_Grancol/R"



```
### Análises eploratórias

## Rota

Capturar por Tempo
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
  geom_smooth(method = loess,se = FALSE, alpha = 0.6, aes(colour = Treinador)) +  #method = lm ou loess
  geom_xsideboxplot(aes(fill = Treinador),alpha = 0.5) +
  geom_ysideboxplot(aes(fill = Treinador),alpha = 0.5) + #
  stat_ellipse(geom="polygon", aes(fill = Treinador), alpha = 0.2, show.legend = TRUE,level = 0.25) + 
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  scale_color_tq() + scale_fill_tq() + theme_tq() 

#ggsave(width = 20, height = 13, device = "pdf", filename = "2024_2_1_tempo")

p4 <- data.frame(p4)

ggplot(p4, aes(x = reorder(Fase, Rota), y = Data, colour = Treinador)) + 
  geom_boxplot(aes(full = Treinador)) +
  geom_point(aes(size = N.0, shape = Contato), alpha = 0.4) + 
  scale_shape_manual(values = 0:10) +
  scale_size(range = c(5, 18), name = "Nível") +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  labs(x="Rota")+
  scale_color_tq() + scale_fill_tq() + theme_tq() + coord_flip()

#ggsave(width = 20, height = 13, device = "pdf", filename = "2024_1_tempo2")
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

ggplot(p4, aes(x = N.0, y = Tag)) + 
  geom_jitter(aes(size = Atributo.T, color = Treinador), alpha = 0.6) + 
  scale_shape_manual(values = 0:10) +
  geom_boxplot(alpha = 0.2) +
  scale_size(range = c(1, 11), name = "Atributo total") +
  labs(title=" ", subtitle="",y="Tipo de treinador",x="Nível do Pokémon", caption="", shape = "", colour = "Treinador", size = "Atributo total") +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  scale_fill_tq() + scale_color_tq() + theme_tq() 

#ggsave(width = 20, height = 13, device = "pdf", filename = "2024_2_adversários")
```

## Pókemns

Agora, vamos ver os pokémons filogenéticamente

```
p2 <- pbase2
p3 <- p2 %>% filter(str_detect(T.Encontro, "0")) 

p4 <- p3

ggplot(p4, aes(x = Atributo.T+N.0*6, y = Reino)) + 
  geom_jitter(aes(size = N.0, color = Filo), alpha = 0.2) + 
  facet_grid(Domínio~., scales = "free_y", space = "free_y") + 
  geom_boxplot(aes(fill = Filo), alpha = 0.6) +
  scale_size(range = c(1, 11), name = "Atributo total") +
  labs(title=" ", subtitle="",y="Reino Pókemon",x="Atributos", caption="", shape = "", colour = "Filo Pókemon", fill = "Filo Pókemon", size = "Nível do Pókemon") +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  scale_fill_tq() + scale_color_tq() + theme_tq() 

#ggsave(width = 20, height = 13, device = "pdf", filename = "2023_12_1_filo")
``` 

# Diversidade de Pokémons treinados
``` 
pacman::p_load(treemapify) 

p2 <- pbase

Data <- p2 %>% 
  select(Ano,Mês,Dia) %>% 
  mutate(Data = make_date(Ano,Mês,Dia))
Data <- data.frame(p2,Data)

p2 <- Data
p3 <- p2 %>% filter(str_detect(T.Treinador, "Jogador"))
#p3 <- p2 %>% filter(str_detect(XXXX, "XXX")) 

#p3 <- p3 %>% filter(str_detect(Treinador, "Vinícius")) 
#p3 <- rbind(p3,p4)
p3 <- p3 %>% filter(str_detect(Jogo, "2")) #escolher artista
p3 <- subset(p3, Jogo!="0") #retirar artista
p3<-unique(p3)
p4 <- p3

#p2 <- subset(p2, XXXX == "XXXX")

local<-reshape2::dcast(p4, Treinador + Nome +  Atributo.T + Treino ~ Região, value.var = "N.0", fun.aggregate = sum) #sum ou NULL
local$Treino <- as.numeric(gsub(",", ".", local$Treino))

soma <- local %>%
   mutate(Soma = Atributo.T + Kanto*6 + Treino)


local=data.frame(local, soma)
local <- local[order(-local$Soma), ]

ggplot(local, aes(area = Soma, fill = Nome, 
  label = paste (Nome, Soma, sep = "\n"), subgroup = Treinador)) +
  geom_treemap() +
  geom_treemap_subgroup_text(place = "centre", grow = TRUE,
                             alpha = 0.25, colour = "black",
                             fontface = "italic") +
  geom_treemap_text(colour = "white",
                    place = "bottom",  size = 10) + 
  #scale_color_tq() + scale_fill_tq() + 
  theme_tq() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), legend.position = "none") 

#ggsave(width = 20, height = 10, device = "pdf", filename = "2024_1_treinado")
```
Com todos os treinados
```

p2 <- Data
p3 <- subset(p2, !is.na(Nome))
p3 <- subset(p3, Jogo!="0") #retirar artista
p3<-unique(p3)

p4 <- p3


local<-reshape2::dcast(p4, Treinador + Nome +  Atributo.T + Treino ~ Região, value.var = "N.0", fun.aggregate = sum) #sum ou NULL
local$Treino <- as.numeric(gsub(",", ".", local$Treino))

soma <- local %>%
   mutate(Soma = Atributo.T + Kanto*6 + Treino)

local=data.frame(local, soma)
local <- local[order(-local$Soma), ]

ggplot(local, aes(area = Soma, fill = Nome, 
  label = paste (Nome, Soma, sep = "\n"), subgroup = Treinador)) +
  geom_treemap() +
  geom_treemap_subgroup_text(place = "centre", grow = TRUE,
                             alpha = 0.25, colour = "black",
                             fontface = "italic") +
  geom_treemap_text(colour = "white",
                    place = "bottom",  size = 10) + 
  #scale_color_tq() + scale_fill_tq() + 
  theme_tq() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), legend.position = "none") 

#ggsave(width = 20, height = 10, device = "pdf", filename = "2024_1_treinadototal")

```

## Dimensão

Evidenciando os mais diferentes quanto a difrença de tamanho e peso
``` 
pacman::p_load(ggside, stringr) #, tidyverse,tidyquant)

p2 <- Data
#p2 <- planilhatotal
#p3 <- p2 %>% filter(str_detect(Classe, "Mammalia"))
#p3 <- p2 %>% filter(str_detect(N.E., "16")) 
#p3 <- rbind(p3,p4)
#p3 <- subset(p2, T.Treinador == "Jogador") #escolher Pokémon
p3 <- p3 %>% filter(str_detect(Jogo, "2")) #escolher artista
#p3 <- subset(p3, Route!="Parceria") #retirar artista
p3<-unique(p3)

p4 <- p3
#p4 <- subset(p4,Tipo!="Grass") 

#p4 <- p4 %>%  subset(Fator > 75)

ggplot(p4, aes(x = Peso, y = Altura, colour = Treinador)) + 
  geom_point(aes(shape = Treinador, size = Atributo.T + N.0*6)) + 
  scale_shape_manual(values = 0:10) +
  geom_label_repel(aes(label = Pokémon, colour = Treinador), size=2.5, alpha= 1,box.padding = 0.35,point.padding = 0.75,segment.color = 'grey50') +
  geom_vline(xintercept = 80, alpha = 0.4)+
  geom_hline(yintercept = 1.8, alpha = 0.4) +
  #facet_grid(.~Treinador, scales = "free_y", space = "free_y") + #
  #gghighlight(Fator > 75, label_key = Pokémon) +
  scale_size(range = c(5, 18), name = "Pontos de atributos") +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  scale_color_tq() + scale_fill_tq() + theme_tq() 

#ggsave(width = 20, height = 10, device = "pdf", filename = "2024_1_tamanho")
``` 
Evidenciando os mais diferentes quanto a tamanho geral
``` 
p2 <- Data
#p2 <- planilhatotal
#p3 <- p2 %>% filter(str_detect(Classe, "Mammalia"))
#p3 <- p2 %>% filter(str_detect(N.E., "16")) 
#p3 <- rbind(p3,p4)
p3 <- subset(p2, T.Treinador == "Jogador") #escolher Pokémon
#p3 <- subset(p3, Route!="Parceria") #retirar artista
p3<-unique(p3)

p4 <- p3
#p4 <- subset(p4,Tipo!="Grass") 

p4 <- p4 %>%  subset(Fator > 100)
p4 <- p4 %>%  subset(Fator < 0.05)
#p4 <- p4 %>%  subset(A_V > 99 | A_V < 1 | P_V > 99 | P_V < 1)


ggplot(p4, aes(x = P_V, y = A_V, colour = Treinador)) + 
  geom_point(aes(shape = Treinador, size = Fator)) + 
  scale_shape_manual(values = 0:10) +
  #geom_boxplot(aes(fill = Número)) +
  geom_label_repel(aes(label = Pokémon, colour = Treinador), size=2.5, alpha= 1,box.padding = 0.35,point.padding = 0.75,segment.color = 'grey50') +
  #facet_grid(.~Treinador, scales = "free_y", space = "free_y") + #
  #gghighlight(Fator > 75, label_key = Pokémon) +
  scale_size(range = c(5, 18), name = "Fator de diferença") +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  scale_color_tq() + scale_fill_tq() + theme_tq() 

#ggsave(width = 20, height = 10, device = "pdf", filename = "2023_12_2")
``` 

#### Outro

### Acumulação
```
p2 <- Data

#p2 <- subset(p2, Jogador == "Daniel") 
#p2 <- subset(p2,Rotas!="XXXX") 

acum<-reshape2::dcast(p2, Data ~ Pokémon, value.var = "Total.dex", fun.aggregate = sum)
acum=data.frame(acum, row.names=1)

acumplot<-specaccum(acum) #dados de acumulação
#plot(acumplot) #curva simples
plot(acumplot,ci.type="poly",col="black",lwd=2,ci.lty=0,ci.col="lightgrey",ylab="Riqueza",
     xlab="Dias de amostragem",main="Curva de acumulação de registros",las=1,font=1.5,font.lab=1.5,cex.lab=1,cex.axis=1) #curva clássica
```
Outro gráficos mais detalhados e visando as subespécies. 
```
acum<-reshape2::dcast(p2, Data ~ Sub, value.var = "Total.dex", fun = sum)
acum=data.frame(acum, row.names=1)

sp1<-specaccum(acum,method="rarefaction")
sp2<-specaccum(acum,method="exact")
sp3<-specaccum(acum,method="random")
sp4<-specaccum(acum,method="collector")

par(mfrow=c(2,2)) 
plot(sp1, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Dias de amostragem",ylab="Rarefação")
plot(sp2, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightgrey",xlab="Dias de amostragem",ylab="Riqueza Esperada")
plot(sp3, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="yellow",xlab="Dias de amostragem",ylab="Sítios Aleatórios")
plot(sp4, ci.type="poly", col="black", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Dias de amostragem",ylab="Curva do Coletor")
par(mfrow=c(1,1)) #compilado de curvas

```
Focar na curva do coletor
```
sp4<-specaccum(acum,method="collector")
shannon<-diversity(acum)
spAbund<-rowSums(acum)

acum<-reshape2::dcast(Data, Data ~ Sub, value.var = "Total.dex", fun = sum)
p3<-data.frame(shannon,spAbund,sp4$sites,sp4$richness,acum)

ggplot(p3, aes(x = Data, y = sp4.richness)) + 
  geom_line(linewidth=6, alpha=0.6, color="Gray") + #geom_line(aes(group = sp4.sites))
  geom_point(aes(size=spAbund, colour=shannon), alpha=0.3) +
    geom_label_repel(aes(label = sp4$richness), size=4, alpha=0.8, #funciona no zoom
                   box.padding   = 0.35, 
                   point.padding = 0.75,
                   segment.color = 'grey50') +
  scale_size(range = c(.1, 24), name="Abundancia de registros") +
  #geom_text(aes(label = a$sp4.richness),col = 'black',size = 5) +
  labs(title="Curva do coletor", subtitle="Total", y="Riqueza",x="Data", caption="",
       color = "Diversidade", size = "Abundancia de registros") +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) +
  #scale_color_tq() +
  #scale_fill_tq() +
  theme_tq() 
  
#ggsave("2023_09_07_Acum2.pdf",width = 14, height = 6, dpi = 300)
```
## Estimativa de riqueza

```
p2 <- Data
#p2 <- subset(p2, Grupo == "Hepertofauna") 

p3<-reshape2::dcast(p2, Data ~ Sub, value.var = "Total.dex", fun = sum)
p3=data.frame(p3, row.names=1)

pool<-specpool(p3)
pool
```

Também podemos separar pelas varíaveis
```
#p3 <- subset(Data, Empresa == "XXX")

p3<-reshape2::dcast(p2, Data + Linha.E ~ Sub, value.var = "Total.dex", fun = sum)
excluir <- c("Data", "Linha.E")
p3 <- p3[,!(names(p3)%in% excluir)]
p4<-reshape2::dcast(Data, Data + Linha.E ~ Classe, value.var = "Total.dex", fun = sum)

pool<-specpool(p3, p4$Linha.E) 
pool
#boxplot(pool$chao) 
```
### Diversidade

Diversidade treinador (rota, paisagem)
```
p2 <- Data
p3 <- subset(p2, T.Treinador == "Jogador") 
p3 <- subset(p3, !is.na(Rota))

local<-reshape2::dcast(p3, Rota ~ Sub, value.var = "Total.dex", fun.aggregate = sum)
local=data.frame(local, row.names=1)

R <- renyi(local,hill = TRUE)

R <- R %>%  
  rownames_to_column() %>% 
  pivot_longer(-rowname) %>% 
  mutate(name = factor(name, name[1:length(R)])) %>% 
  ggplot(aes(x = name, y = value, group = rowname,
             col = rowname)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  xlab("Parâmetro de ordem de diversidade (q)") +
  ylab("Diversidade") +
  labs(col = "Tipos") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14), legend.position="bottom")

R  

```
### Cluster

Cluster de similaridade das rotas (Treinador)
```
pacman::p_load("ade4")
p2 <- Data
p3 <- subset(p2, T.Treinador == "Jogador") 

local<-reshape2::dcast(p3, Treinador ~ Sub, value.var = "Total.dex", fun = sum)
local=data.frame(local, row.names=1)
d <- dist.binary(local, method = 1, diag = FALSE, upper = FALSE) #method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
plot(hc, labels=local$ID)    # plot the dendrogram

```
### PCA

E as rotas
```
pacman::p_load(ggfortify, cluster)

p2 <- Data
p2 <- subset(p2, T.Treinador == "Jogador")

local<-reshape2::dcast(p2, Família ~ Rota, value.var = "Total.dex", fun.aggregate = sum) #sum ou NULL
local=data.frame(local, row.names=1)

pca_res <- prcomp(local, scale. = TRUE)
#autoplot(pca_res)

local<-reshape2::dcast(p2, Família + Classe ~ Rota, value.var = "Total.dex", fun.aggregate = sum) #sum ou NULL
pca <-autoplot(pca_res, data = local, colour = 'Classe', label = TRUE, label.size = 4, 
         frame = TRUE, frame.type = NULL, frame.color = 'Grupo', #ou frame.type = 't'
         loadings = TRUE, loadings.colour = 'blue',loadings.label = TRUE, loadings.label.size = 3) +
         scale_color_tq() + scale_fill_tq() + theme_tq() 
pca
```
E as rotas por subspécie
```
pacman::p_load(ggfortify, cluster)

p2 <- Data

#p2 <- subset(p2, Grupo == "Mastofauna")

local<-reshape2::dcast(p2, Sub ~ Rota, value.var = "Total.dex", fun.aggregate = sum) #sum ou NULL
local=data.frame(local, row.names=1)

pca_res <- prcomp(local, scale. = TRUE)
#autoplot(pca_res)

local<-reshape2::dcast(p2, Sub + Classe ~ Rota, value.var = "Total.dex", fun.aggregate = sum) #sum ou NULL
pca <-autoplot(pca_res, data = local, colour = 'Classe', label = TRUE, label.size = 3, 
         frame = TRUE, frame.type = NULL, frame.color = 'Grupo', #ou frame.type = 't'
         loadings = TRUE, loadings.colour = 'blue',loadings.label = TRUE, loadings.label.size = 4) +                   
         scale_color_tq() + scale_fill_tq() + theme_tq() 
pca

#ggsave(width = 20, height = 10, device = "pdf", filename = "2023_09_07_PCAabu", plot = pca)
```
### Adversários 
Adversários na base - usaro p2 de adversário
```
p2 <- pbase2
p3 <- p2 %>% filter(str_detect(T.Encontro, "4")) 

p4 <- p3
#p4 <- subset(p4,Tipo!="Grass") 

#p4 <- p4 %>%  subset(Fator > 75)

ggplot(p4, aes(x = N.0, y = Tag)) + 
  geom_jitter(aes(size = Atributo.T, color = Encontro), alpha = 0.4) + 
  scale_size(range = c(1, 11), name = "Atributo total") +
  geom_boxplot(alpha = 0.2) +
  labs(title="", subtitle="",y="Tipo de treinador",x="Nível do Pókemon", caption="", shape = "", colour = "Tipo de encontro", size = "Atributo total") +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))+
  scale_fill_tq() + theme_tq() 
```
### Gráficos de área

Diversidade de Pokémons em gráficos de área por jogador
```
p2 <- pbase

Data <- p2 %>% 
  select(Ano,Mês,Dia) %>% 
  mutate(Data = make_date(Ano,Mês,Dia))
Data <- data.frame(p2,Data)

p2 <- Data
p3 <- p2 %>% filter(str_detect(T.Treinador, "Jogador"))
#p3 <- p2 %>% filter(str_detect(XXXX, "XXX")) 

#p3 <- p3 %>% filter(str_detect(Treinador, "Vinícius")) 
#p3 <- rbind(p3,p4)
#p3 <- p3 %>% filter(str_detect(Jogo, "2")) #escolher artista
p3 <- subset(p3, Jogo!="0") #retirar artista
p3<-unique(p3)

p4 <- p3

#p4 <- p4 %>%  subset(XXX > 0.6)

local<-reshape2::dcast(p4, Linha.E + Sub ~ Região, value.var = "Total.dex", fun.aggregate = sum) #sum ou NULL
#local=data.frame(local, row.names=1)

ggplot(local, aes(area = Kanto, fill = Sub, 
  label = paste (Sub, Kanto, sep = "\n"), subgroup = Linha.E)) +
  geom_treemap() +
  geom_treemap_subgroup_text(place = "centre", grow = TRUE,
                             alpha = 0.25, colour = "black",
                             fontface = "italic") +
  geom_treemap_text(colour = "white",
                    place = "bottom",
                    size = 10) +
  theme_classic() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), legend.position = "none") 


p2 <- Data
p3 <- subset(p2, !is.na(Nome))
p3 <- subset(p3, Jogo!="0") #retirar artista
p3<-unique(p3)

p4 <- p3


local<-reshape2::dcast(p4, Treinador + Nome +  Atributo.T ~ Região, value.var = "N.0", fun.aggregate = sum) #sum ou NULL
soma <- local %>%
   mutate(Soma = Atributo.T + Kanto*6)

local=data.frame(local, soma)

ggplot(local, aes(area = Soma, fill = Nome, 
  label = paste (Nome, Soma, sep = "\n"), subgroup = Treinador)) +
  geom_treemap() +
  geom_treemap_subgroup_text(place = "centre", grow = TRUE,
                             alpha = 0.25, colour = "black",
                             fontface = "italic") +
  geom_treemap_text(colour = "white",
                    place = "bottom",  size = 10) + 
  #scale_color_tq() + scale_fill_tq() + 
  theme_tq() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), legend.position = "none") 

```
Raridade por rota
```
p2 <- pbase
p3 <- p2 %>% filter(str_detect(T.Treinador, "Selvagem"))
#p3 <- p2 %>% filter(str_detect(XXXX, "XXX")) 

p3 <- p3 %>% filter(str_detect(Linha.E, "Cartepie")) 
#p3 <- rbind(p3,p4)
#p3 <- p3 %>% filter(str_detect(Encontro, "Walking")) #escolher artista
p3 <- subset(p3, Encontro!="Fishing") #retirar artista
p3<-unique(p3)

p4 <- p3
#p4 <- subset(p4,XXXX!="XXX") 
#p4 <- subset(p4,XXXX!="XX") 

#p4 <- p4 %>%  subset(XXX > 0.6)

local<-reshape2::dcast(p4, Rota + Sub ~ Região, value.var = "Raridade", fun.aggregate = sum) #sum ou NULL
#local=data.frame(local, row.names=1)

ggplot(local, aes(area = Kanto, fill = Sub, 
  label = paste (Sub, Kanto, sep = "\n"), subgroup = Rota)) +
  geom_treemap() +
  geom_treemap_subgroup_text(place = "centre", grow = TRUE,
                             alpha = 0.25, colour = "black",
                             fontface = "italic") +
  geom_treemap_text(colour = "white",
                    place = "bottom",
                    size = 10) +
  theme_classic() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), legend.position = "none") 

```
Raridade por Pokémon
```
p2 <- pbase2
p3 <- p2 %>% filter(str_detect(T.Treinador, "Selvagem"))
#p3 <- p2 %>% filter(str_detect(XXXX, "XXX")) 

p3 <- p3 %>% filter(str_detect(Fase, "9")) 
#p3 <- rbind(p3,p4)
#p3 <- p3 %>% filter(str_detect(Encontro, "Walking")) #escolher artista
p3 <- subset(p3, Encontro!="Fishing") #retirar artista
p3<-unique(p3)

p4 <- p3
#p4 <- subset(p4,XXXX!="XXX") 
#p4 <- subset(p4,XXXX!="XX") 

#p4 <- p4 %>%  subset(XXX > 0.6)

local<-reshape2::dcast(p4, Linha.E + Sub ~ Região, value.var = "Raridade", fun.aggregate = sum) #sum ou NULL
#local=data.frame(local, row.names=1)

ggplot(local, aes(area = Kanto, fill = Sub, 
  label = paste (Sub, Kanto, sep = "\n"), subgroup = Linha.E)) +
  geom_treemap() +
  geom_treemap_subgroup_text(place = "centre", grow = TRUE,
                             alpha = 0.25, colour = "black",
                             fontface = "italic") +
  geom_treemap_text(colour = "white",
                    place = "bottom",
                    size = 10) +
  theme_classic() +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14), legend.position = "none") 

```

### Outros Teste
