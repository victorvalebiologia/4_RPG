# ============================================================================
# ANÁLISE COMPLETA DAS CAMPANHAS DE RPG - ARTON
# ============================================================================
# ORDEM LÓGICA: Visão Geral → Personagens → Religião → Mapas → Estrutura → Combates
# ============================================================================

# ============================================================================
# 1. CONFIGURAÇÃO INICIAL
# ============================================================================

rm(list = ls())
gc()
setwd("/home/victor-vale/Área de trabalho/R/RPG")

cat("\n", rep("=", 70), "\n", sep = "")
cat("🎲 ANÁLISE DAS CAMPANHAS DE RPG - ARTON\n")
cat(rep("=", 70), "\n\n", sep = "")

# ============================================================================
# 2. PACOTES
# ============================================================================

pacotes <- c(
  "magrittr", "dplyr", "reshape2", "ggplot2", "ggrepel",
  "lubridate", "gghighlight", "forcats", "iNEXT", "tidyr",
  "tibble", "vegan", "ggside", "googledrive", "googlesheets4",
  "readxl", "patchwork", "ggfortify", "cluster", "ade4",
  "spaa", "recluster", "analogue", "ape", "tm", "xml2",
  "SnowballC", "wordcloud", "wesanderson", "wordcloud2", "stringr",
  "fmsb", "plotly", "scales"
)

novos <- pacotes[!pacotes %in% installed.packages()]
if(length(novos) > 0) install.packages(novos)
invisible(lapply(pacotes, library, character.only = TRUE))

cat("✅ Pacotes carregados!\n")

# ============================================================================
# 3. FUNÇÕES AUXILIARES
# ============================================================================

# PALETA DE CORES
cores_personagens <- function(n) {
  if(n <= 8) {
    RColorBrewer::brewer.pal(n, "Set2")
  } else if(n <= 12) {
    RColorBrewer::brewer.pal(n, "Paired")
  } else {
    colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n)
  }
}

# SHAPES
shapes_personalizados <- c(16, 17, 15, 18, 8, 3, 4, 0, 1, 2, 5, 6, 7, 9, 10, 
                           11, 12, 13, 14, 19, 20, 21, 22, 23, 24, 25)

# TEMA PROFISSIONAL
tema_rpg <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = base_size + 4, color = "#2C3E50"),
      plot.subtitle = element_text(hjust = 0.5, size = base_size, color = "#7F8C8D"),
      axis.title = element_text(face = "bold", size = base_size, color = "#34495E"),
      axis.text = element_text(size = base_size - 1, color = "#2C3E50"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = base_size, color = "#2C3E50"),
      legend.text = element_text(size = base_size - 1, color = "#34495E"),
      legend.key.size = unit(0.7, "cm"),
      legend.key.width = unit(0.8, "cm"),
      legend.background = element_rect(fill = "white", color = "#E0E0E0", size = 0.3),
      legend.margin = margin(6, 10, 6, 10),
      panel.grid.major = element_line(color = "#ECF0F1", size = 0.4),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(15, 20, 15, 20)
    )
}

# ============================================================================
# SALVAR GRÁFICOS NA PASTA "resultados"
# ============================================================================

# Criar a pasta "resultados" se não existir
dir.create("resultados", recursive = TRUE, showWarnings = FALSE)

# SALVAR PDF
salvar_pdf <- function(plot, nome, largura = 20, altura = 10) {
  caminho <- paste0("resultados/", nome, ".pdf")
  ggsave(filename = caminho, plot = plot, 
         width = largura, height = altura, device = "pdf")
  cat("  ✅ Salvo:", caminho, "\n")
}

# SALVAR PNG
salvar_png <- function(plot, nome, largura = 12, altura = 8, dpi = 300) {
  caminho <- paste0("resultados/", nome, ".png")
  ggsave(filename = caminho, plot = plot, 
         width = largura, height = altura, dpi = dpi, device = "png")
  cat("  ✅ Salvo:", caminho, "\n")
}
# ============================================================================
# 4. IMPORTAR DADOS
# ============================================================================

cat("\n📥 Importando dados...\n")

drive_auth()
pasta <- drive_get(as_id("1yVHEzmcHc5GnwoFqcdT60eSlvX-QEn3l"))
arquivo <- drive_ls(pasta) %>% filter(name == "25_2_16_arton")

if(nrow(arquivo) == 1) {
  drive_download(as_id(arquivo$id), path = "25_2_16_arton.xlsx", overwrite = TRUE)
  planilhatotal <- read_excel("25_2_16_arton.xlsx", .name_repair = "minimal")
  cat("✅ Arquivo importado:", nrow(planilhatotal), "linhas\n")
} else {
  stop("❌ Arquivo não encontrado!")
}

# ============================================================================
# 5. PREPARAR DADOS
# ============================================================================

cat("\n🔧 Preparando dados...\n")

# Remover colunas sem nome
planilhatotal <- planilhatotal[, !is.na(names(planilhatotal)) & names(planilhatotal) != ""]
names(planilhatotal) <- make.names(names(planilhatotal), unique = TRUE)

# Base inicial
pbase <- planilhatotal %>%
  filter(!is.na(Jogador), !is.na(Personagem))

# Filtrar grupo G5.1
#p2 <- pbase %>% filter(Grupo == "G5.1")
p2 <- pbase %>% filter(Grupo %in% c("G5.1", "G5.3"))

names(p2) <- make.names(names(p2), unique = TRUE)

# Preparar data de Arton
p2 <- p2 %>%
  mutate(
    Mês_A = ifelse(is.na(Mês_A) | Mês_A == "", 1, Mês_A),
    Dia_A = ifelse(is.na(Dia_A) | Dia_A == "", 1, Dia_A)
  ) %>%
  filter(!is.na(Ano_A), !is.na(Mês_A), !is.na(Dia_A))

Data <- p2 %>%
  mutate(Data = make_date(Ano_A, Mês_A, Dia_A))

# Preparar data do jogo
p2 <- pbase %>% filter(Grupo == "G5.1")
names(p2) <- make.names(names(p2), unique = TRUE)

p2 <- p2 %>%
  mutate(
    Mês = ifelse(is.na(Mês) | Mês == "", 1, Mês),
    Dia = ifelse(is.na(Dia) | Dia == "", 1, Dia),
    Ano = ifelse(is.na(Ano) | Ano == "", 2025, Ano)
  ) %>%
  filter(!is.na(Ano), !is.na(Mês), !is.na(Dia))

Data2 <- Data %>%
  mutate(Data2 = make_date(Ano, Mês, Dia))

# Base sem data
pbase2 <- planilhatotal %>%
  filter(!is.na(Jogador), !is.na(Personagem)) %>%
  filter(Grupo == "G5.1")

names(pbase2) <- make.names(names(pbase2), unique = TRUE)

cat("✅ Dados preparados!\n")
cat("📊 Eventos:", nrow(Data2), "\n")
cat("👥 Personagens:", paste(unique(Data2$Personagem), collapse = ", "), "\n")

# ============================================================================
# 6. GRÁFICOS - VISÃO GERAL
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("📊 VISÃO GERAL\n")
cat(rep("=", 70), "\n\n", sep = "")

# ============================================================================
# 6.1. DASHBOARD - PERSONAGENS (Pizza + Dias de Jogo)
# ============================================================================

cat("📊 01. Dashboard - Personagens...\n")

p3 <- Data2 %>%
  filter(!is.na(Personagem), N_categoria == "20") %>%
  distinct(Evento, .keep_all = TRUE)

p3_pie <- p3 %>%
  count(Personagem) %>%
  mutate(
    Proporcao = n / sum(n),
    Label = paste0(Personagem, "\n", round(Proporcao * 100, 1), "%")
  )

g_pizza <- ggplot(p3_pie, aes(x = "", y = Proporcao, fill = Personagem)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 0.3) +
  coord_polar("y", start = 0) +
  geom_text_repel(aes(label = Label), position = position_stack(vjust = 0.5),
                  size = 3.5, fontface = "bold", show.legend = FALSE) +
  scale_fill_manual(values = cores_personagens(length(unique(p3_pie$Personagem)))) +
  labs(title = "Quem São os Personagens?", subtitle = "Proporção de eventos (Categoria 20)") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14, color = "#2C3E50"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#7F8C8D"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9)
  ) +
  guides(fill = guide_legend(title = "Personagem", ncol = 1))

p4 <- Data2 %>%
  filter(!is.na(Personagem), N_categoria == "20", Ano_A >= 1330) %>%
  distinct(Evento, .keep_all = TRUE) %>%
  mutate(Dia_Jogo = as.Date(Data2)) %>%
  filter(!is.na(Dia_Jogo))

total_dias <- p4 %>%
  group_by(Personagem) %>%
  summarise(
    Dias_Jogo = n_distinct(Dia_Jogo),
    Eventos = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Dias_Jogo))

g_dias <- ggplot(total_dias, aes(x = reorder(Personagem, Dias_Jogo), y = Dias_Jogo, fill = Personagem)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  geom_text(aes(label = paste0(Dias_Jogo, " dia", ifelse(Dias_Jogo > 1, "s", ""))), 
            hjust = -0.1, size = 3.5, fontface = "bold", color = "#2C3E50") +
  coord_flip() +
  scale_fill_manual(values = cores_personagens(nrow(total_dias))) +
  labs(title = "Quando Jogaram?", 
       subtitle = paste("Período:", format(min(p4$Dia_Jogo), "%d/%m/%Y"), "a", format(max(p4$Dia_Jogo), "%d/%m/%Y")),
       x = "", y = "Número de Dias de Jogo") +
  tema_rpg() +
  theme(legend.position = "none", axis.text.y = element_text(size = 10, face = "bold"))

library(patchwork)
dashboard_personagens <- (g_pizza | g_dias) +
  plot_annotation(
    title = "📖 Quem São e Quando Jogaram?",
    subtitle = "Distribuição de eventos e dias de jogo por personagem",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18, color = "#2C3E50"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#7F8C8D"),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

print(dashboard_personagens)
salvar_pdf(dashboard_personagens, "01_dashboard_personagens", largura = 22, altura = 10)

# ============================================================================
# 6.2. ACUMULAÇÃO POR DATA
# ============================================================================

cat("📊 02. Acumulado de Adversários...\n")

p2 <- Data
p3 <- subset(p2, N_categoria == "20" & !is.na(Combate))

df <- p3 %>%
  arrange(Data) %>%
  group_by(Personagem) %>%
  mutate(Acumulado = cumsum(ifelse(is.na(Quantidade), 0, Quantidade))) %>%
  ungroup()

g_acum <- ggplot(df, aes(x = Data, y = Acumulado, color = Personagem)) +
  geom_line(size = 1.2) +
  geom_point(aes(size = Quantidade), alpha = 0.7) +
  scale_x_date(date_labels = "%b/%Y") +
  scale_color_manual(values = cores_personagens(length(unique(df$Personagem)))) +
  scale_size_continuous(range = c(1, 5), name = "Quantidade") +
  labs(title = "Acumulado de Adversários por Personagem",
       x = "Data do Combate", y = "Número de Adversários") +
  tema_rpg() +
  theme(legend.position = "right") +
  guides(color = guide_legend(title = "Personagem", ncol = 1, override.aes = list(size = 3)))

print(g_acum)
salvar_pdf(g_acum, "02_acumulado_adversarios")

# ============================================================================
# 6.3. DIAS DA SEMANA
# ============================================================================

cat("📊 04. Dias da Semana...\n")

p4_semana <- Data2 %>%
  filter(!is.na(Personagem), N_categoria == "20") %>%
  distinct(Evento, Personagem, .keep_all = TRUE) %>%
  mutate(
    Dia_Semana = wday(Data2, label = TRUE, abbr = TRUE, locale = "pt_BR.UTF-8")
  ) %>%
  filter(!is.na(Data2))

# Verificar valores de Presencial
cat("Valores de Presencial:\n")
print(unique(p4_semana$Presencial))
print(table(p4_semana$Presencial, useNA = "ifany"))

# Criar paleta de cores com base nos valores únicos
valores_presencial <- unique(p4_semana$Presencial)
n_cores <- length(valores_presencial)

# Definir cores para cada valor
if(n_cores <= 3) {
  cores_presencial <- c("#2E8B57", "#E74C3C", "#95A5A6")[1:n_cores]
} else {
  cores_presencial <- rainbow(n_cores)
}
names(cores_presencial) <- valores_presencial

g_semana <- ggplot(p4_semana, aes(x = Dia_Semana, fill = Presencial)) +
  geom_bar() +
  facet_wrap(~ Personagem) +
  labs(
    title = "Distribuição de jogadas por dia da semana",
    x = "Dia da Semana",
    y = "Contagem"
  ) +
  scale_fill_manual(values = cores_presencial, name = "Presencial") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(title = "Presencial", ncol = 1))

print(g_semana)
salvar_pdf(g_semana, "04_dias_semana")

# ============================================================================
# 6.4. CALENDÁRIO DE JOGO
# ============================================================================

cat("📊 05. Calendário de Jogo...\n")

matriz_presenca <- p4 %>%
  select(Personagem, Dia_Jogo) %>%
  mutate(Presente = 1) %>%
  unique()

g_calendario <- ggplot(matriz_presenca, aes(x = Dia_Jogo, y = Personagem, fill = factor(Presente))) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_manual(values = c("1" = "#2E8B57"), labels = c("Jogo"), name = "Atividade") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = "Matriz de Presença - Dias de Jogo por Personagem",
       x = "Data", y = "Personagem") +
  tema_rpg() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    legend.position = "none"  # <-- REMOVE A LEGENDA
  )

print(g_calendario)
salvar_pdf(g_calendario, "05_calendario_jogo", largura = 15, altura = 8)

# ============================================================================
# 6.5. OVERLAP TEMPORAL
# ============================================================================

cat("📊 06. Overlap Temporal...\n")

p3_overlap <- Data2 %>%
  filter(!is.na(Personagem), N_categoria == "20") %>%
  distinct(Evento, .keep_all = TRUE)

g_overlap <- ggplot(p3_overlap, aes(x = Data2, fill = Personagem)) +
  geom_area(stat = "bin", bins = 30, alpha = 0.7, position = "stack") +
  scale_fill_manual(values = cores_personagens(length(unique(p3_overlap$Personagem)))) +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "1 month") +
  labs(title = "Distribuição Temporal de Eventos", x = "Data da Sessão", y = "Número de Eventos") +
  tema_rpg() +
  theme(legend.position = "right") +
  guides(fill = guide_legend(title = "Personagem", ncol = 1))

print(g_overlap)
salvar_png(g_overlap, "06_overlap_temporal", largura = 12, altura = 8)

# ============================================================================
# 6.6. RELAÇÃO ENTRE DATAS
# ============================================================================

cat("📊 07. Relação entre Datas...\n")

library(ggside)

p3_datas <- Data2 %>%
  filter(!is.na(Personagem), N_categoria == "20", Ano_A >= 1330) %>%
  distinct(Evento, .keep_all = TRUE) %>%
  mutate(Data = as.Date(Data), Data2 = as.Date(Data2)) %>%
  filter(!is.na(Data), !is.na(Data2))

if(!"Continente" %in% colnames(p3_datas)) {
  p3_datas$Continente <- sample(c("África", "América", "Ásia", "Europa", "Oceania"), 
                                 nrow(p3_datas), replace = TRUE)
}

# Definir paleta de cores
n_personagens <- length(unique(p3_datas$Personagem))
cores_personagens2 <- rainbow(n_personagens)

g_rota <- ggplot(p3_datas, aes(x = Data2, y = Data)) + 
  # Pontos principais
  geom_jitter(aes(colour = Personagem, shape = Categoria, size = Continente), alpha = 0.7) + 
  
  # Linha de tendência
  geom_smooth(method = loess, se = FALSE, alpha = 0.5, aes(colour = Personagem)) + 
  
  # Escalas
  scale_shape_manual(values = 0:10, name = "Categoria") +
  scale_size_discrete(range = c(2, 6), name = "Continente") +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "2 months") +
  scale_y_date(date_labels = "%b/%Y", date_breaks = "2 months") +
  
  # ===== GRÁFICOS LATERAIS =====
  geom_ysideboxplot(aes(fill = Personagem), alpha = 0.4, width = 0.6) + 
  geom_xsidebar(aes(fill = Personagem), 
                position = "stack", 
                stat = "count",
                alpha = 0.4, 
                orientation = "x",
                width = 1) +  
  
  # Rótulos
  labs(
    title = "Relação entre Data de Jogo e Data em Arton",
    subtitle = "Tamanho do ponto indica o continente",
    x = "Data da Sessão",
    y = "Data em Arton",
    colour = "Personagem",
    shape = "Categoria"
  ) +  
  
  # Cores
  scale_fill_manual(values = cores_personagens2) +
  scale_colour_manual(values = cores_personagens2) +
  
  # Tema
  tema_rpg() +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    ggside.panel.scale.x = 0.3,
    ggside.panel.scale.y = 0.3
  ) +
  guides(
    colour = guide_legend(title = "Personagem", ncol = 1, override.aes = list(size = 3)),
    fill = guide_legend(title = "Personagem", ncol = 1),
    shape = guide_legend(title = "Categoria", ncol = 1),
    size = guide_legend(title = "Continente", ncol = 1)
  )

print(g_rota)
salvar_pdf(g_rota, "07_relacao_datas", largura = 20, altura = 13)

# ============================================================================
# 7. GRÁFICOS - PERSONAGENS E ATRIBUTOS
# ============================================================================

# ============================================================================
# 7.1. ATRIBUTOS - PERFIL (Linhas) - ORDEM CORRIGIDA
# ============================================================================

cat("📊 08. Atributos - Perfil...\n")

# Nomes das colunas na planilha (abreviados)
colunas_atributos <- c("For", "Des", "Con", "Int", "Sab", "Car")

# Nomes completos para exibição (NA ORDEM CORRETA)
nomes_completos <- c("Força", "Destreza", "Constituição", "Inteligência", "Sabedoria", "Carisma")

# Verificar quais colunas existem
atributos_existentes <- colunas_atributos[colunas_atributos %in% names(pbase)]

if(length(atributos_existentes) >= 4) {
  
  # ===== FILTRAR APENAS G5.1 E G5.3 =====
  df_atributos <- pbase %>%
    filter(Grupo %in% c("G5.1", "G5.3"), !is.na(Personagem)) %>%
    group_by(Personagem) %>%
    summarise(across(all_of(atributos_existentes), sum, na.rm = TRUE), .groups = "drop")
  
  # Renomear colunas para nomes completos
  for(i in seq_along(atributos_existentes)) {
    col_abrev <- atributos_existentes[i]
    col_completo <- nomes_completos[which(colunas_atributos == col_abrev)]
    names(df_atributos)[names(df_atributos) == col_abrev] <- col_completo
  }
  
  # Lista de atributos completos que existem
  atributos_completos <- nomes_completos[colunas_atributos %in% atributos_existentes]
  
  # Transformar para formato longo
  df_linhas_atributos <- df_atributos %>%
    pivot_longer(cols = all_of(atributos_completos), names_to = "Atributo", values_to = "Valor")
  
  # Definir a ordem correta dos atributos
  ordem_atributos <- c("Força", "Destreza", "Constituição", "Inteligência", "Sabedoria", "Carisma")
  
  # Filtrar apenas os atributos que existem, mantendo a ordem
  ordem_atributos_existentes <- ordem_atributos[ordem_atributos %in% atributos_completos]
  
  # Aplicar a ordem como fator
  df_linhas_atributos$Atributo <- factor(df_linhas_atributos$Atributo, levels = ordem_atributos_existentes)
  
  # Gráfico
  g_atributos <- ggplot(df_linhas_atributos, aes(x = Atributo, y = Valor, color = Personagem, group = Personagem)) +
    geom_line(size = 1.5) +
    geom_point(size = 5, alpha = 0.9) +
    geom_text(aes(label = round(Valor, 1)), vjust = -1.2, size = 4.5, fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = cores_personagens(length(unique(df_linhas_atributos$Personagem)))) +
    labs(
      title = "Perfil de Atributos dos Personagens (G5.1 e G5.3)",
      subtitle = paste("Somatório total de cada atributo (", length(unique(df_linhas_atributos$Personagem)), " personagens)", sep = ""),
      x = "Atributo",
      y = "Valor Total",
      color = "Personagem"
    ) +
    tema_rpg() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 12),
      legend.position = "right"
    ) +
    guides(color = guide_legend(title = "Personagem", ncol = 1, override.aes = list(size = 4)))
  
  print(g_atributos)
  salvar_pdf(g_atributos, "08_atributos_perfil")
  
  # Exibir dados
  cat("\n📊 Valores dos atributos (somatório por personagem):\n")
  print(df_atributos)
  
  cat("\n📈 Total de personagens:", nrow(df_atributos), "\n")
  
} else {
  cat("⚠️  Colunas de atributos não encontradas\n")
  cat("📋 Colunas disponíveis:", paste(names(pbase), collapse = ", "), "\n")
}
# ============================================================================
# 7.2. COMBATE - PERFIL (Linhas)
# ============================================================================

cat("📊 09. Combate - Perfil...\n")

colunas_combate <- c("Vida", "Golpe", "Ataque", "Defesa", "Ataque_M", "Defesa_M")
nomes_combate <- c("Vida", "Golpe", "Ataque", "Defesa", "Ataque Mágico", "Defesa Mágica")
combate_existentes <- colunas_combate[colunas_combate %in% names(pbase)]

if(length(combate_existentes) >= 3) {
  
  # ===== FILTRAR G5.1 E G5.3 =====
  df_combate <- pbase %>%
    filter(Grupo %in% c("G5.1", "G5.3"), !is.na(Personagem), T_Atributo != "Dano") %>%
    group_by(Personagem) %>%
    summarise(across(all_of(combate_existentes), sum, na.rm = TRUE), .groups = "drop")
  
  df_exibicao <- df_combate
  names(df_exibicao)[names(df_exibicao) == "Ataque_M"] <- "Ataque Mágico"
  names(df_exibicao)[names(df_exibicao) == "Defesa_M"] <- "Defesa Mágica"
  
  df_linhas_combate <- df_exibicao %>%
    pivot_longer(cols = -Personagem, names_to = "Atributo", values_to = "Valor")
  
  ordem_atributos <- c("Vida", "Golpe", "Ataque", "Ataque Mágico", "Defesa", "Defesa Mágica")
  df_linhas_combate$Atributo <- factor(df_linhas_combate$Atributo, levels = ordem_atributos)
  
  g_combate <- ggplot(df_linhas_combate, aes(x = Atributo, y = Valor, color = Personagem, group = Personagem)) +
    geom_line(size = 1.5) +
    geom_point(size = 5, alpha = 0.9) +
    geom_text(aes(label = round(Valor, 1)), vjust = -1.2, size = 4.5, fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = cores_personagens(length(unique(df_linhas_combate$Personagem)))) +
    labs(
      title = "Perfil de Combate dos Personagens (G5.1 e G5.3)",
      subtitle = paste("Somatório total de cada atributo (", length(unique(df_linhas_combate$Personagem)), " personagens)", sep = ""),
      x = "Atributo",
      y = "Valor Total",
      color = "Personagem"
    ) +
    tema_rpg() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 11),
      legend.position = "right"
    ) +
    guides(color = guide_legend(title = "Personagem", ncol = 1, override.aes = list(size = 4)))
  
  print(g_combate)
  salvar_pdf(g_combate, "09_combate_perfil")
  
  # Exibir dados
  cat("\n📊 Valores dos atributos de combate (somatório por personagem):\n")
  print(df_exibicao)
  
  cat("\n📈 Total de personagens:", nrow(df_exibicao), "\n")
  
} else {
  cat("⚠️  Colunas de combate não encontradas\n")
  cat("📋 Colunas disponíveis:", paste(names(pbase), collapse = ", "), "\n")
}
# ============================================================================
# 7.3. ATRIBUTOS ACUMULADOS
# ============================================================================

cat("📊 10. Atributos - Crescimento Acumulado por Semana...\n")

library(lubridate)

p3_atrib <- Data2 %>%
  filter(N_categoria == 20, !is.na(Atributos) & Atributos > 0.01) %>%
  arrange(Personagem, Data2)

if(nrow(p3_atrib) > 0) {
  
  # Agrupar por semana e acumular
  p3_semana <- p3_atrib %>%
    mutate(Semana = floor_date(Data2, unit = "week")) %>%
    group_by(Personagem, Semana) %>%
    summarise(Atributos_Semana = sum(Atributos, na.rm = TRUE), .groups = "drop") %>%
    arrange(Personagem, Semana) %>%
    group_by(Personagem) %>%
    mutate(Acumulado = cumsum(Atributos_Semana)) %>%
    ungroup()
  
  # Gráfico
  g_atrib_final <- ggplot(p3_semana, aes(x = Semana, y = Acumulado, color = Personagem)) +
    geom_line(size = 1.5) +
    geom_point(size = 4, alpha = 0.8) +
    geom_text(
      aes(label = round(Acumulado, 1)),
      vjust = -1.2,
      size = 3.5,
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_color_manual(values = cores_personagens(length(unique(p3_semana$Personagem)))) +
    scale_x_date(date_labels = "%d/%m", date_breaks = "1 week") +
    labs(
      title = "Crescimento Acumulado de Atributos",
      subtitle = "Atributos somados por semana e acumulados a partir do zero",
      x = "Semana",
      y = "Acúmulo de Atributos",
      color = "Personagem"
    ) +
    tema_rpg() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.box = "vertical"
    ) +
    guides(color = guide_legend(title = "Personagem", ncol = 3))
  
  print(g_atrib_final)
  salvar_pdf(g_atrib_final, "10_atributos_crescimento_semana")
  
}
# ============================================================================
# 7.4. DASHBOARD - ATRIBUTOS (Heatmap + Perfil)
# ============================================================================

cat("📊 11. Dashboard - Atributos...\n")

if(exists("df_atributos") && nrow(df_atributos) > 0 && exists("df_combate") && nrow(df_combate) > 0) {
  
  # Preparar dados combinados
  df_atributos_long <- df_atributos %>%
    pivot_longer(cols = -Personagem, names_to = "Atributo", values_to = "Valor")
  
  df_combate_long <- df_exibicao %>%
    pivot_longer(cols = -Personagem, names_to = "Atributo", values_to = "Valor")
  
  # Heatmap de atributos
  g_heatmap <- ggplot(df_atributos_long, aes(x = Atributo, y = Personagem, fill = Valor)) +
    geom_tile(color = "white", size = 1.5) +
    geom_text(aes(label = round(Valor, 1)), size = 4.5, fontface = "bold", color = "white") +
    scale_fill_gradient2(low = "#E74C3C", mid = "#F1C40F", high = "#2ECC71", 
                         midpoint = median(df_atributos_long$Valor), name = "Valor") +
    labs(title = "Atributos dos Personagens", x = "", y = "") +
    tema_rpg() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"))
  
  # Heatmap de combate
  g_heatmap_combate <- ggplot(df_combate_long, aes(x = Atributo, y = Personagem, fill = Valor)) +
    geom_tile(color = "white", size = 1.5) +
    geom_text(aes(label = round(Valor, 1)), size = 4.5, fontface = "bold", color = "white") +
    scale_fill_gradient2(low = "#E74C3C", mid = "#F1C40F", high = "#2ECC71",
                         midpoint = median(df_combate_long$Valor), name = "Valor") +
    labs(title = "Atributos de Combate", x = "", y = "") +
    tema_rpg() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"))
  
  # Dashboard atributos
  dashboard_atributos <- (g_heatmap | g_heatmap_combate) +
    plot_annotation(
      title = "📊 Atributos e Combate dos Personagens",
      subtitle = "Valores somados por personagem",
      theme = theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18, color = "#2C3E50"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#7F8C8D"),
        plot.background = element_rect(fill = "white", color = NA)
      )
    )
  
  print(dashboard_atributos)
  salvar_pdf(dashboard_atributos, "11_dashboard_atributos", largura = 22, altura = 10)
}

# ============================================================================
# 7.5. HABILIDADES
# ============================================================================

cat("📊 12. Habilidades...\n")

p3_hab <- pbase %>%
  filter(
    Grupo %in% c("G5.1", "G5.3"),  # <-- FILTRO DE GRUPO
    !is.na(Personagem),
    N_categoria == "20",
    Categoria == "Combate",
    Equipado == 1,
    T_Atributo != "Dano"
  )

if(nrow(p3_hab) > 0) {
  
  cat("  Personagens encontrados:", paste(unique(p3_hab$Personagem), collapse = ", "), "\n")
  cat("  Total de registros:", nrow(p3_hab), "\n")
  
  g_habilidades <- ggplot(p3_hab, aes(x = N_Hab, y = Bonus, color = Personagem, shape = Classe)) +
    geom_point(size = 4, alpha = 0.75) +
    geom_text_repel(aes(label = D_bonus), size = 2.8, box.padding = 0.4,
                    max.overlaps = 30, show.legend = FALSE, fontface = "bold") +
    facet_wrap(~ T_bonus, ncol = 1, scales = "free_y") +
    scale_color_manual(values = cores_personagens(length(unique(p3_hab$Personagem)))) +
    scale_shape_manual(values = shapes_personalizados[1:length(unique(p3_hab$Classe))]) +
    labs(
      title = "Distribuição de Habilidades por Personagem (G5.1 e G5.3)",
      subtitle = paste("Nível vs Bônus das habilidades equipadas (", length(unique(p3_hab$Personagem)), " personagens)", sep = ""),
      x = "Nível da Habilidade",
      y = "Bônus",
      color = "Personagem",
      shape = "Classe"
    ) +
    tema_rpg() +
    theme(
      legend.position = "right",
      strip.text = element_text(face = "bold", size = 11)
    ) +
    guides(
      color = guide_legend(title = "Personagem", ncol = 1, override.aes = list(size = 3)),
      shape = guide_legend(title = "Classe", ncol = 1)
    )
  
  print(g_habilidades)
  salvar_pdf(g_habilidades, "12_habilidades_personagem")
  
} else {
  cat("⚠️  Nenhum dado encontrado para Habilidades\n")
}


# ============================================================================
# 7.6. ITENS ESPECIAIS
# ============================================================================

cat("📊 13. Itens Especiais...\n")

p3_itens <- pbase %>%
  filter(
    Grupo %in% c("G5.1", "G5.3"),  # <-- FILTRO DE GRUPO
    !is.na(Personagem),
    N_categoria == "20",
    Categoria == "Item",
    Equipado == 1,
    Classe_I != "Item"
  ) %>%
  distinct(Personagem, D_bonus, .keep_all = TRUE)

if(nrow(p3_itens) > 0) {
  
  cat("  Personagens encontrados:", paste(unique(p3_itens$Personagem), collapse = ", "), "\n")
  cat("  Total de registros:", nrow(p3_itens), "\n")
  
  g_itens <- ggplot(p3_itens, aes(x = N_Hab, y = Bonus, color = Personagem, shape = Classe_M)) +
    geom_point(size = 4, alpha = 0.75) +
    geom_text_repel(aes(label = D_bonus), size = 2.8, box.padding = 0.4,
                    max.overlaps = 30, show.legend = FALSE, fontface = "bold") +
    facet_wrap(~ Classe_I, ncol = 1, scales = "free_y") +
    scale_color_manual(values = cores_personagens(length(unique(p3_itens$Personagem)))) +
    scale_shape_manual(values = shapes_personalizados[1:length(unique(p3_itens$Classe_M))]) +
    labs(
      title = "Itens Especiais por Personagem (G5.1 e G5.3)",
      subtitle = paste("Nível vs Bônus dos itens especiais equipados (", length(unique(p3_itens$Personagem)), " personagens)", sep = ""),
      x = "Nível do Item",
      y = "Bônus",
      color = "Personagem",
      shape = "Classe Mágica"
    ) +
    tema_rpg() +
    theme(
      legend.position = "right",
      strip.text = element_text(face = "bold", size = 11)
    ) +
    guides(
      color = guide_legend(title = "Personagem", ncol = 1, override.aes = list(size = 3)),
      shape = guide_legend(title = "Classe Mágica", ncol = 1)
    )
  
  print(g_itens)
  salvar_pdf(g_itens, "13_itens_especiais")
  
} else {
  cat("⚠️  Nenhum dado encontrado para Itens Especiais\n")
}

# ============================================================================
# 7.7. OUTROS ITENS
# ============================================================================

cat("📊 14. Outros Itens...\n")

p3_outros <- pbase %>%
  filter(
    Grupo %in% c("G5.1", "G5.3"),  # <-- FILTRO DE GRUPO
    !is.na(Personagem),
    N_categoria == "20",
    Categoria == "Item",
    Equipado == 1,
    Classe_I == "Item"
  ) %>%
  distinct(Personagem, D_bonus, .keep_all = TRUE)

if(nrow(p3_outros) > 0) {
  
  cat("  Personagens encontrados:", paste(unique(p3_outros$Personagem), collapse = ", "), "\n")
  cat("  Total de registros:", nrow(p3_outros), "\n")
  
  g_outros <- ggplot(p3_outros, aes(x = Valor, y = Bonus, color = Personagem, shape = T_bonus)) +
    geom_point(alpha = 0.75, size = 4) +
    geom_text_repel(aes(label = D_bonus), size = 2.8, box.padding = 0.4,
                    max.overlaps = 30, show.legend = FALSE, fontface = "bold") +
    scale_color_manual(values = cores_personagens(length(unique(p3_outros$Personagem)))) +
    scale_shape_manual(values = shapes_personalizados[1:length(unique(p3_outros$T_bonus))]) +
    labs(
      title = "Outros Itens por Personagem (G5.1 e G5.3)",
      subtitle = paste("Valor vs Bônus dos itens comuns equipados (", length(unique(p3_outros$Personagem)), " personagens)", sep = ""),
      x = "Valor do Item",
      y = "Bônus",
      color = "Personagem",
      shape = "Tipo de Bônus"
    ) +
    tema_rpg() +
    theme(legend.position = "right") +
    guides(
      color = guide_legend(title = "Personagem", ncol = 1, override.aes = list(size = 3)),
      shape = guide_legend(title = "Tipo de Bônus", ncol = 1)
    )
  
  print(g_outros)
  salvar_pdf(g_outros, "14_outros_itens")
  
} else {
  cat("⚠️  Nenhum dado encontrado para Outros Itens\n")
}

# ============================================================================
# 8. GRÁFICOS - RELIGIÃO E TENDÊNCIA
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("📊 RELIGIÃO E TENDÊNCIA\n")
cat(rep("=", 70), "\n\n", sep = "")

# ============================================================================
# 8.1. TENDÊNCIA DOS PERSONAGENS
# ============================================================================

cat("📊 15. Tendência dos Personagens...\n")

p3_tend <- Data2 %>%
  separate_rows(Patrono, sep = "/") %>%
  mutate(
    Tendência = case_when(
      Patrono %in% c("Azgher", "Gloriénn", "Khalmyr", "Lenna", "Lin-Wu", "Marah", "Tauron", "Tyathis") ~ 1,
      Patrono %in% c("Alihanna", "Grande Oceano", "Nimb", "Tanna-Toh", "Valkaria", "Wynna") ~ 0,
      Patrono %in% c("Aharadak", "Hyninn", "Kallyadranock", "Kenn", "Megalock", "Ragnar", "Sszzass", "Tenebra") ~ -1,
      TRUE ~ NA_real_
    ),
    Lealdade = case_when(
      Patrono %in% c("Azgher", "Gloriénn", "Khalmyr", "Lenna", "Lin-Wu", "Marah", "Tauron", "Tyathis", "Valkaria") ~ 1,
      Patrono %in% c("Kallyadranock", "Tanna-Toh", "Wynna") ~ 0,
      Patrono %in% c("Aharadak", "Alihanna", "Grande Oceano", "Hyninn", "Kenn", "Megalock", "Nimb", "Ragnar", "Sszzass", "Tenebra") ~ -1,
      TRUE ~ NA_real_
    )
  )

dados_tend <- p3_tend %>%
  group_by(Personagem) %>%
  summarise(
    Tendência_media = mean(Tendência, na.rm = TRUE),
    Lealdade_media = mean(Lealdade, na.rm = TRUE),
    Contagem = n(),
    .groups = 'drop'
  ) %>%
  filter(!is.na(Tendência_media), !is.na(Lealdade_media))

g_tendencia <- ggplot(dados_tend, aes(x = Tendência_media, y = Lealdade_media)) +
  geom_point(aes(size = Contagem, color = Personagem), alpha = 0.85) +
  geom_text_repel(aes(label = Personagem, color = Personagem),
                  size = 3.5, max.overlaps = 20, fontface = "bold", show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5, color = "#95A5A6") +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5, color = "#95A5A6") +
  scale_x_continuous(limits = c(-1.5, 1.5), breaks = c(-1, 0, 1), labels = c("Caótico", "Neutro", "Leal")) +
  scale_y_continuous(limits = c(-1.5, 1.5), breaks = c(-1, 0, 1), labels = c("Mal", "Neutro", "Bom")) +
  scale_color_manual(values = cores_personagens(nrow(dados_tend))) +
  scale_size_continuous(range = c(3, 10), name = "Registros") +
  labs(title = "Tendência e Lealdade dos Personagens", x = "Tendência (Caótico → Leal)", y = "Lealdade (Mal → Bom)") +
  tema_rpg() +
  theme(legend.position = "right") +
  guides(color = guide_legend(title = "Personagem", ncol = 1, override.aes = list(size = 3)))

print(g_tendencia)
salvar_pdf(g_tendencia, "15_tendencia_personagens")

# ============================================================================
# 8.2. PCA - PATRONOS VS PERSONAGENS
# ============================================================================

cat("📊 16. PCA - Patronos vs Personagens...\n")

personagens_selecionados <- c("Artemisia Asteracea", "Darius Ravnus", "Tulipa Vale", "Acheron Yudennach", "Bryn", "Zirk", "Elian Solhart")
# ================================================

cat("📊 16. PCA - Patronos vs Personagens...\n")
cat("   Personagens selecionados:", paste(personagens_selecionados, collapse = ", "), "\n")

p3_pca <- Data2 %>%
  separate_rows(Esfera, sep = "/") %>%
  separate_rows(Patrono, sep = "/") %>%
  filter(!is.na(Personagem), !is.na(Patrono), !is.na(Esfera), N_categoria == "20",
         Personagem %in% personagens_selecionados)

if(nrow(p3_pca) > 0) {
  # ... resto do código
}

p3_pca <- Data2 %>%
  separate_rows(Esfera, sep = "/") %>%
  separate_rows(Patrono, sep = "/") %>%
  filter(!is.na(Personagem), !is.na(Patrono), !is.na(Esfera), N_categoria == "20",
         Personagem %in% personagens_selecionados)

if(nrow(p3_pca) > 0) {
  local <- dcast(p3_pca, Patrono + Personagem ~ Esfera,
                 value.var = "N_categoria", fun.aggregate = sum, na.rm = TRUE)
  local <- local[complete.cases(local), ]
  
  local_numeric <- local %>% select(where(is.numeric))
  pca_res <- prcomp(local_numeric, scale. = TRUE)
  
  pca_scores <- as.data.frame(pca_res$x)
  pca_scores$Personagem <- local$Personagem
  pca_scores$Patrono <- local$Patrono
  
  hull_por_personagem <- pca_scores %>%
    group_by(Personagem) %>%
    slice(chull(PC1, PC2)) %>%
    ungroup()
  
  g_pca <- ggplot(pca_scores, aes(x = PC1, y = PC2)) +
    geom_polygon(data = hull_por_personagem,
                 aes(color = Personagem, group = Personagem, fill = Personagem),
                 alpha = 0.1, size = 1) +
    geom_jitter(aes(color = Personagem, shape = Patrono), size = 4, alpha = 0.8) +
    geom_text_repel(aes(label = Patrono, color = Personagem),
                    size = 3.2, max.overlaps = 20, show.legend = FALSE, fontface = "bold") +
    scale_color_manual(values = cores_personagens(length(unique(pca_scores$Personagem)))) +
    scale_fill_manual(values = cores_personagens(length(unique(pca_scores$Personagem)))) +
    scale_shape_manual(values = shapes_personalizados) +
    labs(title = "PCA - Patronos por Personagem", color = "Personagem", shape = "Patrono") +
    tema_rpg() +
    theme(legend.position = "right") +
    guides(color = guide_legend(title = "Personagem", ncol = 1, override.aes = list(size = 3)))
  
  print(g_pca)
  salvar_pdf(g_pca, "16_pca_patronos_personagens")
}

# ============================================================================
# 8.3. PCA - ESFERAS VS PERSONAGENS
# ============================================================================

cat("📊 17. PCA - Esferas vs Personagens...\n")

if(exists("p3_pca") && nrow(p3_pca) > 0) {
  local <- dcast(p3_pca, Esfera + Personagem ~ Patrono,
                 value.var = "N_categoria", fun.aggregate = sum, na.rm = TRUE)
  local <- local[complete.cases(local), ]
  
  local_numeric <- local %>% select(where(is.numeric))
  pca_res <- prcomp(local_numeric, scale. = TRUE)
  
  pca_scores <- as.data.frame(pca_res$x)
  pca_scores$Personagem <- local$Personagem
  pca_scores$Esfera <- local$Esfera
  
  hull_por_personagem <- pca_scores %>%
    group_by(Personagem) %>%
    slice(chull(PC1, PC2)) %>%
    ungroup()
  
  g_pca2 <- ggplot(pca_scores, aes(x = PC1, y = PC2)) +
    geom_polygon(data = hull_por_personagem,
                 aes(color = Personagem, group = Personagem, fill = Personagem),
                 alpha = 0.1, size = 1) +
    geom_jitter(aes(color = Personagem, shape = Esfera), size = 4, alpha = 0.8) +
    geom_text_repel(aes(label = Esfera, color = Personagem),
                    size = 3.2, max.overlaps = 20, show.legend = FALSE, fontface = "bold") +
    scale_color_manual(values = cores_personagens(length(unique(pca_scores$Personagem)))) +
    scale_fill_manual(values = cores_personagens(length(unique(pca_scores$Personagem)))) +
    scale_shape_manual(values = shapes_personalizados) +
    labs(title = "PCA - Esferas por Personagem", color = "Personagem", shape = "Esfera") +
    tema_rpg() +
    theme(legend.position = "right") +
    guides(color = guide_legend(title = "Personagem", ncol = 1, override.aes = list(size = 3)))
  
  print(g_pca2)
  salvar_pdf(g_pca2, "17_pca_esferas_personagens")
}

# ============================================================================
# 8.4. CONSTELAÇÃO DE PERSONAGENS
# ============================================================================

cat("📊 18. Constelação de Personagens...\n")

p3_const <- Data2 %>%
  separate_rows(Esfera, sep = "/") %>%
  separate_rows(Patrono, sep = "/") %>%
  filter(!is.na(Personagem), !is.na(Patrono), !is.na(Esfera), N_categoria == "20")

criar_pca_completo <- function(data, var_setas, titulo, sem_legenda = FALSE) {
  
  local <- dcast(data, Personagem ~ get(var_setas),
                 value.var = "N_categoria", fun.aggregate = sum, na.rm = TRUE)
  local <- local[complete.cases(local), ]
  local_numeric <- local %>% select(where(is.numeric))
  
  if(nrow(local_numeric) < 2) return(NULL)
  
  pca_res <- prcomp(local_numeric, scale. = TRUE)
  
  pca_scores <- as.data.frame(pca_res$x)
  pca_scores$Personagem <- local$Personagem
  
  hull_por_personagem <- pca_scores %>%
    group_by(Personagem) %>%
    filter(n() >= 3) %>%
    slice(chull(PC1, PC2)) %>%
    ungroup()
  
  loadings_df <- as.data.frame(pca_res$rotation * 3)
  loadings_df$label <- rownames(loadings_df)
  
  g <- ggplot(pca_scores, aes(x = PC1, y = PC2)) +
    geom_polygon(data = hull_por_personagem,
                 aes(color = Personagem, group = Personagem, fill = Personagem),
                 alpha = 0.1, size = 1) +
    geom_point(aes(color = Personagem), size = 3) +
    geom_text_repel(aes(label = Personagem, color = Personagem),
                    size = 3.5, max.overlaps = 15, fontface = "bold", show.legend = FALSE) +
    geom_segment(data = loadings_df,
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.15, "cm")),
                 color = "#7F8C8D", alpha = 0.6) +
    geom_text(data = loadings_df,
              aes(x = PC1 * 1.05, y = PC2 * 1.05, label = label),
              size = 3, color = "#2C3E50", fontface = "bold") +
    scale_color_manual(values = cores_personagens(length(unique(pca_scores$Personagem)))) +
    scale_fill_manual(values = cores_personagens(length(unique(pca_scores$Personagem)))) +
    labs(title = titulo, x = "PC1", y = "PC2") +
    tema_rpg() +
    guides(fill = "none")
  
  # Se sem_legenda = TRUE, remove a legenda
  if(sem_legenda) {
    g <- g + theme(legend.position = "none")
  } else {
    g <- g + theme(legend.position = "right") +
      guides(color = guide_legend(title = "Personagem", ncol = 1, override.aes = list(size = 3)))
  }
  
  return(g)
}

q1 <- criar_pca_completo(p3_const, "Patrono", "Personagens × Deuses", sem_legenda = TRUE)
q2 <- criar_pca_completo(p3_const, "Esfera", "Personagens × Esferas", sem_legenda = FALSE)

if(!is.null(q1) && !is.null(q2)) {
  constelacao <- q1 + q2 + 
    plot_annotation(title = "Constelação de Personagens",
                    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18, color = "#2C3E50")))
  print(constelacao)
  salvar_pdf(constelacao, "18_constelacao_personagens", largura = 22, altura = 10)
}

# ============================================================================
# 8.5. CONSTELAÇÃO INVERTIDA
# ============================================================================

cat("📊 19. Constelação Invertida...\n")

# ============================================================================
# PCA para Patrono (GRÁFICO DA ESQUERDA - SEM LEGENDA)
# ============================================================================

local_patrono <- dcast(p3_const, Patrono ~ Personagem,
                       value.var = "N_categoria", fun.aggregate = sum, na.rm = TRUE)
local_patrono <- local_patrono[complete.cases(local_patrono), ]
local_numeric_patrono <- local_patrono %>% select(where(is.numeric))
pca_res_patrono <- prcomp(local_numeric_patrono, scale. = TRUE)

pca_scores_patrono <- as.data.frame(pca_res_patrono$x)
pca_scores_patrono$Patrono <- local_patrono$Patrono

loadings_patrono <- as.data.frame(pca_res_patrono$rotation * 3)
loadings_patrono$label <- rownames(loadings_patrono)

q1_inv <- ggplot(pca_scores_patrono, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Patrono), size = 3) +
  geom_text_repel(aes(label = Patrono, color = Patrono),
                  size = 3.5, max.overlaps = 15, fontface = "bold", show.legend = FALSE) +
  geom_segment(data = loadings_patrono,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.15, "cm")),
               color = "#7F8C8D", alpha = 0.6) +
  geom_text(data = loadings_patrono,
            aes(x = PC1 * 1.05, y = PC2 * 1.05, label = label),
            size = 3, color = "#2C3E50", fontface = "bold") +
  scale_color_manual(values = cores_personagens(length(unique(pca_scores_patrono$Patrono)))) +
  labs(title = "Patronos × Personagens") +
  tema_rpg() +
  theme(legend.position = "none")  # SEM LEGENDA

# ============================================================================
# PCA para Esfera (GRÁFICO DA DIREITA - SEM LEGENDA)
# ============================================================================

local_esfera <- dcast(p3_const, Esfera ~ Personagem,
                      value.var = "N_categoria", fun.aggregate = sum, na.rm = TRUE)
local_esfera <- local_esfera[complete.cases(local_esfera), ]
local_numeric_esfera <- local_esfera %>% select(where(is.numeric))
pca_res_esfera <- prcomp(local_numeric_esfera, scale. = TRUE)

pca_scores_esfera <- as.data.frame(pca_res_esfera$x)
pca_scores_esfera$Esfera <- local_esfera$Esfera

loadings_esfera <- as.data.frame(pca_res_esfera$rotation * 3)
loadings_esfera$label <- rownames(loadings_esfera)

q2_inv <- ggplot(pca_scores_esfera, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = Esfera), size = 3) +
  geom_text_repel(aes(label = Esfera, color = Esfera),
                  size = 3.5, max.overlaps = 15, fontface = "bold", show.legend = FALSE) +
  geom_segment(data = loadings_esfera,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.15, "cm")),
               color = "#7F8C8D", alpha = 0.6) +
  geom_text(data = loadings_esfera,
            aes(x = PC1 * 1.05, y = PC2 * 1.05, label = label),
            size = 3, color = "#2C3E50", fontface = "bold") +
  scale_color_manual(values = cores_personagens(length(unique(pca_scores_esfera$Esfera)))) +
  labs(title = "Esferas × Personagens") +
  tema_rpg() +
  theme(legend.position = "none")  # SEM LEGENDA

# ============================================================================
# COMBINAR GRÁFICOS
# ============================================================================

constelacao_inv <- q1_inv + q2_inv +
  plot_annotation(
    title = "Constelação Invertida",
    subtitle = "Patronos × Personagens (esq) e Esferas × Personagens (dir)",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18, color = "#2C3E50"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#7F8C8D"),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

print(constelacao_inv)
salvar_pdf(constelacao_inv, "19_constelacao_invertida", largura = 22, altura = 10)

# ============================================================================
# 9. GRÁFICOS - MAPA E LOCALIZAÇÃO
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("🗺️ MAPA E LOCALIZAÇÃO\n")
cat(rep("=", 70), "\n\n", sep = "")

# ============================================================================
# 9.1. MAPA LOCAL
# ============================================================================

cat("📊 21. Mapa Local...\n")

p3_mapa <- Data2 %>%
  filter(!is.na(Reino), !is.na(Personagem), !is.na(Bairro), !is.na(E_2),
         N_categoria == "20", is.finite(Longitude), is.finite(Latitude)) %>%
  arrange(Data2)

p3_labels <- p3_mapa %>% group_by(Bairro) %>% slice(1)

g_mapa <- ggplot(p3_mapa, aes(x = Longitude, y = Latitude, shape = Reino)) + 
  stat_ellipse(aes(colour = Reino), level = 0.333, type = "norm",
               linetype = "dashed", size = 1.2, alpha = 0.8) +
  geom_path(aes(colour = Reino, group = Reino), size = 1, alpha = 0.6) + 
  geom_point(aes(colour = Reino), size = 4, alpha = 0.6) + 
  scale_shape_manual(values = shapes_personalizados) +
  geom_text_repel(data = p3_labels, aes(label = Bairro), 
                  size = 4, color = "black", box.padding = 0.5, max.overlaps = 20) +
  labs(title = "Mapa de Localização dos Personagens", x = "Longitude", y = "Latitude") +
  facet_wrap(~ Personagem, drop = TRUE, scales = "free") +
  tema_rpg() +
  theme(axis.title = element_text(size = 14, face = "bold"), legend.position = "right") +
  guides(colour = guide_legend(title = "Reino", ncol = 1, override.aes = list(size = 3)))

print(g_mapa)
salvar_pdf(g_mapa, "21_mapa_local", largura = 20, altura = 12)

# ============================================================================
# 9.2. MAPA POR REINO (DEHEON)
# ============================================================================

cat("📊 22. Mapa por Reino (Deheon)...\n")

p3_reino <- planilhatotal %>%
  filter(!is.na(Vila), !is.na(Personagem), !is.na(Estrutura),
         N_categoria == "20", !is.na(E_2), is.finite(Longitude), is.finite(Latitude)) %>%
  filter(str_detect(Reino, "Deheon"))

if(nrow(p3_reino) > 0) {
  p3_labels_reino <- p3_reino %>% group_by(Estrutura) %>% slice(1)
  
  g_reino <- ggplot(p3_reino, aes(x = Longitude, y = Latitude, group = Personagem)) + 
    geom_path(aes(colour = Personagem), size = 1, alpha = 0.6) + 
    geom_jitter(aes(colour = Personagem, shape = Vila), size = 4, alpha = 0.6) + 
    scale_color_manual(values = cores_personagens(length(unique(p3_reino$Personagem)))) +  
    scale_shape_manual(values = 0:21, name = "Vila") +
    geom_text_repel(data = p3_labels_reino, aes(label = Estrutura), 
                    size = 4, color = "black", box.padding = 0.5, max.overlaps = 25) + 
    labs(title = "Mapa do Reino de Deheon", x = "Longitude", y = "Latitude") +
    tema_rpg() +
    theme(axis.title = element_text(size = 14, face = "bold"), legend.position = "right") +
    guides(colour = guide_legend(title = "Personagem", ncol = 1, override.aes = list(size = 3)))
  
  print(g_reino)
  salvar_pdf(g_reino, "22_mapa_reino_deheon")
}

# ============================================================================
# 9.3. MAPA TOTAL
# ============================================================================

cat("📊 23. Mapa Total...\n")

p3_total <- planilhatotal %>%
  filter(!is.na(Jogador), !is.na(Personagem), !is.na(Reino), 
         is.finite(Longitude), is.finite(Latitude))

p3_labels_total <- p3_total %>% group_by(Vila) %>% slice(1)

g_total <- ggplot(p3_total, aes(x = Longitude, y = Latitude, group = Reino)) + 
  geom_path(aes(colour = Reino), size = 1, alpha = 0.6) + 
  geom_jitter(aes(colour = Reino, shape = Jogador), size = 4, alpha = 0.6) + 
  scale_color_manual(values = cores_personagens(length(unique(p3_total$Reino)))) +  
  scale_shape_manual(values = 0:20, name = "Jogador") +
  geom_text_repel(data = p3_labels_total, aes(label = Vila), 
                  size = 4, color = "black", box.padding = 0.5, max.overlaps = 25) + 
  labs(title = "Mapa Geral de Arton", x = "Longitude", y = "Latitude") +
  tema_rpg() +
  theme(axis.title = element_text(size = 14, face = "bold"), legend.position = "right") +
  guides(colour = guide_legend(title = "Reino", ncol = 1, override.aes = list(size = 3)))

print(g_total)
salvar_pdf(g_total, "23_mapa_total", largura = 20, altura = 12)

# ============================================================================
# 10. GRÁFICOS - ESTRUTURA E COMUNIDADES
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("📊 ESTRUTURA E COMUNIDADES\n")
cat(rep("=", 70), "\n\n", sep = "")

# ============================================================================
# 10.1. MST - COMUNIDADES (BAIRRO)
# ============================================================================

cat("📊 24. MST - Comunidades...\n")

p3_mst <- planilhatotal %>%
  filter(!is.na(Personagem), !is.na(Bairro), N_categoria >= 20 & N_categoria <= 23)

local_mst <- dcast(p3_mst, Personagem ~ Bairro, value.var = "N_categoria", fun.aggregate = NULL) %>%
  na.omit() %>%
  data.frame(row.names = 1)

if(nrow(local_mst) > 1) {
  dist_matrix <- vegdist(local_mst, method = "jaccard")
  mst_tree <- mst(dist_matrix)
  mst_coordinates <- cmdscale(dist_matrix)
  
  df_mst <- as.data.frame(mst_coordinates)
  colnames(df_mst) <- c("X", "Y")
  
  edges <- which(mst_tree != 0, arr.ind = TRUE)
  df_lines <- data.frame(
    X1 = mst_coordinates[edges[, 1], 1],
    Y1 = mst_coordinates[edges[, 1], 2],
    X2 = mst_coordinates[edges[, 2], 1],
    Y2 = mst_coordinates[edges[, 2], 2]
  )
  
  g_mst <- ggplot() +
    geom_segment(data = df_lines, aes(x = X1, y = Y1, xend = X2, yend = Y2), 
                 color = "#3498DB", size = 1) +
    geom_point(data = df_mst, aes(x = X, y = Y), color = "#E74C3C", size = 4) +
    geom_text_repel(data = df_mst, aes(x = X, y = Y, label = rownames(df_mst)),
                    vjust = -0.5, max.overlaps = 25, fontface = "bold") +
    labs(title = "Minimum Spanning Tree - Comunidades", x = "Dimensão 1", y = "Dimensão 2") +
    tema_rpg() +
    theme(legend.position = "none")
  
  print(g_mst)
  salvar_pdf(g_mst, "24_mst_comunidades")
}

# ============================================================================
# 10.2. MST - BONUS
# ============================================================================

cat("📊 25. MST - Bonus...\n")

p3_mst2 <- planilhatotal %>%
  filter(!is.na(Bonus), !is.na(Personagem)) %>%
  filter(N_categoria >= 20 & N_categoria <= 20)

local_mst2 <- dcast(p3_mst2, Personagem ~ Bonus, value.var = "N_categoria", fun.aggregate = NULL) %>%
  na.omit() %>%
  data.frame(row.names = 1)

if(nrow(local_mst2) > 1) {
  dist_matrix2 <- vegdist(local_mst2, method = "jaccard")
  mst_tree2 <- mst(dist_matrix2)
  mst_coordinates2 <- cmdscale(dist_matrix2)
  
  df_mst2 <- as.data.frame(mst_coordinates2)
  colnames(df_mst2) <- c("X", "Y")
  
  edges2 <- which(mst_tree2 != 0, arr.ind = TRUE)
  df_lines2 <- data.frame(
    X1 = mst_coordinates2[edges2[, 1], 1],
    Y1 = mst_coordinates2[edges2[, 1], 2],
    X2 = mst_coordinates2[edges2[, 2], 1],
    Y2 = mst_coordinates2[edges2[, 2], 2]
  )
  
  g_mst2 <- ggplot() +
    geom_segment(data = df_lines2, aes(x = X1, y = Y1, xend = X2, yend = Y2), 
                 color = "#3498DB", size = 1) +
    geom_point(data = df_mst2, aes(x = X, y = Y), color = "#E74C3C", size = 4) +
    geom_text_repel(data = df_mst2, aes(x = X, y = Y, label = rownames(df_mst2)),
                    vjust = -0.5, max.overlaps = 25, fontface = "bold") +
    labs(title = "Minimum Spanning Tree - Estruturas", x = "Dimensão 1", y = "Dimensão 2") +
    tema_rpg() +
    theme(legend.position = "none")
  
  print(g_mst2)
  salvar_pdf(g_mst2, "25_mst_estruturas")
}

# ============================================================================
# 10.3. JACCARD - SIMILARIDADE
# ============================================================================

cat("📊 26. Jaccard - Similaridade...\n")

p2_jaccard <- pbase2
p2_jaccard <- p2_jaccard[, !(names(p2_jaccard) == "" | is.na(names(p2_jaccard)))]

p3_jaccard <- p2_jaccard %>%
  filter(!is.na(Personagem), !is.na(Bonus), N_categoria >= 20 & N_categoria <= 24) %>%
  distinct()

if(nrow(p3_jaccard) > 0) {
  local_jaccard <- dcast(p3_jaccard, Personagem ~ Bonus, value.var = "Atributos", fun.aggregate = NULL, fill = 0)
  local_jaccard <- data.frame(local_jaccard, row.names = 1) %>% na.omit()
  
  if(nrow(local_jaccard) > 1) {
    d <- dist.binary(local_jaccard, method = 1, diag = FALSE, upper = FALSE)
    d[is.na(d)] <- 0
    d[is.infinite(d)] <- max(d[!is.infinite(d)])
    
    hc <- hclust(d)
    plot(hc, labels = rownames(local_jaccard), main = "Dendrograma de Personagens (Jaccard)", xlab = "", sub = "")
  }
}
