# 03_pam_analise_descritiva_1997_2024.R
# Estatísticas descritivas, verificação de consistência e 7 gráficos da PAM.
# Saídas: 01_pam/analise_descritiva/pam_analise_descritiva_1997_2024.xlsx e 01_pam/graficos/

source(here::here("scripts", "funcoes_comuns.R"))
library(dplyr)
library(tidyr)
library(here)
library(writexl)
library(purrr)
library(ggplot2)
library(scales)
library(forcats)

pasta_saida    <- here("01_pam", "analise_descritiva")
pasta_graficos <- here("01_pam", "graficos")
dir.create(pasta_saida,    showWarnings = FALSE, recursive = TRUE)
dir.create(pasta_graficos, showWarnings = FALSE, recursive = TRUE)

# 1. Leitura -----------------------------------------------------------------------
arq_entrada <- here("01_pam", "dados", "pam_piaui_estado_1997_2024_bruto.rds")
exigir_arquivo(arq_entrada, "01_pam_extracao_1997_2024.R")
pam_wide <- readRDS(arq_entrada)

# As 5 variáveis da PAM: área plantada, área colhida, quantidade, rendimento, valor
colunas_var <- setdiff(names(pam_wide), c("ano", "codigo_uf", "uf", "produto"))
anos        <- sort(unique(pam_wide$ano))
periodo     <- paste0(min(anos), "–", max(anos))
cat("Base:", nrow(pam_wide), "linhas;", length(colunas_var), "variáveis:",
    paste(colunas_var, collapse = "; "), "\n")

# 2. Estatísticas descritivas ------------------------------------------------------
resumo_geral <- tibble(
  metrica = c("Total de linhas", "Anos cobertos (min-max)", "N. de anos distintos",
              "N. de produtos distintos", "N. de variáveis"),
  valor   = c(as.character(nrow(pam_wide)),
              paste(min(pam_wide$ano), "-", max(pam_wide$ano)),
              as.character(n_distinct(pam_wide$ano)),
              as.character(n_distinct(pam_wide$produto)),
              as.character(length(colunas_var)))
)

por_ano <- pam_wide %>%
  group_by(ano) %>%
  summarise(
    n_linhas   = n(),
    n_produtos = n_distinct(produto),
    across(all_of(colunas_var), list(
      n_validos = ~ sum(!is.na(.)),
      n_na      = ~ sum(is.na(.)),
      media     = ~ mean(., na.rm = TRUE),
      mediana   = ~ median(., na.rm = TRUE),
      dp        = ~ sd(., na.rm = TRUE),
      minimo    = ~ suppressWarnings(min(., na.rm = TRUE)),
      maximo    = ~ suppressWarnings(max(., na.rm = TRUE))
    ), .names = "{.col}__{.fn}"),
    .groups = "drop"
  )

por_produto <- pam_wide %>%
  group_by(produto) %>%
  summarise(
    n_linhas = n(),
    n_anos   = n_distinct(ano),
    across(all_of(colunas_var), list(
      n_validos          = ~ sum(!is.na(.)),
      taxa_preenchimento = ~ round(sum(!is.na(.)) / n() * 100, 1),
      media              = ~ mean(., na.rm = TRUE)
    ), .names = "{.col}__{.fn}"),
    .groups = "drop"
  ) %>%
  arrange(desc(n_anos))

por_variavel <- map_dfr(colunas_var, function(v) {
  x <- pam_wide[[v]]
  tibble(
    variavel           = v,
    n_total            = length(x),
    n_validos          = sum(!is.na(x)),
    n_na               = sum(is.na(x)),
    taxa_preenchimento = round(sum(!is.na(x)) / length(x) * 100, 1),
    media              = mean(x, na.rm = TRUE),
    mediana            = median(x, na.rm = TRUE),
    dp                 = sd(x, na.rm = TRUE),
    minimo             = min(x, na.rm = TRUE),
    maximo             = max(x, na.rm = TRUE),
    p95                = quantile(x, 0.95, na.rm = TRUE),
    p99                = quantile(x, 0.99, na.rm = TRUE)
  )
})

# 3. Cobertura temporal: em que anos cada produto tem alguma variável preenchida ----
cobertura_longa <- pam_wide %>%
  mutate(tem_dado = if_any(all_of(colunas_var), ~ !is.na(.))) %>%
  group_by(produto, ano) %>%
  summarise(tem_dado = as.integer(any(tem_dado)), .groups = "drop")

cobertura_temporal <- cobertura_longa %>%
  pivot_wider(id_cols = produto, names_from = ano, values_from = tem_dado,
              names_prefix = "ano_", values_fill = 0) %>%
  mutate(
    anos_com_dado         = rowSums(across(starts_with("ano_"))),
    pct_cobertura_periodo = round(anos_com_dado / length(anos) * 100, 1)
  ) %>%
  arrange(pct_cobertura_periodo, produto)

cat(sprintf("Cobertura temporal: %d de %d produtos têm dado em todos os %d anos.\n",
            sum(cobertura_temporal$anos_com_dado == length(anos)),
            nrow(cobertura_temporal), length(anos)))

# 4. Valores suspeitos, cada produto comparado com ele mesmo ------------------------
# Salto anual > 4x ou < 1/4. Só no rendimento: > 5x ou < 1/5 da mediana do produto
# (nas outras variáveis, tendência e inflação gerariam falsos alarmes). Identidades:
# colhida > plantada; rendimento recalculado com diferença > 5%. base_pequena: área
# colhida mediana < 100 ha, onde saltos grandes são normais.
LIMIAR_SALTO        <- 4    # razão ano / ano anterior
LIMIAR_MEDIANA      <- 5    # razão valor / mediana do produto
LIMIAR_RENDIMENTO   <- 5    # diferença % no rendimento recalculado
LIMIAR_BASE_PEQUENA <- 100  # ha

fora_do_intervalo <- function(razao, limiar) !is.na(razao) & (razao > limiar | razao < 1 / limiar)

area_mediana <- pam_wide %>%
  group_by(produto) %>%
  summarise(mediana_area_colhida = median(`Área colhida`, na.rm = TRUE), .groups = "drop")

variacao_por_produto <- pam_wide %>%
  select(ano, produto, all_of(colunas_var)) %>%
  pivot_longer(all_of(colunas_var), names_to = "variavel", values_to = "valor") %>%
  arrange(produto, variavel, ano) %>%
  group_by(produto, variavel) %>%
  mutate(
    valor_ano_anterior = lag(valor),
    razao_anual        = if_else(valor_ano_anterior > 0, valor / valor_ano_anterior, NA_real_),
    var_anual_pct      = (razao_anual - 1) * 100,
    mediana_produto    = median(valor, na.rm = TRUE),
    razao_mediana      = if_else(mediana_produto > 0, valor / mediana_produto, NA_real_)
  ) %>%
  ungroup() %>%
  filter(!is.na(valor)) %>%
  left_join(area_mediana, by = "produto") %>%
  mutate(
    flag_salto_anual     = fora_do_intervalo(razao_anual, LIMIAR_SALTO),
    flag_fora_da_mediana = variavel == "Rendimento médio da produção" &
                           fora_do_intervalo(razao_mediana, LIMIAR_MEDIANA),
    base_pequena         = coalesce(mediana_area_colhida < LIMIAR_BASE_PEQUENA, FALSE)
  ) %>%
  select(produto, variavel, ano, valor, valor_ano_anterior, var_anual_pct,
         mediana_produto, razao_mediana, flag_salto_anual, flag_fora_da_mediana,
         base_pequena)

valores_suspeitos <- variacao_por_produto %>%
  filter(flag_salto_anual | flag_fora_da_mediana) %>%
  arrange(base_pequena, produto, variavel, ano)

inconsistencias_identidades <- pam_wide %>%
  transmute(
    ano, produto,
    area_plantada        = `Área plantada ou destinada à colheita`,
    area_colhida         = `Área colhida`,
    quantidade           = `Quantidade produzida`,
    rendimento_publicado = `Rendimento médio da produção`,
    rendimento_calculado = if_else(area_colhida > 0, quantidade * 1000 / area_colhida, NA_real_),
    dif_rendimento_pct   = (rendimento_publicado / rendimento_calculado - 1) * 100,
    flag_colhida_maior_plantada   = coalesce(area_colhida > area_plantada, FALSE),
    flag_rendimento_inconsistente = coalesce(abs(dif_rendimento_pct) > LIMIAR_RENDIMENTO, FALSE)
  ) %>%
  filter(flag_colhida_maior_plantada | flag_rendimento_inconsistente) %>%
  arrange(produto, ano)

cat("Valores suspeitos:", nrow(valores_suspeitos), "linhas, sendo",
    sum(!valores_suspeitos$base_pequena), "em produtos com área colhida mediana >=",
    LIMIAR_BASE_PEQUENA, "ha.\n")
cat("Inconsistências nas identidades contábeis:", nrow(inconsistencias_identidades), "\n")

arq_xlsx <- file.path(pasta_saida, "pam_analise_descritiva_1997_2024.xlsx")
write_xlsx(list(
  resumo_geral                = resumo_geral,
  por_ano                     = por_ano,
  por_produto                 = por_produto,
  por_variavel                = por_variavel,
  cobertura_temporal          = cobertura_temporal,
  variacao_por_produto        = variacao_por_produto,
  valores_suspeitos           = valores_suspeitos,
  inconsistencias_identidades = inconsistencias_identidades
), arq_xlsx)
cat("Excel salvo em:", arq_xlsx, "\n")

# 5. Gráficos ----------------------------------------------------------------------
fonte      <- "Fonte: IBGE, PAM, tabela 5457. Elaboração própria."
N_DESTAQUE <- 8  # produtos nos gráficos de "principais produtos"
grafico    <- function(nome) file.path(pasta_graficos, nome)

# Os mesmos N_DESTAQUE produtos (maior valor médio anual) em todos os gráficos
valor_medio_produto <- pam_wide %>%
  group_by(produto) %>%
  summarise(valor_medio_anual = mean(`Valor da produção`, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(valor_medio_anual), valor_medio_anual > 0) %>%
  arrange(desc(valor_medio_anual))
produtos_destaque <- head(valor_medio_produto$produto, N_DESTAQUE)

cat("Gerando gráficos em", pasta_graficos, "\n")

# 01. Área plantada e colhida total
g <- pam_wide %>%
  group_by(ano) %>%
  summarise(`Área plantada` = sum(`Área plantada ou destinada à colheita`, na.rm = TRUE),
            `Área colhida`  = sum(`Área colhida`, na.rm = TRUE)) %>%
  pivot_longer(-ano, names_to = "tipo_area", values_to = "hectares") %>%
  ggplot(aes(ano, hectares / 1000, color = tipo_area)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  scale_color_viridis_d(end = 0.75) +
  escala_anos() +
  scale_y_continuous(labels = rotulo_num) +
  labs(title    = "Evolução da área agrícola total do Piauí",
       subtitle = paste0(periodo, " — soma de todos os produtos da PAM"),
       x = "Ano", y = "Área (mil hectares)",
       caption  = "Fonte: IBGE, Pesquisa Agrícola Municipal (PAM), tabela 5457. Elaboração própria.") +
  tema_dissertacao()
salvar_grafico(g, grafico("01_evolucao_area_total.png"))

# 02. Valor total da produção
g <- pam_wide %>%
  group_by(ano) %>%
  summarise(valor_total = sum(`Valor da produção`, na.rm = TRUE)) %>%
  ggplot(aes(ano, valor_total / 1000)) +
  geom_col(fill = "#31688EFF") +
  escala_anos() +
  scale_y_continuous(labels = rotulo_num) +
  labs(title    = "Evolução do valor total da produção agrícola do Piauí",
       subtitle = paste0(periodo, ", valores correntes (sem deflacionar)"),
       x = "Ano", y = "Valor da produção (R$ milhões)", caption = fonte) +
  tema_dissertacao()
salvar_grafico(g, grafico("02_evolucao_valor_producao.png"))

# 03. Composição do valor da produção. complete() preenche com 0 os anos em
# que um grupo não aparece; sem isso o geom_area interpola e a pilha passa de 100%.
g <- pam_wide %>%
  mutate(grupo = if_else(produto %in% produtos_destaque, produto, "Outros")) %>%
  group_by(ano, grupo) %>%
  summarise(valor = sum(`Valor da produção`, na.rm = TRUE), .groups = "drop") %>%
  complete(ano, grupo, fill = list(valor = 0)) %>%
  mutate(total = sum(valor), participacao = if_else(total > 0, valor / total * 100, 0), .by = ano) %>%
  mutate(grupo = fct_relevel(factor(grupo), "Outros", after = Inf)) %>%
  ggplot(aes(ano, participacao, fill = grupo)) +
  geom_area(position = "stack", color = "white", linewidth = 0.1) +
  scale_fill_viridis_d() +
  escala_anos(expand = c(0, 0)) +
  scale_y_continuous(labels = label_percent(scale = 1), expand = c(0, 0)) +
  labs(title    = "Composição do valor da produção agrícola por produto",
       subtitle = paste0("Participação percentual no valor total anual — principais ",
                         length(produtos_destaque), " produtos + Outros"),
       x = "Ano", y = "Participação no valor total (%)", caption = fonte) +
  tema_dissertacao() +
  theme(legend.position = "right")
salvar_grafico(g, grafico("03_composicao_valor_produtos.png"))

# 04. Ranking dos 15 produtos de maior valor médio anual
g <- valor_medio_produto %>%
  slice_head(n = 15) %>%
  ggplot(aes(valor_medio_anual, fct_reorder(produto, valor_medio_anual), fill = valor_medio_anual)) +
  geom_col() +
  scale_fill_viridis_c(guide = "none") +
  scale_x_continuous(labels = rotulo_num) +
  labs(title    = "Principais produtos agrícolas do Piauí por valor de produção",
       subtitle = paste0("Valor médio anual, ", periodo, " (Mil R$ correntes) — top 15"),
       x = "Valor médio anual da produção (Mil R$)", y = NULL, caption = fonte) +
  tema_dissertacao()
salvar_grafico(g, grafico("04_ranking_produtos_valor.png"), altura = 7)

# 05. Rendimento dos principais produtos. Eixo y livre em cada painel: as
# magnitudes (ex.: mandioca x soja) são muito diferentes.
g <- pam_wide %>%
  filter(produto %in% produtos_destaque) %>%
  ggplot(aes(ano, `Rendimento médio da produção`)) +
  geom_line(color = "#440154FF", linewidth = 0.8) +
  geom_point(color = "#440154FF", size = 1.2) +
  facet_wrap(~ produto, scales = "free_y", ncol = 3) +
  escala_anos(n = 4) +
  scale_y_continuous(labels = rotulo_num) +
  labs(title    = "Evolução do rendimento médio por produto",
       subtitle = paste0("Principais ", length(produtos_destaque), " produtos por valor de produção, ",
                         periodo, " — escalas do eixo Y livres entre painéis"),
       x = "Ano", y = "Rendimento médio (kg/ha)", caption = fonte) +
  tema_dissertacao() +
  theme(strip.text = element_text(size = rel(0.8)))
salvar_grafico(g, grafico("05_evolucao_rendimento_produtos.png"), largura = 11, altura = 8)

# 06. Distribuição do rendimento dos principais produtos
g <- pam_wide %>%
  filter(produto %in% produtos_destaque, !is.na(`Rendimento médio da produção`)) %>%
  ggplot(aes(`Rendimento médio da produção`,
             fct_reorder(produto, `Rendimento médio da produção`, .fun = median))) +
  geom_boxplot(fill = "#35B779FF", alpha = 0.75, outlier.color = "firebrick") +
  scale_x_continuous(labels = rotulo_num) +
  labs(title    = "Distribuição do rendimento médio por produto",
       subtitle = paste0("Principais ", length(produtos_destaque), " produtos, ", periodo),
       x = "Rendimento médio (kg/ha)", y = NULL,
       caption  = paste("Fonte: IBGE, PAM, tabela 5457. Caixa = intervalo interquartil;",
                        "linha central = mediana; pontos = outliers. Elaboração própria.")) +
  tema_dissertacao()
salvar_grafico(g, grafico("06_boxplot_rendimento_produtos.png"))

# 07. Mapa de calor da cobertura temporal
g <- cobertura_longa %>%
  left_join(select(cobertura_temporal, produto, pct_cobertura_periodo), by = "produto") %>%
  ggplot(aes(ano, fct_reorder(produto, pct_cobertura_periodo), fill = factor(tem_dado))) +
  geom_tile(color = "white", linewidth = 0.15) +
  scale_fill_manual(values = c("0" = "grey85", "1" = "#21908CFF"),
                    labels = c("Sem dado", "Com dado")) +
  escala_anos(expand = c(0, 0)) +
  labs(title    = "Cobertura temporal da produção por produto",
       subtitle = paste0("Piauí, ", periodo, " — produtos ordenados por % de cobertura no período"),
       x = "Ano", y = NULL, caption = fonte) +
  tema_dissertacao() +
  theme(panel.grid = element_blank(), axis.text.y = element_text(size = rel(0.55)),
        legend.position = "top")
salvar_grafico(g, grafico("07_cobertura_temporal_heatmap.png"), largura = 11,
               altura = max(6, 0.16 * n_distinct(cobertura_longa$produto)))
