# 07_exportacoes_analise_descritiva_1997_2024.R
# Estatísticas e 7 gráficos das exportações. Parte A: toda a pauta do Piauí.
# Parte B: os 35 produtos da pesquisa, por produto x ano (crosswalk e painel).
# Saídas: 02_exportacoes/analise_descritiva/exportacoes_analise_descritiva_1997_2024.xlsx e 02_exportacoes/graficos/

source(here::here("scripts", "funcoes_comuns.R"))
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(here)
library(stringr)
library(ggplot2)
library(scales)
library(forcats)

pasta_saida    <- here("02_exportacoes", "analise_descritiva")
pasta_graficos <- here("02_exportacoes", "graficos")
dir.create(pasta_saida,    showWarnings = FALSE, recursive = TRUE)
dir.create(pasta_graficos, showWarnings = FALSE, recursive = TRUE)

SEM_INTENSIDADE <- c("Cana-de-açúcar", "Abacaxi*", "Coco-da-baía*")
GRUPO_RATEIO    <- c("Manga", "Goiaba")
LIMIAR_SALTO    <- 4  # mesma regra do script 03: alta > 300% ou queda > 75%

# 1. Leitura -------------------------------------------------------------------
arq_ncm <- here("02_exportacoes", "exportacoes_ncm_piaui_estado_1997_2024.rds")
arq_cw  <- here("05_painel", "dados", "crosswalk_produto_pam_ncm_1997_2024.xlsx")
arq_pan <- here("05_painel", "dados", "painel_pam_comex_final_1997_2024.rds")
exigir_arquivo(arq_ncm, "05_exportacoes_extracao_ncm_1997_2024.R")
exigir_arquivo(arq_cw,  "04_crosswalk_produto_pam_ncm.R")
exigir_arquivo(arq_pan, "06_painel_merge_pam_comex_1997_2024.R")

exportacoes <- readRDS(arq_ncm) %>% filter(ano %in% ANOS)
crosswalk   <- read_excel(arq_cw)
painel      <- readRDS(arq_pan)

stopifnot(
  all(nchar(exportacoes$codigo_ncm) == 8),
  all(c("produto_pam", "codigos_ncm", "status_ncm") %in% names(crosswalk)),
  all(c("produto", "ano", "Quantidade produzida", "valor_fob_usd",
        "peso_liquido_t", "status_ncm") %in% names(painel))
)

# NCM -> grupo de produto (Manga e Goiaba juntos, por causa do 08045000)
mapa_ncm <- crosswalk %>%
  filter(!is.na(codigos_ncm)) %>%
  separate_rows(codigos_ncm, sep = ",") %>%
  transmute(
    codigo_ncm    = str_trim(codigos_ncm),
    produto_grupo = if_else(produto_pam %in% GRUPO_RATEIO, "Manga e Goiaba", produto_pam)
  ) %>%
  distinct()
stopifnot(!anyDuplicated(mapa_ncm$codigo_ncm))

exportacoes <- exportacoes %>%
  left_join(mapa_ncm, by = "codigo_ncm") %>%
  mutate(na_pesquisa = !is.na(produto_grupo))

cat("Base NCM:", nrow(exportacoes), "linhas,", n_distinct(exportacoes$codigo_ncm), "NCMs,",
    n_distinct(exportacoes$pais), "países.\n")

# 2. PARTE A - pauta exportadora completa --------------------------------------
fob_total_geral    <- sum(exportacoes$valor_fob_usd)
fob_pesquisa_geral <- sum(exportacoes$valor_fob_usd[exportacoes$na_pesquisa])
formatar_br        <- function(x) format(x, big.mark = ".", decimal.mark = ",")

A_resumo_geral <- tibble(
  metrica = c("Anos cobertos", "NCMs distintos exportados", "Países de destino",
              "Valor FOB total (US$)", "Valor FOB dos 35 produtos da pesquisa (US$)",
              "Participação dos 35 produtos no FOB total (%)"),
  valor   = c(paste(min(exportacoes$ano), "-", max(exportacoes$ano)),
              as.character(n_distinct(exportacoes$codigo_ncm)),
              as.character(n_distinct(exportacoes$pais)),
              formatar_br(round(fob_total_geral)),
              formatar_br(round(fob_pesquisa_geral)),
              formatar_br(round(100 * fob_pesquisa_geral / fob_total_geral, 1)))
)

A_pauta_por_ano <- exportacoes %>%
  group_by(ano) %>%
  summarise(
    fob_total_usd    = sum(valor_fob_usd),
    fob_pesquisa_usd = sum(valor_fob_usd[na_pesquisa]),
    peso_total_t     = sum(peso_liquido_kg) / 1000,
    n_ncms           = n_distinct(codigo_ncm),
    n_paises         = n_distinct(pais)
  ) %>%
  mutate(participacao_pesquisa_pct = round(100 * fob_pesquisa_usd / fob_total_usd, 1))

A_por_ncm <- exportacoes %>%
  group_by(codigo_ncm, produto_ncm_desc, produto_grupo) %>%
  summarise(
    fob_total_usd = sum(valor_fob_usd),
    peso_total_t  = sum(peso_liquido_kg) / 1000,
    n_anos        = n_distinct(ano),
    n_paises      = n_distinct(pais),
    .groups = "drop"
  ) %>%
  mutate(participacao_pct = round(100 * fob_total_usd / fob_total_geral, 2)) %>%
  rename(produto_da_pesquisa = produto_grupo) %>%
  arrange(desc(fob_total_usd))

A_por_pais <- exportacoes %>%
  group_by(pais) %>%
  summarise(
    fob_total_usd    = sum(valor_fob_usd),
    fob_pesquisa_usd = sum(valor_fob_usd[na_pesquisa]),
    n_anos           = n_distinct(ano),
    n_ncms           = n_distinct(codigo_ncm)
  ) %>%
  mutate(participacao_pct = round(100 * fob_total_usd / fob_total_geral, 2)) %>%
  arrange(desc(fob_total_usd))

# 3. PARTE B - os 35 produtos da pesquisa --------------------------------------
B_produto_ano <- painel %>%
  filter(!is.na(status_ncm)) %>%
  transmute(
    produto, ano, status_ncm, unidade_quantidade,
    quantidade_produzida    = `Quantidade produzida`,  # ver unidade_quantidade
    quantidade_produzida_t  = if_else(quantidade_em_toneladas, quantidade_produzida, NA_real_),
    valor_fob_usd,
    peso_exportado_t        = peso_liquido_t,
    preco_medio_usd_kg      = if_else(peso_exportado_t > 0,
                                      valor_fob_usd / (peso_exportado_t * 1000), NA_real_),
    # só com a produção em toneladas (frutas antes de 2001 e abacaxi/coco ficam NA)
    intensidade_exportadora = if_else(!produto %in% SEM_INTENSIDADE & quantidade_em_toneladas &
                                        quantidade_produzida_t > 0,
                                      peso_exportado_t / quantidade_produzida_t, NA_real_)
  ) %>%
  arrange(produto, ano)

B_por_produto <- B_produto_ano %>%
  mutate(exportou = valor_fob_usd > 0) %>%
  group_by(produto, status_ncm) %>%
  summarise(
    fob_total_usd          = sum(valor_fob_usd),
    peso_exportado_total_t = sum(peso_exportado_t),
    anos_com_exportacao    = sum(exportou),
    anos_com_producao      = sum(quantidade_produzida > 0, na.rm = TRUE),
    primeiro_ano_exportou  = if (any(exportou)) min(ano[exportou]) else NA_integer_,
    ultimo_ano_exportou    = if (any(exportou)) max(ano[exportou]) else NA_integer_,
    intensidade_media_anos_exportou = if (any(exportou & !is.na(intensidade_exportadora)))
      mean(intensidade_exportadora[exportou], na.rm = TRUE) else NA_real_,
    anos_intensidade_acima_de_1 = sum(intensidade_exportadora > 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(participacao_fob_pesquisa_pct = round(100 * fob_total_usd / sum(fob_total_usd), 2)) %>%
  arrange(desc(fob_total_usd))

B_produto_pais <- exportacoes %>%
  filter(na_pesquisa) %>%
  group_by(produto_grupo, pais) %>%
  summarise(fob_total_usd = sum(valor_fob_usd),
            peso_total_t  = sum(peso_liquido_kg) / 1000,
            n_anos        = n_distinct(ano),
            .groups = "drop") %>%
  mutate(participacao_no_produto_pct = round(100 * fob_total_usd / sum(fob_total_usd), 1),
         .by = produto_grupo) %>%
  arrange(produto_grupo, desc(fob_total_usd))

B_cobertura_temporal <- B_produto_ano %>%
  transmute(produto, ano, exportou = as.integer(valor_fob_usd > 0)) %>%
  pivot_wider(names_from = ano, values_from = exportou, names_prefix = "ano_") %>%
  mutate(anos_com_exportacao = rowSums(across(starts_with("ano_")))) %>%
  arrange(desc(anos_com_exportacao), produto)

# Saltos dentro de cada produto, só entre anos com exportação nos dois lados
B_valores_suspeitos <- B_produto_ano %>%
  select(produto, ano, valor_fob_usd, peso_exportado_t) %>%
  pivot_longer(c(valor_fob_usd, peso_exportado_t), names_to = "variavel", values_to = "valor") %>%
  arrange(produto, variavel, ano) %>%
  mutate(valor_ano_anterior = lag(valor),
         razao_anual = if_else(valor > 0 & valor_ano_anterior > 0, valor / valor_ano_anterior, NA_real_),
         .by = c(produto, variavel)) %>%
  filter(razao_anual > LIMIAR_SALTO | razao_anual < 1 / LIMIAR_SALTO) %>%
  mutate(var_anual_pct = round((razao_anual - 1) * 100, 1)) %>%
  select(-razao_anual)

# Os totais da Parte B batem com o painel e com o arquivo NCM
stopifnot(
  abs(sum(B_produto_ano$valor_fob_usd) - fob_pesquisa_geral) < 1,
  abs(sum(B_produto_pais$fob_total_usd) - fob_pesquisa_geral) < 1
)

cat("\n=== PARTE A: pauta completa ===\n")
print(A_resumo_geral, n = Inf)
cat("\n=== PARTE B: produtos da pesquisa com exportação ===\n")
print(B_por_produto %>%
        filter(fob_total_usd > 0) %>%
        select(produto, fob_total_usd, anos_com_exportacao,
               intensidade_media_anos_exportou, anos_intensidade_acima_de_1), n = Inf)
cat("\nSaltos anuais sinalizados (B_valores_suspeitos):", nrow(B_valores_suspeitos), "\n")

arq_xlsx <- file.path(pasta_saida, "exportacoes_analise_descritiva_1997_2024.xlsx")
write_xlsx(list(
  A_resumo_geral       = A_resumo_geral,
  A_pauta_por_ano      = A_pauta_por_ano,
  A_por_ncm            = A_por_ncm,
  A_por_pais           = A_por_pais,
  B_por_produto        = B_por_produto,
  B_produto_ano        = B_produto_ano,
  B_produto_pais       = B_produto_pais,
  B_cobertura_temporal = B_cobertura_temporal,
  B_valores_suspeitos  = B_valores_suspeitos
), arq_xlsx)
cat("\nExcel salvo em:", arq_xlsx, "\n")

# 4. Gráficos ------------------------------------------------------------------
fonte      <- "Fonte: Comex Stat/MDIC (critério UF do Produto); IBGE/PAM. Elaboração própria."
N_DESTAQUE <- 8
N_PAISES   <- 5
grafico    <- function(nome) file.path(pasta_graficos, nome)

# Títulos dos painéis em até duas linhas
tema_facetas <- theme(strip.text = element_text(size = rel(0.8), lineheight = 0.9),
                      panel.spacing = unit(1, "lines"))

produtos_destaque <- B_por_produto %>%
  filter(fob_total_usd > 0) %>%
  slice_head(n = N_DESTAQUE) %>%
  pull(produto)

# Participação % no FOB do ano; fora de `destaque` vira "Outros"
participacao_anual <- function(dados, grupo, destaque) {
  dados %>%
    mutate(grupo = if_else({{ grupo }} %in% destaque, {{ grupo }}, "Outros")) %>%
    group_by(ano, grupo) %>%
    summarise(fob = sum(valor_fob_usd), .groups = "drop") %>%
    complete(ano = ANOS, grupo, fill = list(fob = 0)) %>%
    mutate(total = sum(fob), part = if_else(total > 0, 100 * fob / total, 0), .by = ano) %>%
    mutate(grupo = factor(grupo, levels = c(destaque, "Outros")))
}

cat("\nGerando gráficos em", pasta_graficos, "\n")

# 01. Pauta total: produtos da pesquisa x demais
g <- A_pauta_por_ano %>%
  transmute(ano,
            `Produtos agrícolas da pesquisa` = fob_pesquisa_usd,
            `Demais produtos`                = fob_total_usd - fob_pesquisa_usd) %>%
  pivot_longer(-ano, names_to = "grupo", values_to = "fob") %>%
  mutate(grupo = factor(grupo, levels = c("Demais produtos", "Produtos agrícolas da pesquisa"))) %>%
  ggplot(aes(ano, fob / 1e6, fill = grupo)) +
  geom_col() +
  scale_fill_viridis_d(begin = 0.25, end = 0.75) +
  escala_anos() +
  scale_y_continuous(labels = rotulo_num) +
  labs(title = "Exportações do Piauí: total e parcela dos produtos agrícolas da pesquisa",
       x = "Ano", y = "US$ milhões (FOB, correntes)", caption = fonte) +
  tema_dissertacao()
salvar_grafico(g, grafico("01_evolucao_fob_total.png"))

# 02. Composição do FOB dos produtos da pesquisa
g <- participacao_anual(B_produto_ano, produto, produtos_destaque) %>%
  ggplot(aes(ano, part, fill = grupo)) +
  geom_area(position = "stack") +
  scale_fill_viridis_d() +
  escala_anos() +
  labs(title = "Composição do valor exportado dos produtos agrícolas da pesquisa",
       subtitle = paste0("Participação % no FOB anual — principais ",
                         length(produtos_destaque), " produtos + Outros"),
       x = "Ano", y = "%", caption = fonte) +
  tema_dissertacao() +
  guides(fill = guide_legend(nrow = 3))
salvar_grafico(g, grafico("02_composicao_fob_produtos.png"))

# 03. Ranking dos produtos por FOB acumulado
g <- B_por_produto %>%
  filter(fob_total_usd > 0) %>%
  ggplot(aes(fob_total_usd / 1e6, fct_reorder(produto, fob_total_usd))) +
  geom_col(fill = "#31688EFF") +
  scale_x_continuous(trans = "log10", labels = rotulo_num) +
  labs(title = "Valor exportado acumulado por produto, 1997-2024",
       subtitle = "Escala logarítmica: a soja supera o segundo colocado em quase dez vezes",
       x = "US$ milhões (FOB, escala log)", y = NULL, caption = fonte) +
  tema_dissertacao()
salvar_grafico(g, grafico("03_ranking_produtos_fob.png"), altura = 7)

# 04. Destinos dos produtos da pesquisa
paises_top <- B_produto_pais %>%
  summarise(fob = sum(fob_total_usd), .by = pais) %>%
  slice_max(fob, n = N_PAISES) %>%
  pull(pais)
g <- participacao_anual(filter(exportacoes, na_pesquisa), pais, paises_top) %>%
  ggplot(aes(ano, part, fill = grupo)) +
  geom_area(position = "stack") +
  scale_fill_viridis_d() +
  escala_anos() +
  labs(title = "Destino das exportações dos produtos agrícolas da pesquisa",
       subtitle = paste0("Participação % no FOB anual — principais ", N_PAISES, " países + Outros"),
       x = "Ano", y = "%", caption = fonte) +
  tema_dissertacao()
salvar_grafico(g, grafico("04_concentracao_paises_destino.png"))

# 05. Preço médio de exportação (US$/kg)
g <- B_produto_ano %>%
  filter(produto %in% produtos_destaque, !is.na(preco_medio_usd_kg)) %>%
  ggplot(aes(ano, preco_medio_usd_kg)) +
  geom_line(color = "#31688EFF") +
  geom_point(size = 1, color = "#31688EFF") +
  facet_wrap(~ str_wrap(produto, 26), scales = "free_y", ncol = 3) +
  escala_anos(n = 5) +
  labs(title = "Preço médio de exportação por produto",
       subtitle = "US$ FOB por kg, valores correntes; só anos com exportação",
       x = "Ano", y = "US$/kg", caption = fonte) +
  tema_dissertacao() +
  tema_facetas
salvar_grafico(g, grafico("05_evolucao_preco_medio.png"), largura = 12, altura = 10)

# 06. Cobertura temporal: em que anos cada produto exportou
d <- B_produto_ano %>%
  filter(any(valor_fob_usd > 0), .by = produto) %>%
  mutate(exportou = factor(if_else(valor_fob_usd > 0, "Exportou", "Não exportou"),
                           levels = c("Exportou", "Não exportou")))
ordem <- d %>%
  group_by(produto) %>%
  summarise(n = sum(valor_fob_usd > 0)) %>%
  arrange(n) %>%
  pull(produto)
g <- ggplot(d, aes(ano, factor(produto, levels = ordem), fill = exportou)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("Exportou" = "#31688EFF", "Não exportou" = "grey90")) +
  escala_anos(expand = c(0, 0)) +
  labs(title = "Anos com exportação, por produto da pesquisa",
       subtitle = "Só produtos com exportação em pelo menos um ano",
       x = "Ano", y = NULL, caption = fonte) +
  tema_dissertacao()
salvar_grafico(g, grafico("06_cobertura_temporal_heatmap.png"), altura = 7)

# 07. Intensidade exportadora (t exportadas / t produzidas)
g <- B_produto_ano %>%
  filter(produto %in% produtos_destaque, !is.na(intensidade_exportadora)) %>%
  ggplot(aes(ano, intensidade_exportadora)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  geom_line(color = "#31688EFF") +
  geom_point(size = 1, color = "#31688EFF") +
  facet_wrap(~ str_wrap(produto, 26), scales = "free_y", ncol = 3) +
  escala_anos(n = 5) +
  scale_y_continuous(labels = label_percent(decimal.mark = ",")) +
  labs(title = "Intensidade exportadora por produto",
       subtitle = "Toneladas exportadas / toneladas produzidas no ano. Linha tracejada = 100%",
       x = "Ano", y = "% da produção exportada", caption = fonte) +
  tema_dissertacao() +
  tema_facetas
salvar_grafico(g, grafico("07_intensidade_exportadora.png"), largura = 12, altura = 10)
