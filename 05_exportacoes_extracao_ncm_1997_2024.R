# 05_exportacoes_extracao_ncm_1997_2024.R
# Extrai as exportações do Piauí (Comex Stat, critério UF do Produto) por ano x NCM x país
# e monta a tabela do orientador: ano | codigo_ncm | produto_ncm_desc | <País>_fob | <País>_kg ...
# Saídas: 02_exportacoes/exportacoes_ncm_piaui_estado_1997_2024 (.rds long, _wide.rds, .xlsx
#         com as abas long, por_pais e por_pais_35_produtos)

REEXTRAIR <- FALSE  # TRUE baixa tudo de novo da API; FALSE reaproveita o .rds salvo

source(here::here("scripts", "funcoes_comuns.R"))
library(dplyr)
library(tidyr)
library(here)
library(writexl)
library(stringr)

pasta    <- here("02_exportacoes")
arq_long <- file.path(pasta, "exportacoes_ncm_piaui_estado_1997_2024.rds")
arq_wide <- file.path(pasta, "exportacoes_ncm_piaui_estado_1997_2024_wide.rds")
arq_xlsx <- file.path(pasta, "exportacoes_ncm_piaui_estado_1997_2024.xlsx")
dir.create(pasta, showWarnings = FALSE, recursive = TRUE)

# 1. Extração (só se REEXTRAIR = TRUE ou se ainda não houver extração salva) ----
# Pausa de 2 s entre anos; no erro 429 (limite da API) espera 15 s e tenta
# de novo, até 5 vezes no total. Outros erros pulam o ano.
extrair_ano <- function(ano, tentativa = 1) {
  cat("Extraindo", ano, "\n")
  resultado <- tryCatch(
    comexr::comex_query(
      flow         = "export",
      start_period = sprintf("%d-01", ano),
      end_period   = sprintf("%d-12", ano),
      details      = c("state", "country", "ncm"),
      filters      = list(state = UF_PIAUI),
      month_detail = FALSE,
      metric_fob   = TRUE,
      metric_kg    = TRUE,
      language     = "pt"
    ),
    error = function(e) {
      if (grepl("429", conditionMessage(e)) && tentativa < 5) {
        cat("  limite da API, nova tentativa em 15 s\n")
        Sys.sleep(15)
        return(extrair_ano(ano, tentativa + 1))
      }
      message("ERRO no ano ", ano, ": ", conditionMessage(e))
      NULL
    }
  )
  Sys.sleep(2)
  resultado
}

if (REEXTRAIR || !file.exists(arq_long)) {
  exportacoes_ncm <- bind_rows(lapply(ANOS, extrair_ano)) %>%
    transmute(
      ano              = as.integer(year),
      codigo_ncm       = str_pad(as.character(coNcm), 8, pad = "0"),  # preserva o zero à esquerda
      codigo_sh4       = as.integer(substr(codigo_ncm, 1, 4)),
      produto_ncm_desc = ncm,
      pais             = country,
      valor_fob_usd    = as.numeric(metricFOB),
      peso_liquido_kg  = as.numeric(metricKG)
    ) %>%
    arrange(ano, codigo_ncm, desc(valor_fob_usd))
  saveRDS(exportacoes_ncm, arq_long)
} else {
  cat("Usando a extração já salva:", arq_long, "\n")
  exportacoes_ncm <- readRDS(arq_long)
}

# 2. Conferências --------------------------------------------------------------
anos_faltando <- setdiff(ANOS, exportacoes_ncm$ano)
if (length(anos_faltando) > 0) {
  warning("Anos sem dados (rode de novo com REEXTRAIR = TRUE): ",
          paste(anos_faltando, collapse = ", "))
}

stopifnot(
  !anyNA(exportacoes_ncm$ano),
  !anyNA(exportacoes_ncm$valor_fob_usd),
  all(nchar(exportacoes_ncm$codigo_ncm) == 8),
  # cada NCM tem uma única descrição em cada ano
  !anyDuplicated(distinct(exportacoes_ncm, ano, codigo_ncm, produto_ncm_desc)[c("ano", "codigo_ncm")])
)

# 3. Tabela no formato do orientador -------------------------------------------
# Países em ordem alfabética do português ("África do Sul" antes de "Alemanha")
paises       <- stringi::stri_sort(unique(exportacoes_ncm$pais), locale = "pt_BR")
colunas_pais <- as.vector(rbind(paste0(paises, "_fob"), paste0(paises, "_kg")))

exportacoes_ncm_wide <- exportacoes_ncm %>%
  summarise(fob = sum(valor_fob_usd), kg = sum(peso_liquido_kg),
            .by = c(ano, codigo_ncm, produto_ncm_desc, pais)) %>%
  pivot_wider(names_from = pais, values_from = c(fob, kg),
              names_glue = "{pais}_{.value}", values_fill = 0) %>%
  select(ano, codigo_ncm, produto_ncm_desc, all_of(colunas_pais)) %>%
  arrange(ano, codigo_ncm)

# O formato largo tem os mesmos totais do longo
stopifnot(
  ncol(exportacoes_ncm_wide) == 3 + 2 * length(paises),
  abs(sum(select(exportacoes_ncm_wide, ends_with("_fob"))) - sum(exportacoes_ncm$valor_fob_usd)) < 1,
  abs(sum(select(exportacoes_ncm_wide, ends_with("_kg")))  - sum(exportacoes_ncm$peso_liquido_kg)) < 1
)

cat(sprintf(
  "\n%d linhas (long) | %d linhas x %d colunas (por_pais) | %d NCMs | %d países | %d anos | FOB total US$ %s\n",
  nrow(exportacoes_ncm), nrow(exportacoes_ncm_wide), ncol(exportacoes_ncm_wide),
  n_distinct(exportacoes_ncm$codigo_ncm), length(paises), n_distinct(exportacoes_ncm$ano),
  format(sum(exportacoes_ncm$valor_fob_usd), big.mark = ".", decimal.mark = ",")
))

# 4. Aba só com os 35 produtos agrícolas da pesquisa ---------------------------
# Usa o crosswalk do script 04. Mesma estrutura da aba por_pais, com a coluna
# produto_pam após a descrição do NCM; ficam só os países que compraram algum
# desses produtos. Manga e Goiaba dividem o NCM 08045000 e aparecem juntas.
arq_cw <- here("05_painel", "dados", "crosswalk_produto_pam_ncm_1997_2024.xlsx")
exigir_arquivo(arq_cw, "04_crosswalk_produto_pam_ncm.R")

mapa_ncm <- readxl::read_excel(arq_cw, col_types = "text") %>%
  filter(!is.na(codigos_ncm)) %>%
  separate_rows(codigos_ncm, sep = ",") %>%
  transmute(codigo_ncm  = str_trim(codigos_ncm),
            produto_pam = if_else(produto_pam %in% c("Manga", "Goiaba"), "Manga e Goiaba", produto_pam)) %>%
  distinct()
stopifnot(!anyDuplicated(mapa_ncm$codigo_ncm))

por_pais_35 <- exportacoes_ncm_wide %>%
  inner_join(mapa_ncm, by = "codigo_ncm") %>%
  relocate(produto_pam, .after = produto_ncm_desc)

paises_35 <- paises[vapply(paises, function(p)
  sum(por_pais_35[[paste0(p, "_fob")]]) > 0 || sum(por_pais_35[[paste0(p, "_kg")]]) > 0, logical(1))]
por_pais_35 <- por_pais_35 %>%
  select(ano, codigo_ncm, produto_ncm_desc, produto_pam,
         all_of(as.vector(rbind(paste0(paises_35, "_fob"), paste0(paises_35, "_kg"))))) %>%
  arrange(ano, codigo_ncm)

fob_35 <- sum(exportacoes_ncm$valor_fob_usd[exportacoes_ncm$codigo_ncm %in% mapa_ncm$codigo_ncm])
stopifnot(abs(sum(select(por_pais_35, ends_with("_fob"))) - fob_35) < 1)
cat(sprintf("Aba por_pais_35_produtos: %d linhas x %d colunas | %d NCMs | %d países | FOB US$ %s\n",
            nrow(por_pais_35), ncol(por_pais_35), n_distinct(por_pais_35$codigo_ncm),
            length(paises_35), format(round(fob_35), big.mark = ".", decimal.mark = ",")))

# 5. Saídas --------------------------------------------------------------------
saveRDS(exportacoes_ncm_wide, arq_wide)
write_xlsx(list(long = exportacoes_ncm, por_pais = exportacoes_ncm_wide,
                por_pais_35_produtos = por_pais_35), arq_xlsx)
cat("Arquivos salvos em", pasta, "\n")
