# 06_painel_merge_pam_comex_1997_2024.R
# Junta PAM (01) e exportações (05) no painel produto x ano, pelo crosswalk (04).
# Saída: 05_painel/dados/painel_pam_comex_final_1997_2024 (.rds e .xlsx)
#
# Unidades: a coluna quantidade_em_toneladas marca as linhas em que a PAM mede
# a quantidade em toneladas (e o rendimento em kg/ha). É FALSE para as frutas
# em 1997-2000 (mil frutos / mil cachos) e para abacaxi e coco-da-baía em todos
# os anos. Rendimento e intensidade exportadora só são comparáveis entre linhas
# com quantidade_em_toneladas = TRUE: as frutas afetadas entram no modelo a
# partir de 2001 (ver nota metodológica).
#
# A coluna usar_no_modelo resume as exclusões: fora do crosswalk, quantidade
# fora de toneladas, exportação sem produção ou maior que a produção.

source(here::here("scripts", "funcoes_comuns.R"))
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(here)

NCM_RATEIO <- "08045000"

# 1. Leitura -------------------------------------------------------------------
arq_pam <- here("01_pam", "dados", "pam_piaui_estado_1997_2024_bruto.rds")
arq_exp <- here("02_exportacoes", "exportacoes_ncm_piaui_estado_1997_2024.rds")
arq_cw  <- here("05_painel", "dados", "crosswalk_produto_pam_ncm_1997_2024.xlsx")
exigir_arquivo(arq_pam, "01_pam_extracao_1997_2024.R")
exigir_arquivo(arq_exp, "05_exportacoes_extracao_ncm_1997_2024.R")
exigir_arquivo(arq_cw,  "04_crosswalk_produto_pam_ncm.R")

pam           <- readRDS(arq_pam)
exportacoes   <- readRDS(arq_exp)
crosswalk_ncm <- read_excel(arq_cw, col_types = "text") %>%
  select(produto_pam, codigos_ncm, status_ncm)

arq_uni <- here("01_pam", "dados", "pam_piaui_unidades_1997_2024.rds")
exigir_arquivo(arq_uni, "01_pam_extracao_1997_2024.R")
unidades <- readRDS(arq_uni)

stopifnot(
  nrow(pam) == n_distinct(pam$ano, pam$produto),
  crosswalk_ncm$produto_pam %in% pam$produto
)

# 2. Exportações por produto da PAM ---------------------------------------------
mapa <- crosswalk_ncm %>%
  filter(!is.na(codigos_ncm)) %>%
  separate_rows(codigos_ncm, sep = ",") %>%
  rename(codigo_ncm = codigos_ncm)

ncm_fora_do_mapa <- exportacoes %>%
  filter(substr(codigo_ncm, 1, 4) %in% substr(mapa$codigo_ncm, 1, 4),
         !codigo_ncm %in% mapa$codigo_ncm) %>%
  summarise(valor_fob_usd = sum(valor_fob_usd), .by = c(codigo_ncm, produto_ncm_desc)) %>%
  arrange(codigo_ncm, produto_ncm_desc)

# O NCM 08045000 (goiabas e mangas) é rateado pela quantidade produzida no ano
fracao_rateio <- pam %>%
  filter(produto %in% c("Goiaba", "Manga")) %>%
  transmute(ano, produto_pam = produto, q = coalesce(`Quantidade produzida`, 0)) %>%
  mutate(fracao = if (sum(q) > 0) q / sum(q) else 0.5, .by = ano) %>%
  select(-q)

exportacoes_produto <- exportacoes %>%
  inner_join(mapa, by = "codigo_ncm", relationship = "many-to-many") %>%
  left_join(fracao_rateio, by = c("ano", "produto_pam")) %>%
  mutate(rateado = codigo_ncm == NCM_RATEIO,
         fracao  = if_else(rateado, fracao, 1)) %>%
  summarise(across(c(valor_fob_usd, peso_liquido_kg), ~ sum(.x * fracao)),
            rateado = any(rateado), .by = c(produto_pam, ano))

# 3. Painel --------------------------------------------------------------------
painel <- pam %>%
  left_join(crosswalk_ncm, by = c(produto = "produto_pam")) %>%
  left_join(exportacoes_produto, by = c(produto = "produto_pam", "ano")) %>%
  mutate(
    status_merge = case_when(
      is.na(status_ncm)              ~ "produto_fora_do_crosswalk",
      is.na(codigos_ncm)             ~ "sem_exportacao_sh4_ncm_28_anos",
      rateado %in% TRUE              ~ "rateio_producao_goiaba_manga",
      coalesce(valor_fob_usd, 0) > 0 ~ "exportado_no_ano",
      TRUE                           ~ "nao_exportado_no_ano"
    ),
    # fora do crosswalk = NA; no crosswalk sem exportação no ano = 0
    across(c(valor_fob_usd, peso_liquido_kg), ~ if_else(is.na(status_ncm), NA, coalesce(.x, 0))),
    peso_liquido_t = peso_liquido_kg / 1000,
    rateado        = NULL
  ) %>%
  left_join(unidades, by = c("ano", "produto")) %>%
  mutate(
    quantidade_em_toneladas = unidade_quantidade %in% "Toneladas",
    # Consistência entre exportação e produção (regra para todos os produtos):
    # exportar sem produção registrada, ou exportar mais toneladas do que se
    # produziu no ano, não é fisicamente plausível para a produção do Piauí.
    # Causas prováveis: produto de outra UF declarado como do Piauí (o critério
    # UF do Produto é autodeclarado na DU-E) ou produção subestimada pela PAM.
    exportacao_sem_producao = !is.na(status_ncm) & coalesce(valor_fob_usd, 0) > 0 &
                              coalesce(`Quantidade produzida`, 0) == 0,
    exportacao_maior_que_producao = !is.na(status_ncm) & quantidade_em_toneladas &
                                    coalesce(`Quantidade produzida`, 0) > 0 &
                                    coalesce(peso_liquido_t, 0) > `Quantidade produzida`,
    # Linha utilizável na estimação: produto no crosswalk, quantidade em
    # toneladas e exportação consistente com a produção
    usar_no_modelo = !is.na(status_ncm) & quantidade_em_toneladas &
                     !exportacao_sem_producao & !exportacao_maior_que_producao
  ) %>%
  arrange(produto, ano)

totais <- painel %>%
  filter(!is.na(status_ncm)) %>%
  summarise(fob_total_usd       = sum(valor_fob_usd),
            peso_total_t        = sum(peso_liquido_t),
            anos_com_exportacao = sum(valor_fob_usd > 0),
            .by = c(produto, status_ncm)) %>%
  arrange(desc(fob_total_usd), produto, status_ncm)

# Nenhuma linha da PAM perdida ou duplicada; todo o FOB do crosswalk distribuído
stopifnot(
  nrow(painel) == nrow(pam),
  abs(sum(totais$fob_total_usd) -
        sum(exportacoes$valor_fob_usd[exportacoes$codigo_ncm %in% mapa$codigo_ncm])) < 1
)
stopifnot(!anyNA(painel$unidade_quantidade))
cat("\nExportação inconsistente com a produção (excluídas do modelo):\n")
print(painel %>% filter(exportacao_sem_producao | exportacao_maior_que_producao) %>%
        transmute(produto, ano, producao_t = `Quantidade produzida`, exportacao_t = round(peso_liquido_t, 1),
                  valor_fob_usd, motivo = if_else(exportacao_sem_producao, "exportação sem produção",
                                                  "exportação maior que a produção")),
      n = Inf)
cat("Linhas utilizáveis na estimação (usar_no_modelo):", sum(painel$usar_no_modelo), "\n")
print(count(painel, status_merge, sort = TRUE))
cat("\nLinhas do crosswalk com quantidade fora de toneladas (excluídas do modelo):\n")
print(painel %>% filter(!is.na(status_ncm), !quantidade_em_toneladas, !is.na(`Quantidade produzida`)) %>%
        summarise(ano_inicial = min(ano), ano_final = max(ano), n = n(),
                  .by = c(produto, unidade_quantidade)),
      n = Inf)
print(totais, n = Inf)

# 4. Saídas --------------------------------------------------------------------
pasta <- here("05_painel", "dados")
dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
saveRDS(painel, file.path(pasta, "painel_pam_comex_final_1997_2024.rds"))
write_xlsx(list(painel             = rotular_unidades(painel),
                totais_por_produto = totais,
                ncm_fora_do_mapa   = ncm_fora_do_mapa),
           file.path(pasta, "painel_pam_comex_final_1997_2024.xlsx"))
