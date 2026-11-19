# 02_pam_validacao_cruzada_sidra_1997_2024.R
# Compara a PAM do script 01, célula a célula, com uma nova consulta ao SIDRA.
# A consulta lê a API direto (sem sidrar), para não repetir um erro do script 01.
# Saída: 01_pam/analise_descritiva/pam_validacao_cruzada_sidra_1997_2024.xlsx

source(here::here("scripts", "funcoes_comuns.R"))
library(dplyr)
library(tidyr)
library(writexl)
library(here)

VARIAVEIS <- c(8331, 216, 214, 112, 215)  # área plantada, área colhida, quantidade, rendimento, valor

# 1. Arquivo local ---------------------------------------------------------------
arq_local <- here("01_pam", "dados", "pam_piaui_estado_1997_2024_bruto.rds")
exigir_arquivo(arq_local, "01_pam_extracao_1997_2024.R")

local <- readRDS(arq_local) %>%
  select(-any_of(c("codigo_uf", "uf"))) %>%
  pivot_longer(-c(ano, produto), names_to = "variavel", values_to = "valor_local") %>%
  mutate(ano = as.integer(ano), valor_local = as.numeric(valor_local))

# 2. Reconsulta à API do SIDRA ----------------------------------------------------
api <- sprintf(
  "https://apisidra.ibge.gov.br/values/t/5457/n3/%d/v/%s/p/%d-%d/c782/allxt",
  UF_PIAUI, paste(VARIAVEIS, collapse = ","), min(ANOS), max(ANOS)
) %>%
  consultar_api_sidra() %>%
  transmute(
    ano             = as.integer(Ano),
    produto         = `Produto das lavouras temporárias e permanentes`,
    produto_codigo  = `Produto das lavouras temporárias e permanentes (Código)`,
    variavel        = `Variável`,
    variavel_codigo = `Variável (Código)`,
    simbolo_api     = Valor,
    valor_api       = suppressWarnings(as.numeric(Valor))
  )

stopifnot(nrow(api) > 0, !any(grepl("^[0-9]+$", api$produto)))
saveRDS(api, here("01_pam", "dados", "pam_reconsulta_sidra_auditoria.rds"))

# Uma variável do arquivo local sem par na API indicaria nome divergente
vars_sem_par <- setdiff(local$variavel, api$variavel)
if (length(vars_sem_par) > 0) {
  stop("Variáveis sem correspondência na API: ", paste(vars_sem_par, collapse = "; "))
}

# 3. Comparação ------------------------------------------------------------------
presentes_so_no_local <- anti_join(distinct(local, ano, produto), distinct(api, ano, produto),
                                   by = c("ano", "produto"))
presentes_so_na_api   <- anti_join(distinct(api, ano, produto), distinct(local, ano, produto),
                                   by = c("ano", "produto"))

comparacao <- inner_join(local, api, by = c("ano", "produto", "variavel")) %>%
  mutate(
    identico = case_when(
      # o sidrar converte os símbolos do IBGE ("-", "...", "..", "X") em NA
      is.na(valor_local) & is.na(valor_api)              ~ TRUE,
      simbolo_api == "-" & coalesce(valor_local, 0) == 0 ~ TRUE,   # "-" = zero
      is.na(valor_local) | is.na(valor_api)              ~ FALSE,
      TRUE                                               ~ valor_local == valor_api
    )
  )

celulas_divergentes <- comparacao %>%
  filter(!identico) %>%
  select(ano, produto, variavel, valor_local, simbolo_api, valor_api)

n_celulas  <- nrow(comparacao)
n_identica <- sum(comparacao$identico)
pct        <- round(100 * n_identica / n_celulas, 2)

resumo <- tibble(
  metrica = c("Data e hora da reconsulta",
              "Combinações ano x produto no arquivo local",
              "Combinações ano x produto na reconsulta",
              "Combinações presentes só no arquivo local",
              "Combinações presentes só na reconsulta",
              "Células comparadas (ano x produto x variável)",
              "Células com valor numérico",
              "Células idênticas",
              "Células divergentes",
              "Correspondência (%)"),
  valor = as.character(c(
    format(Sys.time(), "%d/%m/%Y %H:%M"),
    n_distinct(local$ano, local$produto),
    n_distinct(api$ano, api$produto),
    nrow(presentes_so_no_local),
    nrow(presentes_so_na_api),
    n_celulas,
    sum(!is.na(comparacao$valor_local)),
    n_identica,
    nrow(celulas_divergentes),
    pct
  ))
)

# 4. Amostra de 15 células com o link da API, para conferir no navegador ----------
set.seed(2024)
amostra_conferencia_manual <- comparacao %>%
  filter(!is.na(valor_local)) %>%
  slice_sample(n = 15) %>%
  transmute(
    ano, produto, variavel, valor_local,
    link_sidra_consulta_direta = sprintf(
      "https://apisidra.ibge.gov.br/values/t/5457/n3/%d/v/%s/p/%d/c782/%s",
      UF_PIAUI, variavel_codigo, ano, produto_codigo)
  ) %>%
  arrange(produto, ano)

# 5. Saída -----------------------------------------------------------------------
vazia <- function(df) if (nrow(df) == 0) tibble(observacao = "nenhuma") else df

arquivo <- here("01_pam", "analise_descritiva", "pam_validacao_cruzada_sidra_1997_2024.xlsx")
dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
write_xlsx(list(
  resumo                     = resumo,
  celulas_divergentes        = vazia(celulas_divergentes),
  presentes_so_no_local      = vazia(presentes_so_no_local),
  presentes_so_na_api        = vazia(presentes_so_na_api),
  amostra_conferencia_manual = amostra_conferencia_manual
), arquivo)

print(resumo, n = Inf)
cat(sprintf(paste0(
  "\nFrase para a metodologia:\n\"Os dados extraídos foram revalidados por meio de uma ",
  "nova consulta independente à API do SIDRA/IBGE, realizada em %s, com correspondência ",
  "de %s%% entre os valores extraídos e os publicados pelo IBGE no momento da revalidação.\"\n"),
  format(Sys.Date(), "%d/%m/%Y"), format(pct, decimal.mark = ",")))
cat("\nRelatório salvo em:", arquivo, "\n")
