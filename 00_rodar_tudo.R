# 00_rodar_tudo.R
# Roda o pipeline inteiro, em ordem, e grava registro_execucao.txt (tempos e versões).
# Abra antes o projeto pelo .Rproj. A etapa mais longa é a 05 (API do Comex Stat).

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
source(here::here("scripts", "funcoes_comuns.R"))

etapas <- c(
  "01_pam_extracao_1997_2024.R"                   = "Extrai a PAM/IBGE (tabela 5457) do Piauí",
  "02_pam_validacao_cruzada_sidra_1997_2024.R"    = "Revalida a PAM célula a célula contra a API do SIDRA",
  "03_pam_analise_descritiva_1997_2024.R"         = "Estatísticas e gráficos da PAM",
  "04_crosswalk_produto_pam_ncm.R"                = "Tabela de correspondência produto PAM -> NCM",
  "05_exportacoes_extracao_ncm_1997_2024.R"       = "Extrai as exportações (Comex Stat, NCM, UF do Produto)",
  "06_painel_merge_pam_comex_1997_2024.R"         = "Junta PAM e exportações no painel produto x ano",
  "07_exportacoes_analise_descritiva_1997_2024.R" = "Estatísticas e gráficos das exportações",
  "08_pam_verificacao_lista_produtos_1997_2024.R" = "Verifica mudanças na lista de produtos da PAM"
)

minutos_desde <- function(t0) round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
linha <- strrep("=", 62)

registro     <- character()
inicio_total <- Sys.time()

for (script in names(etapas)) {
  cat("\n", linha, "\n", script, " - ", etapas[[script]], "\n", linha, "\n", sep = "")

  t0 <- Sys.time()
  tryCatch(
    source(here::here("scripts", script), local = new.env(), encoding = "UTF-8"),
    error = function(e) {
      stop("A etapa ", script, " falhou: ", conditionMessage(e),
           "\nCorrija e rode de novo (as etapas anteriores já salvaram seus arquivos).",
           call. = FALSE)
    }
  )
  registro <- c(registro, sprintf("%-48s %6.1f min", script, minutos_desde(t0)))
  cat("\nOK:", script, "em", minutos_desde(t0), "min\n")
}

dt_total <- minutos_desde(inicio_total)

writeLines(c(
  paste("Execução completa do pipeline em", format(Sys.time(), "%d/%m/%Y %H:%M")),
  paste("Tempo total:", dt_total, "min"),
  "",
  registro,
  "",
  capture.output(sessionInfo())
), here::here("registro_execucao.txt"))

cat("\nPipeline concluído em", dt_total, "min. Registro salvo em registro_execucao.txt\n")
