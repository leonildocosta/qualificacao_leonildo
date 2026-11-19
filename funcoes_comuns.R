# funcoes_comuns.R
# Configuração e funções usadas por vários scripts do pipeline.

ANOS     <- 1997:2024
UF_PIAUI <- 22

# Locale UTF-8: sem ela, os acentos podem sair corrompidos dentro dos PNG
for (loc in c("pt_BR.UTF-8", "en_US.UTF-8", "C.UTF-8")) {
  if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
}

# Pacotes -----------------------------------------------------------------------
pacotes <- c("comexr", "dplyr", "forcats", "ggplot2", "here", "httr2", "jsonlite",
             "purrr", "readxl", "scales", "sidrar", "stringr", "tidyr", "writexl")
faltando <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltando) > 0) install.packages(faltando)
rm(loc, pacotes, faltando)

# Entradas ----------------------------------------------------------------------
exigir_arquivo <- function(caminho, script) {
  if (!file.exists(caminho)) {
    stop("Arquivo não encontrado: ", caminho, "\nRode antes o script ", script, call. = FALSE)
  }
}

# API do SIDRA ------------------------------------------------------------------
# Lê a API direto (sem o sidrar) para preservar na coluna Valor os símbolos do
# IBGE: "-" zero, "..." não disponível, ".." não se aplica, "X" sigilo.
# A primeira linha da resposta traz os rótulos, que viram os nomes das colunas.
consultar_api_sidra <- function(url) {
  cat("Consultando o SIDRA em", format(Sys.time()), "\n ", url, "\n")
  resposta <- httr2::request(url) |>
    httr2::req_retry(max_tries = 5, backoff = ~ 15) |>
    httr2::req_timeout(300) |>
    httr2::req_perform()
  bruto <- jsonlite::fromJSON(httr2::resp_body_string(resposta), simplifyVector = TRUE)
  dados <- bruto[-1, , drop = FALSE]
  names(dados) <- unlist(bruto[1, ])
  tibble::as_tibble(dados)
}

# Gráficos ----------------------------------------------------------------------
tema_dissertacao <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.05)),
      plot.subtitle    = ggplot2::element_text(color = "grey30", size = ggplot2::rel(0.85)),
      plot.caption     = ggplot2::element_text(color = "grey40", size = ggplot2::rel(0.7), hjust = 0),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom",
      legend.title     = ggplot2::element_blank(),
      axis.title       = ggplot2::element_text(face = "bold"),
      strip.text       = ggplot2::element_text(face = "bold")
    )
}

rotulo_ano <- function(x) scales::label_number(accuracy = 1, big.mark = "")(x)
rotulo_num <- function(x) scales::label_number(big.mark = ".", decimal.mark = ",")(x)

escala_anos <- function(n = 10, ...) {
  ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n), labels = rotulo_ano, ...)
}

# PNG a 300 dpi (para vetorial, troque a extensão por .pdf)
salvar_grafico <- function(g, arquivo, largura = 10, altura = 6) {
  ggplot2::ggsave(arquivo, g, width = largura, height = altura, dpi = 300,
                  bg = "white", limitsize = FALSE)
  cat("  gráfico salvo:", basename(arquivo), "\n")
}

# Unidades nos cabeçalhos das planilhas ----------------------------------------
# Só para os .xlsx de leitura humana; os .rds mantêm os nomes originais da PAM,
# que são os que os scripts usam. Quantidade e rendimento têm exceções (frutas
# até 2000, abacaxi e coco): a unidade real de cada linha fica nas colunas
# unidade_quantidade e unidade_rendimento, colocadas logo ao lado.
NOMES_COM_UNIDADE <- c(
  "Área plantada ou destinada à colheita (ha)" = "Área plantada ou destinada à colheita",
  "Área colhida (ha)"                          = "Área colhida",
  "Quantidade produzida (t)"                   = "Quantidade produzida",
  "Rendimento médio da produção (kg/ha)"       = "Rendimento médio da produção",
  "Valor da produção (mil R$)"                 = "Valor da produção"
)
rotular_unidades <- function(df) {
  df <- dplyr::rename(df, dplyr::any_of(NOMES_COM_UNIDADE))
  if (all(c("unidade_quantidade", "unidade_rendimento") %in% names(df))) {
    df <- df |>
      dplyr::relocate(unidade_quantidade, .after = dplyr::any_of("Quantidade produzida (t)")) |>
      dplyr::relocate(unidade_rendimento, .after = dplyr::any_of("Rendimento médio da produção (kg/ha)"))
  }
  df
}
