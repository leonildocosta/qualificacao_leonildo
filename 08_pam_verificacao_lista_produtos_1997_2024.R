# 08_pam_verificacao_lista_produtos_1997_2024.R
# Verifica se algum produto entrou ou saiu da lista da PAM em 1997-2024, olhando o
# Brasil inteiro: "..." (não disponível) em alguns anos e valor em outros indica mudança;
# "-" é só produção zero.
# Saída: 06_verificacao_metodologica/dados/verificacao_lista_produtos_pam_1997_2024.xlsx

source(here::here("scripts", "funcoes_comuns.R"))
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(here)

arq_cw <- here("05_painel", "dados", "crosswalk_produto_pam_ncm_1997_2024.xlsx")
exigir_arquivo(arq_cw, "04_crosswalk_produto_pam_ncm.R")

# 1. Consulta ao SIDRA: Brasil, quantidade produzida (214) e área colhida (216),
#    todos os produtos da c782 exceto o Total. ~4.800 valores: uma chamada.
sidra <- sprintf(
  "https://apisidra.ibge.gov.br/values/t/5457/n1/all/v/214,216/p/%d-%d/c782/allxt",
  min(ANOS), max(ANOS)
) %>%
  consultar_api_sidra() %>%
  transmute(
    variavel       = `Variável`,
    ano            = as.integer(Ano),
    produto_codigo = `Produto das lavouras temporárias e permanentes (Código)`,
    produto        = `Produto das lavouras temporárias e permanentes`,
    simbolo        = Valor
  )

stopifnot(
  nrow(sidra) > 0,
  setequal(sidra$ano, ANOS),
  !any(grepl("^[0-9]+$", sidra$produto))  # produto deve vir como nome, não código
)

# 2. Estado de cada célula -----------------------------------------------------
sidra <- sidra %>%
  mutate(
    numero = suppressWarnings(as.numeric(simbolo)),
    estado = case_when(
      simbolo == "-"   ~ "zero",
      simbolo == "..." ~ "nao_disponivel",
      simbolo == ".."  ~ "nao_se_aplica",
      simbolo == "X"   ~ "sigilo",
      numero > 0       ~ "positivo",
      numero == 0      ~ "zero",
      TRUE             ~ paste0("outro:", simbolo)
    )
  )

cat("\nSímbolos encontrados:\n")
print(count(sidra, variavel, estado))

# A análise usa a quantidade produzida; a área colhida fica como conferência
qtd <- filter(sidra, grepl("^Quantidade", variavel))

# 3. Resumo por produto --------------------------------------------------------
resumo_por_produto <- qtd %>%
  arrange(produto, ano) %>%
  group_by(produto_codigo, produto) %>%
  summarise(
    anos_positivo         = sum(estado == "positivo"),
    anos_zero             = sum(estado == "zero"),
    anos_nao_disponivel   = sum(estado == "nao_disponivel"),
    anos_sigilo           = sum(estado == "sigilo"),
    primeiro_ano_com_dado = if (any(estado %in% c("positivo", "zero")))
                              min(ano[estado %in% c("positivo", "zero")]) else NA_integer_,
    ultimo_ano_com_dado   = if (any(estado %in% c("positivo", "zero")))
                              max(ano[estado %in% c("positivo", "zero")]) else NA_integer_,
    anos_nao_disponivel_lista = paste(ano[estado == "nao_disponivel"], collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(
    situacao = case_when(
      anos_nao_disponivel == 0            ~ "pesquisado_todo_periodo",
      anos_nao_disponivel == length(ANOS) ~ "nao_pesquisado_no_periodo",
      primeiro_ano_com_dado > min(ANOS) & ultimo_ano_com_dado == max(ANOS) ~ "entrou_na_janela",
      primeiro_ano_com_dado == min(ANOS) & ultimo_ano_com_dado < max(ANOS) ~ "saiu_na_janela",
      TRUE                                ~ "lacunas_na_janela"
    )
  ) %>%
  arrange(situacao, produto)

mudancas_de_lista <- resumo_por_produto %>%
  filter(situacao %in% c("entrou_na_janela", "saiu_na_janela", "lacunas_na_janela"))

# 4. Os 35 produtos do painel --------------------------------------------------
produtos_painel <- read_excel(arq_cw) %>%
  select(produto = produto_pam, status_ncm) %>%
  left_join(resumo_por_produto, by = "produto") %>%
  arrange(situacao, produto)

sem_par <- produtos_painel$produto[is.na(produtos_painel$situacao)]
if (length(sem_par) > 0) {
  warning("Produtos do crosswalk sem correspondência no SIDRA nacional: ",
          paste(sem_par, collapse = "; "))
}

# 5. Matriz produto x ano ------------------------------------------------------
simbolos_ano <- qtd %>%
  transmute(produto, ano, celula = if_else(estado == "positivo", "valor", simbolo)) %>%
  pivot_wider(names_from = ano, values_from = celula, names_prefix = "ano_") %>%
  arrange(produto)

# 6. Console -------------------------------------------------------------------
cat("\n=== Situação dos produtos no Brasil, ", min(ANOS), "-", max(ANOS), " ===\n", sep = "")
print(count(resumo_por_produto, situacao))

cat("\nProdutos que entraram, saíram ou têm lacunas na janela:\n")
print(select(mudancas_de_lista, produto, situacao, primeiro_ano_com_dado,
             ultimo_ano_com_dado, anos_nao_disponivel_lista), n = Inf)

cat("\nOs 35 produtos do painel:\n")
print(count(produtos_painel, situacao))
# Produto sem correspondência (situacao NA) também conta como problema
afetados <- filter(produtos_painel, is.na(situacao) | situacao != "pesquisado_todo_periodo")
if (nrow(afetados) == 0) {
  cat("Nenhum produto do painel entrou ou saiu da pesquisa entre ",
      min(ANOS), " e ", max(ANOS), ".\n", sep = "")
} else {
  cat("ATENÇÃO: produtos do painel com mudança na janela:\n")
  print(select(afetados, produto, situacao, anos_nao_disponivel_lista), n = Inf)
}

# 7. Saída ---------------------------------------------------------------------
pasta <- here("06_verificacao_metodologica", "dados")
dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
arquivo <- file.path(pasta, "verificacao_lista_produtos_pam_1997_2024.xlsx")
write_xlsx(list(
  resumo_por_produto = resumo_por_produto,
  mudancas_de_lista  = mudancas_de_lista,
  produtos_painel    = produtos_painel,
  simbolos_ano       = simbolos_ano
), arquivo)
cat("\nArquivo salvo em:", arquivo, "\n")
