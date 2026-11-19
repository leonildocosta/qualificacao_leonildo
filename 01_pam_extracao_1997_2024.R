# 01_pam_extracao_1997_2024.R
# Extrai a PAM/IBGE (tabela 5457) do Piauí, nível estadual (o rendimento não é
# somável entre municípios). 1997 é o primeiro ano com NCM estável nas exportações.
# Saídas (01_pam/dados/):
#   pam_piaui_estado_1997_2024_bruto.rds      dados (entrada dos scripts 02, 03 e 06)
#   pam_piaui_unidades_1997_2024.rds          unidade da quantidade e do rendimento, produto x ano
#   pam_piaui_estado_1997_2024.xlsx           abas dados, unidades_nao_padrao e dicionario
#
# ATENÇÃO, unidades: a quantidade e o rendimento não têm a mesma unidade em
# todas as linhas. Até 2000, o IBGE media as frutas em mil frutos (banana em
# mil cachos); a partir de 2001, em toneladas. Abacaxi e coco-da-baía seguem
# em mil frutos em todos os anos. Ver a seção "Unidades de medida" abaixo e a
# nota metodológica.

source(here::here("scripts", "funcoes_comuns.R"))
library(sidrar)
library(dplyr)
library(tidyr)
library(here)
library(writexl)

pasta <- here("01_pam", "dados")
dir.create(pasta, showWarnings = FALSE, recursive = TRUE)

# Todos os produtos da classificação c782, exceto o Total ("allxt").
# 8331 área plantada, 216 área colhida, 214 quantidade, 112 rendimento, 215 valor
consulta <- sprintf("/t/5457/n3/%d/v/8331,216,214,112,215/p/%d-%d/c782/allxt",
                    UF_PIAUI, min(ANOS), max(ANOS))

# 1ª tentativa: pacote sidrar. Se a conexão falhar ("cannot open the
# connection"), tenta de novo pela API direta (httr2, com novas tentativas).
# A versão anterior chamava info_sidra(), que lê uma página antiga do IBGE
# e falhou em 17/11/2026; a lista de produtos agora vem do próprio "allxt".
bruto <- tryCatch(
  get_sidra(api = consulta),
  error = function(e) {
    message("sidrar falhou (", conditionMessage(e), "). Usando a API direta.")
    consultar_api_sidra(paste0("https://apisidra.ibge.gov.br/values", consulta)) %>%
      mutate(Valor = suppressWarnings(as.numeric(Valor)))  # símbolos do IBGE -> NA, como no sidrar
  }
)

# Unidades de medida ----------------------------------------------------------
# A coluna "Unidade de Medida" da API do SIDRA NÃO serve para isso: ela traz o
# rótulo da variável na tabela ("Toneladas", "Quilogramas por Hectare") para
# todas as linhas, inclusive para as frutas em 1997-2000 e para abacaxi e coco,
# que não estão em toneladas (conferido em 18/11/2026). Por isso a unidade é
# atribuída pela regra documentada pelo IBGE (COAGRO, "Mudança da unidade de
# medida das frutíferas"): até 2000, frutíferas em mil frutos (banana em mil
# cachos); a partir de 2001, em toneladas; abacaxi e coco-da-baía em mil
# frutos em todos os anos; uva sempre em peso.
FRUTAS_MIL_FRUTOS_ATE_2000 <- c("Abacate", "Banana (cacho)", "Caqui", "Figo", "Goiaba",
                                "Laranja", "Limão", "Maçã", "Mamão", "Manga", "Maracujá",
                                "Marmelo", "Melancia", "Melão", "Pera", "Pêssego", "Tangerina")
MIL_FRUTOS_SEMPRE <- c("Abacaxi*", "Coco-da-baía*")
ANO_MUDANCA_FRUTAS <- 2001

unidade_quantidade_de <- function(produto, ano) {
  case_when(
    produto %in% MIL_FRUTOS_SEMPRE ~ "Mil frutos",
    produto == "Banana (cacho)" & ano < ANO_MUDANCA_FRUTAS ~ "Mil cachos",
    produto %in% FRUTAS_MIL_FRUTOS_ATE_2000 & ano < ANO_MUDANCA_FRUTAS ~ "Mil frutos",
    TRUE ~ "Toneladas"
  )
}

pam <- bruto %>%
  transmute(
    ano       = as.integer(Ano),
    codigo_uf = `Unidade da Federação (Código)`,
    uf        = `Unidade da Federação`,
    produto   = `Produto das lavouras temporárias e permanentes`,
    variavel  = `Variável`,
    valor     = Valor
  ) %>%
  pivot_wider(names_from = variavel, values_from = valor) %>%
  arrange(ano, produto)

# Produto sem nenhum dado no período não vem na resposta do SIDRA
stopifnot(n_distinct(pam$codigo_uf) == 1, setequal(pam$ano, ANOS))
cat("Categorias de produto:", n_distinct(pam$produto), "| linhas:", nrow(pam), "\n")

# Tabela de unidades (quantidade e rendimento) por produto x ano
unidades <- pam %>%
  distinct(ano, produto) %>%
  mutate(unidade_quantidade = unidade_quantidade_de(produto, ano),
         unidade_rendimento = recode(unidade_quantidade,
                                     "Toneladas"  = "Quilogramas por Hectare",
                                     "Mil frutos" = "Frutos por Hectare",
                                     "Mil cachos" = "Cachos por Hectare")) %>%
  arrange(produto, ano)

stopifnot(
  all(FRUTAS_MIL_FRUTOS_ATE_2000 %in% pam$produto),   # nomes conferem com a PAM
  all(MIL_FRUTOS_SEMPRE %in% pam$produto)
)

# Conferência empírica da quebra: para as frutas com dado em 2000 e 2001, o
# rendimento muda de patamar (frutos/ha -> kg/ha). Fica registrado no console.
quebra_2001 <- pam %>%
  filter(produto %in% FRUTAS_MIL_FRUTOS_ATE_2000, ano %in% c(2000, 2001)) %>%
  select(produto, ano, rend = `Rendimento médio da produção`) %>%
  pivot_wider(names_from = ano, values_from = rend, names_prefix = "rend_") %>%
  filter(!is.na(rend_2000), !is.na(rend_2001)) %>%
  mutate(razao_2001_2000 = round(rend_2001 / rend_2000, 2))
cat("\nRendimento das frutas em 2000 (frutos/ha) e 2001 (kg/ha), Piauí:\n")
print(quebra_2001, n = Inf)

com_dado <- pam %>%
  filter(if_any(c(`Quantidade produzida`, `Rendimento médio da produção`), ~ !is.na(.x))) %>%
  distinct(ano, produto)
unidades_nao_padrao <- unidades %>%
  semi_join(com_dado, by = c("ano", "produto")) %>%
  filter(unidade_quantidade != "Toneladas") %>%
  summarise(ano_inicial = min(ano), ano_final = max(ano), n_anos = n(),
            .by = c(produto, unidade_quantidade, unidade_rendimento)) %>%
  arrange(produto)
cat("\nProdutos com produção no Piauí fora de toneladas em algum ano:\n")
print(unidades_nao_padrao, n = Inf)

unidade_padrao <- c("Área plantada ou destinada à colheita" = "Hectares",
                    "Área colhida"                          = "Hectares",
                    "Quantidade produzida"                  = "Toneladas",
                    "Rendimento médio da produção"          = "Quilogramas por Hectare",
                    "Valor da produção"                     = "Mil Reais")

dicionario <- tibble(
  coluna = c("ano", "codigo_uf", "uf", "produto", names(unidade_padrao)),
  descricao = c("Ano de referência da safra", "Código IBGE da UF (Piauí = 22)", "Nome da UF",
                "Produto, como na classificação c782 da tabela 5457. O asterisco (Abacaxi*, Coco-da-baía*) indica quantidade em mil frutos",
                "Área plantada ou destinada à colheita", "Área efetivamente colhida",
                "Quantidade produzida no ano", "Quantidade produzida / área colhida",
                "Valor da produção, preços correntes (sem correção pela inflação)"),
  unidade = c("-", "-", "-", "-", unidade_padrao),
  excecoes = c("", "", "", "", "", "",
               "Frutas até 2000: mil frutos (banana: mil cachos); abacaxi e coco-da-baía: mil frutos em todos os anos. Ver aba unidades_nao_padrao",
               "Mesmas exceções da quantidade: frutos (ou cachos) por hectare",
               "")
)

saveRDS(pam,      file.path(pasta, "pam_piaui_estado_1997_2024_bruto.rds"))
saveRDS(unidades, file.path(pasta, "pam_piaui_unidades_1997_2024.rds"))
write_xlsx(list(dados = rotular_unidades(left_join(pam, unidades, by = c("ano", "produto"))),
                unidades_nao_padrao = unidades_nao_padrao, dicionario = dicionario),
           file.path(pasta, "pam_piaui_estado_1997_2024.xlsx"))
