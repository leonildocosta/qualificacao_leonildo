# 04_crosswalk_produto_pam_ncm.R
# Correspondência entre os 35 produtos da PAM e os NCMs do Comex Stat. Não existe
# tabela oficial: cada ligação segue a descrição do NCM, conferida contra as
# exportações reais do Piauí. O motivo de cada decisão está na coluna observacao.
# Saída: 05_painel/dados/crosswalk_produto_pam_ncm_1997_2024.xlsx
#
# Fora de propósito: 12074090 (gergelim, só pesquisado pela PAM a partir de 2025)
# e 12072900 (sementes de algodão, US$ 55 em 28 anos).

source(here::here("scripts", "funcoes_comuns.R"))
library(dplyr)
library(writexl)
library(here)

crosswalk_ncm <- tribble(
  ~produto_pam,                   ~codigos_ncm,                                    ~status_ncm,                                       ~observacao,
  "Soja (em grão)",               "12019000,12010090,12010010",                    "confirmado_real",                                 "12010090 é o código anterior à revisão da NCM; 12010010 = soja para semeadura",
  "Milho (em grão)",              "10059010",                                      "confirmado_real",                                 "",
  "Sorgo (em grão)",              "10079000",                                      "confirmado_real",                                 "",
  "Algodão herbáceo (em caroço)", "52010020,52010010",                             "confirmado_real",                                 "NCM é de algodão em pluma (debulhado/não debulhado); PAM mede em caroço",
  "Castanha de caju",             "08013200",                                      "confirmado_real",                                 "NCM é sem casca; PAM mede com casca. Coco-da-baía não tem exportação no SH4 0801",
  "Feijão (em grão)",             "07133190,07133290,07133399,07133590,07133990",  "confirmado_real",                                 "Todos os NCMs exportados do SH4 0713 são variedades de feijão; fava = 0",
  "Limão",                        "08053000,08055000",                             "confirmado_real",                                 "08053000 é o código anterior à revisão da NCM; laranja e tangerina = 0",
  "Melão",                        "08071900",                                      "confirmado_real",                                 "",
  "Melancia",                     "08071100",                                      "confirmado_real",                                 "",
  "Mamão",                        "08072000",                                      "confirmado_real",                                 "",
  "Uva",                          "08061000",                                      "confirmado_real",                                 "",
  "Manga",                        "08045020,08045000",                             "confirmado_real_compartilhado_rateio",            "08045020 é só manga; 08045000 (goiabas e mangas) é rateado com a goiaba",
  "Goiaba",                       "08045000",                                      "confirmado_real_compartilhado_rateio",            "08045000 (goiabas e mangas) é rateado com a manga",
  "Banana (cacho)",               "08030000",                                      "confirmado_real_valor_irrisorio",                 "",
  "Alho",                         "07032090",                                      "confirmado_real_valor_irrisorio",                 "Cebola = 0 no SH4 0703",
  "Urucum (semente)",             "12079990",                                      "confirmado_real_valor_irrisorio_por_eliminacao",  "Resíduo do NCM genérico de outras sementes oleaginosas; mamona = 0",
  "Café (em grão) Total",         "09011110",                                      "confirmado_real_divergencia_grave",               "Produção em poucos anos e exportação quase nula pela UF do Produto",
  "Cana-de-açúcar",               "17019900",                                      "confirmado_real_produto_processado",              "Exportação é de açúcar, não de cana",
  "Algodão arbóreo (em caroço)",  NA,                                              "zero_por_regra_produto_dominante",                "NCM 5201 não separa herbáceo e arbóreo; tudo atribuído ao herbáceo",
  "Abacate",                      NA,                                              "zero_confirmado_28_anos",                         "",
  "Abacaxi*",                     NA,                                              "zero_confirmado_28_anos",                         "PAM em mil frutos",
  "Amendoim (em casca)",          NA,                                              "zero_confirmado_28_anos",                         "",
  "Arroz (em casca)",             NA,                                              "zero_confirmado_28_anos",                         "",
  "Batata-doce",                  NA,                                              "zero_confirmado_28_anos",                         "",
  "Cebola",                       NA,                                              "zero_confirmado_28_anos",                         "",
  "Coco-da-baía*",                NA,                                              "zero_confirmado_28_anos",                         "PAM em mil frutos",
  "Fava (em grão)",               NA,                                              "zero_confirmado_28_anos",                         "SH4 0713 exportado é todo feijão",
  "Fumo (em folha)",              NA,                                              "zero_confirmado_28_anos",                         "",
  "Laranja",                      NA,                                              "zero_confirmado_28_anos",                         "SH4 0805 exportado é todo limão",
  "Mamona (baga)",                NA,                                              "zero_confirmado_28_anos",                         "SH4 1207 exportado é gergelim (fora do escopo) e urucum",
  "Mandioca",                     NA,                                              "zero_confirmado_28_anos",                         "",
  "Maracujá",                     NA,                                              "zero_confirmado_28_anos",                         "Sem NCM específico com exportação",
  "Pimenta-do-reino",             NA,                                              "zero_confirmado_28_anos",                         "",
  "Tangerina",                    NA,                                              "zero_confirmado_28_anos",                         "SH4 0805 exportado é todo limão",
  "Tomate",                       NA,                                              "zero_confirmado_28_anos",                         ""
)

# Conferências
codigos <- unlist(strsplit(na.omit(crosswalk_ncm$codigos_ncm), ","))
stopifnot(
  nrow(crosswalk_ncm) == 35,
  !anyDuplicated(crosswalk_ncm$produto_pam),
  all(nchar(codigos) == 8),
  sum(codigos == "08045000") == 2,          # único NCM compartilhado (manga e goiaba)
  !anyDuplicated(codigos[codigos != "08045000"])
)

pasta <- here("05_painel", "dados")
dir.create(pasta, recursive = TRUE, showWarnings = FALSE)
write_xlsx(crosswalk_ncm, file.path(pasta, "crosswalk_produto_pam_ncm_1997_2024.xlsx"))

cat("Crosswalk salvo:", nrow(crosswalk_ncm), "produtos,",
    sum(!is.na(crosswalk_ncm$codigos_ncm)), "com NCM,",
    length(unique(codigos)), "NCMs distintos.\n")
