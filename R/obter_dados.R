# ============================================================
# obter_dados.R — Coleta de dados reais de fontes oficiais
# ============================================================
#
# Este script busca dados de:
#   - IBGE/SIDRA (tabelas de projeções populacionais)
#   - IBGE/SIDRA (mortalidade infantil - projeções)
#
# Cada dataset é salvo como .rds em data/ com atributos de metadados:
#   - fonte: instituição responsável
#   - tabela: código/nome da tabela
#   - url: link para acesso manual
#   - descricao: o que o dado representa
#   - data_coleta: quando foi baixado
#   - notas: observações relevantes
#
# Para executar: source("R/obter_dados.R") ou Rscript R/obter_dados.R
# ============================================================

library(sidrar)
library(dplyr)
library(tidyr)

dir.create("data", showWarnings = FALSE)

# Função auxiliar: salvar .rds com metadados
salvar_dados <- function(df, nome_arquivo, fonte, tabela, url, descricao, notas = "") {
  attr(df, "fonte")       <- fonte
  attr(df, "tabela")      <- tabela
  attr(df, "url")         <- url
  attr(df, "descricao")   <- descricao
  attr(df, "data_coleta") <- Sys.Date()
  attr(df, "notas")       <- notas

  caminho <- file.path("data", paste0(nome_arquivo, ".rds"))
  saveRDS(df, caminho)
  cat("[OK]", nome_arquivo, "->", caminho, "(", nrow(df), "linhas )\n")
}

# ============================================================
# 1. IBGE Projeções (Tabela 7360) — Brasil
#    TFT, TBN, TBM, Índice de Envelhecimento, etc.
# ============================================================
cat("\n=== Baixando IBGE Projeções — Brasil (tabela 7360) ===\n")

proj_br <- sidrar::get_sidra(7360, period = "all", geo = "Brazil")
names(proj_br)[c(8, 9)]   <- c("ano_pub_cod", "ano_pub")
names(proj_br)[c(12, 13)] <- c("ano_proj_cod", "ano_proj")

limpar_proj <- function(df, padrao_variavel) {
  df %>%
    filter(grepl(padrao_variavel, Variável, ignore.case = TRUE)) %>%
    mutate(
      ano   = as.numeric(as.character(ano_proj)),
      valor = as.numeric(as.character(Valor))
    ) %>%
    filter(!is.na(valor)) %>%
    select(ano, valor) %>%
    arrange(ano)
}

# 1a. Taxa de Fecundidade Total (TFT) — Brasil
tft_brasil <- limpar_proj(proj_br, "fecundidade total")
salvar_dados(tft_brasil, "tft_brasil",
  fonte     = "IBGE — Projeções da População do Brasil (Revisão 2018)",
  tabela    = "SIDRA 7360 — Variável: Taxa de fecundidade total",
  url       = "https://sidra.ibge.gov.br/tabela/7360",
  descricao = "Taxa de Fecundidade Total (filhos por mulher) — Brasil, série 2000-2060"
)

# 1b. Taxa Bruta de Natalidade (TBN) — Brasil
tbn_brasil <- limpar_proj(proj_br, "bruta de natalidade")
salvar_dados(tbn_brasil, "tbn_brasil",
  fonte     = "IBGE — Projeções da População do Brasil (Revisão 2018)",
  tabela    = "SIDRA 7360 — Variável: Taxa bruta de natalidade",
  url       = "https://sidra.ibge.gov.br/tabela/7360",
  descricao = "Taxa Bruta de Natalidade (por 1.000 hab.) — Brasil, série 2000-2060"
)

# 1c. Taxa Bruta de Mortalidade (TBM) — Brasil
tbm_brasil <- limpar_proj(proj_br, "bruta de mortalidade")
salvar_dados(tbm_brasil, "tbm_brasil",
  fonte     = "IBGE — Projeções da População do Brasil (Revisão 2018)",
  tabela    = "SIDRA 7360 — Variável: Taxa bruta de mortalidade",
  url       = "https://sidra.ibge.gov.br/tabela/7360",
  descricao = "Taxa Bruta de Mortalidade (por 1.000 hab.) — Brasil, série 2010-2060"
)

# 1d. Índice de Envelhecimento — Brasil
ie_brasil <- limpar_proj(proj_br, "envelhecimento")
salvar_dados(ie_brasil, "ie_brasil",
  fonte     = "IBGE — Projeções da População do Brasil (Revisão 2018)",
  tabela    = "SIDRA 7360 — Variável: Índice de envelhecimento",
  url       = "https://sidra.ibge.gov.br/tabela/7360",
  descricao = "Índice de Envelhecimento (idosos 65+ / jovens 0-14 × 100) — Brasil, série"
)

# 1e. Razão de Dependência Total — Brasil
rd_brasil <- limpar_proj(proj_br, "dependência total")
salvar_dados(rd_brasil, "rd_brasil",
  fonte     = "IBGE — Projeções da População do Brasil (Revisão 2018)",
  tabela    = "SIDRA 7360 — Variável: Razão de dependência total",
  url       = "https://sidra.ibge.gov.br/tabela/7360",
  descricao = "Razão de Dependência Total — Brasil"
)

# 1f. Nascimentos — Brasil (para cálculos)
nasc_brasil <- limpar_proj(proj_br, "^Nascimentos$")
salvar_dados(nasc_brasil, "nascimentos_brasil",
  fonte     = "IBGE — Projeções da População do Brasil (Revisão 2018)",
  tabela    = "SIDRA 7360 — Variável: Nascimentos",
  url       = "https://sidra.ibge.gov.br/tabela/7360",
  descricao = "Número de nascimentos projetados — Brasil"
)

# ============================================================
# 2. IBGE Projeções (Tabela 7360) — Por Região
# ============================================================
cat("\n=== Baixando IBGE Projeções — Regiões (tabela 7360) ===\n")

proj_reg <- sidrar::get_sidra(7360, period = "all", geo = "Region")
names(proj_reg)[c(8, 9)]   <- c("ano_pub_cod", "ano_pub")
names(proj_reg)[c(12, 13)] <- c("ano_proj_cod", "ano_proj")

limpar_proj_reg <- function(df, padrao_variavel) {
  df %>%
    filter(grepl(padrao_variavel, Variável, ignore.case = TRUE)) %>%
    mutate(
      regiao = `Grande Região`,
      ano    = as.numeric(as.character(ano_proj)),
      valor  = as.numeric(as.character(Valor))
    ) %>%
    filter(!is.na(valor)) %>%
    select(regiao, ano, valor) %>%
    arrange(regiao, ano)
}

# 2a. TFT por região
tft_regiao <- limpar_proj_reg(proj_reg, "fecundidade total")
salvar_dados(tft_regiao, "tft_regiao",
  fonte     = "IBGE — Projeções da População do Brasil (Revisão 2018)",
  tabela    = "SIDRA 7360 — TFT por Grande Região",
  url       = "https://sidra.ibge.gov.br/tabela/7360",
  descricao = "Taxa de Fecundidade Total por Grande Região — série 2000-2060"
)

# 2b. TBN por região
tbn_regiao <- limpar_proj_reg(proj_reg, "bruta de natalidade")
salvar_dados(tbn_regiao, "tbn_regiao",
  fonte     = "IBGE — Projeções da População do Brasil (Revisão 2018)",
  tabela    = "SIDRA 7360 — TBN por Grande Região",
  url       = "https://sidra.ibge.gov.br/tabela/7360",
  descricao = "Taxa Bruta de Natalidade por Grande Região — série"
)

# 2c. TBM por região
tbm_regiao <- limpar_proj_reg(proj_reg, "bruta de mortalidade")
salvar_dados(tbm_regiao, "tbm_regiao",
  fonte     = "IBGE — Projeções da População do Brasil (Revisão 2018)",
  tabela    = "SIDRA 7360 — TBM por Grande Região",
  url       = "https://sidra.ibge.gov.br/tabela/7360",
  descricao = "Taxa Bruta de Mortalidade por Grande Região — série"
)

# ============================================================
# 3. IBGE Projeções (Tabela 7362) — MI e Esperança de Vida
# ============================================================
cat("\n=== Baixando MI e Esperança de Vida (tabela 7362) ===\n")

proj_7362 <- sidrar::get_sidra(7362, period = "all", geo = "Brazil")
names(proj_7362)[c(8, 9)]   <- c("ano_pub_cod", "ano_pub")
names(proj_7362)[c(14, 15)] <- c("ano_proj_cod", "ano_proj")

limpar_7362 <- function(df, padrao_variavel, padrao_sexo = "Total") {
  df %>%
    filter(
      grepl(padrao_variavel, Variável, ignore.case = TRUE),
      Sexo == padrao_sexo
    ) %>%
    mutate(
      ano   = as.numeric(as.character(ano_proj)),
      valor = as.numeric(as.character(Valor))
    ) %>%
    filter(!is.na(valor)) %>%
    select(ano, valor) %>%
    arrange(ano)
}

# 3a. Mortalidade Infantil — Brasil
mi_brasil <- limpar_7362(proj_7362, "mortalidade infantil")
salvar_dados(mi_brasil, "mi_brasil",
  fonte     = "IBGE — Projeções da População do Brasil (Revisão 2018)",
  tabela    = "SIDRA 7362 — Variável: Taxa de mortalidade infantil",
  url       = "https://sidra.ibge.gov.br/tabela/7362",
  descricao = "Taxa de Mortalidade Infantil (por 1.000 NV) — Brasil, série 2000-2060",
  notas     = "Valores são projeções do IBGE, não dados observados do SIM. Para dados observados, consultar SIM/DATASUS."
)

# 3b. Esperança de Vida ao Nascer — Brasil (Total, Masc, Fem)
ev_total <- limpar_7362(proj_7362, "Esperança", "Total")
ev_masc  <- limpar_7362(proj_7362, "Esperança", "Homens")
ev_fem   <- limpar_7362(proj_7362, "Esperança", "Mulheres")

ev_brasil <- ev_total %>%
  rename(total = valor) %>%
  left_join(ev_masc %>% rename(masculino = valor), by = "ano") %>%
  left_join(ev_fem %>% rename(feminino = valor), by = "ano")

salvar_dados(ev_brasil, "ev_brasil",
  fonte     = "IBGE — Projeções da População do Brasil (Revisão 2018)",
  tabela    = "SIDRA 7362 — Variável: Esperança de vida ao nascer",
  url       = "https://sidra.ibge.gov.br/tabela/7362",
  descricao = "Esperança de vida ao nascer (anos) — Brasil, por sexo, série 2000-2060"
)

# 3c. MI por região
cat("\n=== Baixando MI por Região (tabela 7362) ===\n")
proj_7362_reg <- sidrar::get_sidra(7362, period = "all", geo = "Region")
names(proj_7362_reg)[c(8, 9)]   <- c("ano_pub_cod", "ano_pub")
names(proj_7362_reg)[c(14, 15)] <- c("ano_proj_cod", "ano_proj")

mi_regiao <- proj_7362_reg %>%
  filter(
    grepl("mortalidade infantil", Variável, ignore.case = TRUE),
    Sexo == "Total"
  ) %>%
  mutate(
    regiao = `Grande Região`,
    ano    = as.numeric(as.character(ano_proj)),
    valor  = as.numeric(as.character(Valor))
  ) %>%
  filter(!is.na(valor)) %>%
  select(regiao, ano, valor) %>%
  arrange(regiao, ano)

salvar_dados(mi_regiao, "mi_regiao",
  fonte     = "IBGE — Projeções da População do Brasil (Revisão 2018)",
  tabela    = "SIDRA 7362 — MI por Grande Região",
  url       = "https://sidra.ibge.gov.br/tabela/7362",
  descricao = "Taxa de Mortalidade Infantil por Grande Região — série 2000-2060"
)

# ============================================================
# 4. IBGE Projeções — População por faixa etária (pirâmide)
#    Tabela 7358
# ============================================================
cat("\n=== Baixando população por faixa etária (tabela 7358) ===\n")

proj_7358 <- sidrar::get_sidra(7358, period = "all", geo = "Brazil")
names(proj_7358)[c(8, 9)]   <- c("ano_pub_cod", "ano_pub")
names(proj_7358)[c(16, 17)] <- c("ano_proj_cod", "ano_proj")

piramide <- proj_7358 %>%
  mutate(
    ano   = as.numeric(as.character(ano_proj)),
    valor = as.numeric(as.character(Valor)),
    sexo  = proj_7358[["Sexo"]],
    faixa = Idade
  ) %>%
  filter(!is.na(valor), !is.na(ano), sexo != "Total", faixa != "Total",
         ano %in% c(2000, 2010, 2020, 2030, 2040)) %>%
  select(ano, sexo, faixa, valor) %>%
  arrange(ano, sexo, faixa)

salvar_dados(piramide, "piramide_brasil",
  fonte     = "IBGE — Projeções da População do Brasil (Revisão 2018)",
  tabela    = "SIDRA 7358 — População por grupo de idade e sexo",
  url       = "https://sidra.ibge.gov.br/tabela/7358",
  descricao = "População por grupo de idade, sexo e ano — Brasil (projeções), anos selecionados"
)

# ============================================================
# RESUMO FINAL
# ============================================================
cat("\n==========================================\n")
cat("COLETA FINALIZADA\n")
cat("==========================================\n")
arquivos <- list.files("data", pattern = "\\.rds$")
for (arq in arquivos) {
  df <- readRDS(file.path("data", arq))
  cat(sprintf("  %-25s  %4d linhas  |  %s\n",
              arq, nrow(df), attr(df, "fonte")))
}
cat("==========================================\n")
