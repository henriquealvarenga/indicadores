# ============================================================
# utils.R — Funções auxiliares para o livreto Indicadores de Saúde
# ============================================================

# Pacotes necessários ----
pacotes <- c(
  "tidyverse",    # manipulação e visualização
  "httr2",        # requisições HTTP
  "janitor",      # limpeza de dados
  "scales",       # formatação de eixos
  "knitr",        # tabelas
  "kableExtra",   # tabelas avançadas
  "glue",         # strings interpoladas
  "patchwork"     # composição de gráficos
)

# Instalar pacotes faltantes
instalar_se_necessario <- function(pkgs) {
  faltantes <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(faltantes) > 0) {
    install.packages(faltantes, repos = "https://cran.r-project.org")
  }
}

# Tema padrão para gráficos ----
tema_indicadores <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size * 1.2, color = "#2c3e50"),
      plot.subtitle = element_text(color = "#6c757d", size = base_size * 0.9),
      plot.caption = element_text(color = "#adb5bd", size = base_size * 0.7, hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_text(color = "#495057"),
      axis.text = element_text(color = "#6c757d"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = base_size * 0.85),
      strip.text = element_text(face = "bold", size = base_size * 0.9)
    )
}

# Paleta de cores padrão ----
cores_indicadores <- c(
  "destaque"  = "#2c7fb8",
  "alerta"    = "#d95f02",
  "sucesso"   = "#1b9e77",
  "perigo"    = "#e31a1c",
  "neutro"    = "#6c757d",
  "branca"    = "#4575b4",
  "parda"     = "#fc8d59",
  "preta"     = "#d73027",
  "indigena"  = "#1a9850",
  "amarela"   = "#fee08b"
)

# Cores para regiões do Brasil
cores_regioes <- c(
  "Norte"        = "#1b9e77",
  "Nordeste"     = "#d95f02",
  "Sudeste"      = "#7570b3",
  "Sul"          = "#e7298a",
  "Centro-Oeste" = "#66a61e"
)

# Formatar números no padrão brasileiro ----
fmt_br <- function(x, digits = 1) {
  formatC(x, format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
}

# Tabela formatada padrão ----
tabela_indicador <- function(df, caption = NULL) {
  kable(df, caption = caption, align = "l") |>
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      font_size = 14
    )
}

# Detectar raiz do projeto ----
raiz_projeto <- function() {
  dir <- getwd()
  for (i in 1:5) {
    if (file.exists(file.path(dir, "_quarto.yml"))) return(dir)
    dir <- dirname(dir)
  }
  getwd()
}

# Carregar dados reais com metadados ----
carregar_dados <- function(nome_arquivo) {
  caminho <- file.path(raiz_projeto(), "data", paste0(nome_arquivo, ".rds"))
  if (!file.exists(caminho)) {
    stop("Arquivo de dados não encontrado: ", nome_arquivo,
         "\nExecute Rscript R/obter_dados.R para baixar os dados.")
  }
  readRDS(caminho)
}

# Exibir fonte dos dados (string curta para caption de gráficos) ----
fonte_dados <- function(nome_arquivo) {
  caminho <- file.path(raiz_projeto(), "data", paste0(nome_arquivo, ".rds"))
  if (!file.exists(caminho)) return("Fonte: dados não encontrados")
  df <- readRDS(caminho)
  fonte <- attr(df, "fonte")
  tabela <- attr(df, "tabela")
  if (is.null(fonte)) fonte <- "Não informada"
  if (is.null(tabela)) tabela <- ""
  paste0("Fonte: ", fonte, " | ", tabela)
}
