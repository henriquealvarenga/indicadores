# Indicadores de Saúde

Material didático sobre indicadores de saúde, com exemplos reproduzíveis em R e dados do Brasil. Construído com [Quarto](https://quarto.org/).

**Site publicado:** https://henriquealvarenga.com/indicadores

---

## ⚠️ Antes de fazer `git push`: rode `quarto render` localmente

Este projeto **executa código R** durante a renderização. Para manter o GitHub Actions rápido e estável, ele **não instala R** — apenas monta o HTML a partir dos resultados já congelados em `_freeze/`.

Isso significa que **você** precisa renderizar localmente antes de empurrar para o GitHub. Se esquecer, o site no ar fica desatualizado em relação ao código.

### Fluxo de publicação

```bash
quarto render                  # 1. renderiza localmente → atualiza _freeze/
git add -A
git commit -m "sua mensagem"
git push                       # 2. GitHub Actions monta o HTML e publica
```

---

## Estrutura do projeto

| Caminho | Função |
|---|---|
| `index.qmd`, `about.qmd`, `metodologia.qmd`, `references.qmd` | Páginas principais |
| `chapters/*.qmd` | Capítulos do material |
| `R/obter_dados.R` | **Aquisição** de dados via `sidrar` (IBGE/SIDRA). Rode manualmente. |
| `R/utils.R` | Funções auxiliares usadas nos chapters |
| `data/*.rds` | Snapshots versionados dos dados (lidos pelos chapters via `readRDS`) |
| `references/` + `references.bib` | Bibliografia |
| `_freeze/` | Resultados congelados das chunks R — **versionado**, regenerado por `quarto render` |
| `.github/workflows/publish.yml` | Workflow que monta o HTML e publica no GitHub Pages |

`_quarto.yml` define `output-dir: docs`, mas `docs/` **não é versionado** — o workflow o gera a cada push.

---

## Atualizando os dados (raro)

Os arquivos `.rds` em `data/` são snapshots. Para atualizar:

```r
source("R/obter_dados.R")   # consulta IBGE/SIDRA, sobrescreve data/*.rds
```

Depois rode `quarto render` como de costume e empurre.

---

## Por que essa estratégia (e não rodar R no GitHub Actions)?

- **Mais rápido:** o GitHub Actions faz só `quarto render` (~30s) em vez de instalar R + pacotes (~3-5min).
- **Mais estável:** sem risco de quebrar quando algum pacote do CRAN é atualizado.
- **Sem `renv` para manter:** simplifica o projeto.

O preço é o passo manual `quarto render` antes do push.
