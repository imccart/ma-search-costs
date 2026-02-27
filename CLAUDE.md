# MA Search Costs

## Project Overview

Research project studying search costs in Medicare Advantage markets.

## Project Structure

```
ma-search-costs/
├── paper/              # Main paper (Quarto .qmd)
├── presentations/      # Slide decks
├── data/
│   ├── input/          # Raw source data (never modified)
│   └── output/         # Cleaned/processed data
├── code/
│   ├── data-build/     # Scripts that transform input → output
│   └── analysis/       # Scripts that produce results from output
├── results/
│   ├── tables/
│   └── figures/
├── background/         # Literature notes, key references
└── scratch/            # Temporary/exploratory files
```

## Workflow

- Solo project using Quarto (.qmd) for the paper
- R scripts run in VS Code
- `code/0-setup.R` loads packages and sets options (activate renv when initialized)
- `code/analysis/export-paper-numbers.R` exports key numbers for inline citation

## Last Session

- Date: 2026-02-27
- Project scaffolded
