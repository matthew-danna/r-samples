# MSF Network Shiny App

3D interactive network map for Marvel Strike Force characters, built with Shiny + Plotly.

## Run

From `/Users/matthewdanna/Documents/GitHub/r-samples/msf` in R:

```r
shiny::runApp("msf-network-shiny")
```

## Required R Packages

```r
install.packages(c("shiny", "bslib", "dplyr", "readr", "stringr", "plotly", "igraph"))
```

## Data Inputs

- `../characters.csv`
- `data/team_memberships.csv`
- `data/synergy_pairs.csv`

## Controls

- Search and focus character
- Toggle Team / Synergy / Inferred links
- Adjust link opacity and node size
- Click nodes to inspect details and neighbors
