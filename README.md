# TextMiningGUI

[![GitHub stars](https://img.shields.io/github/stars/c0reyes/TextMiningGUI?color=red&style=flat-square)](https://github.com/c0reyes/TextMiningGUI)
[![GitHub last commit](https://img.shields.io/github/last-commit/c0reyes/TextMiningGUI?style=flat-square)](https://github.com/c0reyes/TextMiningGUI)
[![GitHub repo size](https://img.shields.io/github/repo-size/c0reyes/TextMiningGUI?style=flat-square)](https://github.com/c0reyes/TextMiningGUI)
[![GitHub pull requests](https://img.shields.io/github/issues-pr/c0reyes/TextMiningGUI?style=flat-square)](https://github.com/c0reyes/TextMiningGUI)
[![GitHub issues](https://img.shields.io/github/issues/c0reyes/TextMiningGUI?style=flat-square)](https://github.com/c0reyes/TextMiningGUI)

Graphic interface for text analysis, implement a few methods such as biplot, correspondece analysis, co-occurrence, clustering, topic models, correlations and sentiments.

https://c0reyes.github.io/TextMiningGUI/

## Prerequisites

```
install.packages(c("tcltk", "dplyr", "tidyr", "tidytext", "tibble", "tm", "SnowballC", "slam", "syuzhet", "ggplot2", "ggwordcloud", "RColorBrewer"))
```

Optionals:

```
install.packages(c("tkrplot", "readxl", "jsonlite", "parallel", "ggrepel", "ggpubr", "igraph", "ggraph", "ape", "topicmodels", "ca", "corrr"))
```

## Install

**CRAN**

```
install.packages("TextMiningGUI")
```

**GitHub**

```
install.packages("devtools")

devtools::install_github("c0reyes/TextMiningGUI")
```

## Run

```
library(TextMiningGUI)

TextMiningGUI()	
```

### File Menu

- Import files: csv, excel, json or RData. 
- Save project.
- Set work directory.

### Data Menu

- Converter Columns
- Transform
- Slice
- View Data
- View Lexical Table
- View Clean Data

### Analysis Menu

- Statistics
- Most common words
- Word Group
- Word Cloud
- Co-ocurrence
- Cluster
- Correlation
- Correlation Between Two Groups
- AFC
- HJ-Biplot
- Emotions & Sentiments
- Topic Models
