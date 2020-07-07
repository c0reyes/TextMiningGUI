# TextMiningGUI

It's a GUI create in R with tcltk. With this is posible the analysis text, convert documents to corpus and lexical table. The base core  of the analysis its the HJ-Biplot and the caracteritation value.

## Prerequisites

```
install.packages(c("tcltk", "tkrplot", "dplyr", "tidyr", "tidytext", "tibble", "tm", "SnowballC", "slam", "syuzhet", "ggplot2", "ggwordcloud", "RColorBrewer"))
```

Optionals:

```
install.packages(c("readxl", "jsonlite", "parallel", "ggrepel", "ggpubr", "igraph", "ggraph", "ape", "topicmodels", "ca", "corrr"))
```

## Install

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
