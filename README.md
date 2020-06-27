# TextMiningGUI

It's a GUI create in R with tcltk. With this is posible the analysis text, convert documents to corpus and then lexical table. The base core  of the analysis its the HJ-Biplot and the caracteritation value, but have another normalize methods and more options.

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

install_github("c0reyes/TextMiningGUI")
```

## Run

```
library(TextMiningGUI)

TextMiningGUI()	
```