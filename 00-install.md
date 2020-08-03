---
layout: page
title: Instalación
permalink: /TextMiningGUI/install/
---

Para la instalación podemos utilizar el repositorio en GitHub donde están los códigos en
desarrollo. Esto requiere de disponer previamente el paquete “devtools” y las dependencias
principales del programa.

Requisitos

{% highlight r %}
install.packages(c("tcltk", "tkrplot", "dplyr", "tidyr", "tidytext", "tibble", "tm", "SnowballC", "slam", "syuzhet", "ggplot2", "ggwordcloud", "RcolorBrewer"))
{% endhighlight %}

Paquetes adicionales

{% highlight r %}
install.packages(c("readxl", "jsonlite", "parallel", "ggrepel", "ggpubr", "igraph", "ggraph", "ape", "topicmodels", "ca", "corrr"))
{% endhighlight %}

Instalación desde GitHub

{% highlight r %}
install.packages("devtools")

devtools::install_github("c0reyes/TextMiningGUI")
{% endhighlight %}

Instaladas las dependencias y el paquete de TextMiningGUI, se procede a cargar la librería y ejecutar el programa:

{% highlight r %}
library(TextMiningGUI)

TextMiningGUI()
{% endhighlight %}