---
layout: page
title: Instalación
permalink: /TextMiningGUI/install/
---

Para la instalación podemos utilizar el repositorio en GitHub donde están los códigos en
desarrollo o desde el repositorio CRAN de R. Para instalar desde GitHub requiere disponer previamente del paquete “devtools” y las dependencias principales del programa.

Sino dispone de R, con esta guia puede hacer la instalación:

<https://knowledgesociety.usal.es/sites/default/files/Guia_instalacion_R.pdf>

Requisitos

{% highlight r %}
install.packages(c("tcltk", "tkrplot", "dplyr", "tidyr", "tidytext", "tibble", "tm", "SnowballC", "slam", "syuzhet", "ggplot2", "ggwordcloud", "RcolorBrewer"))
{% endhighlight %}

Paquetes adicionales

{% highlight r %}
install.packages(c("readxl", "jsonlite", "parallel", "ggrepel", "ggpubr", "igraph", "ggraph", "ape", "topicmodels", "ca", "corrr"))
{% endhighlight %}

Instalación desde CRAN

{% highlight r %}
install.packages("TextMiningGUI")
{% endhighlight %}

Instalación desde GitHub

{% highlight r %}
install.packages("devtools")

devtools::install_github("c0reyes/TextMiningGUI")
{% endhighlight %}

Instaladas las dependencias y el paquete TextMiningGUI, se procede a cargar la librería y ejecutar el programa:

{% highlight r %}
library(TextMiningGUI)

TextMiningGUI()
{% endhighlight %}