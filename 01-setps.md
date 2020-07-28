---
layout: page
title: Uso
permalink: /TextMiningGUI/steps/
---

    library(TextMiningGUI)
    TextMiningGUI()

Para utilizar el programa y efectuar los análisis, se realizan los siguientes pasos:

- Importar archivos, las diferentes opciones están en el menú de Archivos.
- Minería de textos, en otras palabras crear corpus y tablas léxicas. Menú Datos.
- Realizar los análisis, menú Análisis.

<center><img src="/TextMiningGUI/assets/img/gui.png" width="75%"></center>

# Archivos

Bajo la categoría archivo tenemos las opciones para importar los datos. Textos que pueden
estar separados por coma, espacio, o algún otro carácter especificado por el usuario. Archivos de
excel, RData y JSON. El paquete contiene dos conjuntos de datos como ejemplos y pueden ser
cargados desde este menú. Es posible asignar un directorio de trabajo, donde se guarda las salidas
del programa. Guardar y abrir proyectos está disponible en esta categoría, y por el momento solo
reportes en formato PDF también está disponible.

# Datos

La categoría datos es la más importante, ya que se encuentran las opciones para preparar los
datos. Convertir columnas, ayuda a convertir una variable a otro tipo: factor, fecha o rango de
números para tenerlos como un factor. Transformación, donde se ajustan las opciones para la minera
de datos. Parte, realizada una selección de grupos en específicos para la minería. Ver datos, tabla
léxica y datos limpios, como su nombre los indican están para visualizaciones los conjuntos de datos
correspondientes. Y por último consola, permite visualizar las salidas de los procesos mientras se van
ejecutando.

- Converter Columns
    - **Variable:** selección variable/columna a convertir.
    - **Tipo:** tipo de variable al que se va a convertir la columna.
    - **Valor o forma:** si es seleccionado factor o rango, es separado por coma los nombres que
tendrán los factores. En caso de seleccionar fecha como el tipo, se escribe el formato de como está la fecha para que pueda ser transformadas. Ejemplo: 2020/01/01 son los valores que
tenemos en la columna, el formato seria %Y/%m/%d.
    - **Punto de corte:** en caso de ser elegido el tipo rango para la conversión, es separado por comas los valores en los se van a fragmentar la columna.
    - **Nuevo nombre:** nombra una nueva variable sino se desea sobre escribir la que se quiere
convertir.
- Transform
    - **Grupo:** columna por el cual se clasifican u organizan los documentos.
    - **Texto:** documentos a procesar.
    - **Tiempo o Fecha:** columna con serie de fechas, utilizada en conteo de palabras y modelos de tema.
    - **Lenguaje:** utilizado para tener el diccionario de palabras que no son utilizadas “palabras vacías”. También es utilizado para seleccionar el diccionario léxico.
    - **Palabras vacías:** de ser marcado, las palabras vacías son eliminadas del corpus.
    - **Otras palabras vacías:** palabras vacías adicionales, escritas con separación de coma o cuando se da doble clic, se abre la posibilidad de cargar un archivo con el listado de palabras.
    - **Derivado:** marcado, indica que las palabras sean reducidas a su forma base, esto para igualar palabras que pueden estar conjugadas o escritas de varias maneras.
    - **Complementación:** completar las palabras desde la forma base. Esta opción en combinación con la opción derivado es lo conocemos como lemantización. No es recomendable con grandes cantidades de texto.
    - **Bigrams:** si es marcada esta opción, se realiza la búsqueda de bigrams. Por defecto no está marcada.
    - **Normalización:** se elige tipo de normalizado, por defecto “chara-value”. En caso de no tener  una columna o variable de categorías, no se utiliza ninguna normalización.
        - chara-value: valor de caracterización.
        - tf-idf: frecuencia inversa de los términos.
        - media: el peso será la frecuencia entre la media de la columna.
        - none: sin normalización.
    - **Eliminar términos dispersos o escasos:** la última opción elimina esos términos poco frecuentes. Este rango va de 0 a 1, configurada en el GUI de 0.5 a 1 exclusivamente. Permanecerán aquellos términos que tengan a lo sumo el valor de dispersión o menos.
- Slice
- View Data
- View Lexical Table
- View Clean Data

# Análisis

En categoría análisis, donde se encuentran todos los procedimientos disponibles que
se pueden realizar con los documentos y tabla léxica. Este puede variar acorde a las librerías
disponibles, las opciones tomadas y/o los tipos de textos cargados.

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