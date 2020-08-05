\name{TextMiningGUI}
\encoding{UTF-8}
\alias{TextMiningGUI}
\title{TextMiningGUI}
\usage{
TextMiningGUI(seed = 0)
}
\arguments{
\item{seed}{the seed of internal function.}
}
\description{
Graphic interface for text analysis, implement a few methods such as biplots, correspondence analysis, co-occurrence, clustering, topic models, correlations and sentiments.

File Menu:
\itemize{
\item Can import files: csv, excel, json or RData. 
\item Save project. 
\item Set work directory.
}

Data Menu:
\itemize{
\item Converter Columns
\item Transform
\item Slice
\item View Data
\item View Lexical Table
\item View Clean Data
}

Analysis Menu:
\itemize{
\item Statistics
\item Most common words
\item Word Group
\item Word Cloud
\item Co-ocurrence
\item Cluster
\item Correlation
\item Correlation Between Two Groups
\item AFC
\item HJ-Biplot
\item Emotions & Sentiments
\item Topic Models
}
}
\examples{
library(TextMiningGUI)
if(TextMiningGUI()){}
}
\references{
\itemize{
\item Becue, M. B. (1992) El análisis estadístico de datos textuales. La lectura según los escolares de la enseñanza primaria.
\item Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988). The New S Language. Wadsworth & Brooks/Cole. (S version.)
\item Benzècri, J. P. (1973). L'Analyse des Donées: L'Analyse des correspondences. Paris: Dunod.
\item Blei, D. B., Ng, A. Y. N., Jordan, M. I. J. (2003) Latent Dirichlet Allocation.
\item Caballero, D. C. (2014) Grupos de Discusión y HJ-Biplot: Una Nueva Forma de Análisis Textual.
\item Caballero, D.C. (2011). El HJ-Biplot como Herramienta en el Análisis de Grupos de Discusión. Salamanca: Universidad de Salamanca.
\item Collins, M. J. C. (1996) A new Statistical Parser Based on Bigram Lexical Dependencies.
\item Diaz-Faes, A. D. (2013) HJ-Biplot como herramienta de inspección de matrices de datos bibliométricos.
\item Feinerer, I., Hornik, K. (2019). tm: Text Mining Package. R package version 0.7-7. https://CRAN.R-project.org/package=tm
\item Feinerer, I., Hornik, K., Meyer, D. (2008). Text Mining Infrastructure in R. Journal of Statistical Software 25(5): 1-54. http://www.jstatsoft.org/v25/i05/
\item Forgy, E. W. (1965). Cluster analysis of multivariate data: efficiency vs interpretability of classifications. Biometrics, 21, 768–769.
\item Gabriel, K. R. (1971). The biplot graphic display of matrices with application to principal component analysis. Biometrika 58, 3 , 453-467
\item Galindo, M. P. (1985). Contribuciones a la Representación Simultánea de Datos Multidimensionales. Tesis Doctoral. Salamanca: Universidad de Salamanca.
\item Galindo, P. (1986). Una alternativa de representación simultanea: HJ-Biplot. Qüestiió ,13-23.
\item Hartigan, J. A. and Wong, M. A. (1979). Algorithm AS 136: A K-means clustering algorithm. Applied Statistics, 28, 100–108. doi: 10.2307/2346830.
\item Jockers, ML. (2015). Syuzhet: Extract Sentiment and Plot Arcs from Text. https://github.com/mjockers/syuzhet
\item Kuhn, M., Jackson, S., Cimentada, J. (2020). corrr: Correlations in R. R package version 0.4.2. https://CRAN.R-project.org/package=corrr
\item Lloyd, S. P. (1957, 1982). Least squares quantization in PCM. Technical Note, Bell Laboratories. Published in 1982 in IEEE Transactions on Information Theory, 28, 128–137.
\item MacQueen, J. (1967). Some methods for classification and analysis of multivariate observations. In Proceedings of the Fifth Berkeley Symposium on Mathematical Statistics and Probability, eds L. M. Le Cam & J. Neyman, 1, pp. 281–297. Berkeley, CA: University of California Press.
\item Müller, K., Wickham, H. (2020). tibble: Simple Data Frames. R package version 3.0.1. https://CRAN.R-project.org/package=tibble
\item Nenadic, O., Greenacre, M. (2007) Correspondence Analysis in R, with two- and three-dimensional graphics: The ca package. Journal of Statistical Software 20(3):1-13.
\item Silge, J., Robinson, D. (2016). “tidytext: Text Mining and Analysis Using Tidy Data Principles in R. https://doi.org/10.21105/joss.00037
\item Osuna, Z. (2006). Contribuciones al Análisis de Datos Textuales.
\item Osuna, Z. O. (2004) Análisis estadístico de datos textuales. Aplicación al estudio de las declaraciones del Libertador Simón Bolívar
\item Robertson, S. R. (2004) Understanding Inverse Document Frequency: On theoretical arguments for IDF.
\item Vicente-Villardón, J. L. (2017). MultBiplotR: Multivariate Analysis using Biplots. R package version 0.1.0. http://biplot.dep.usal.es/multbiplot/multbiplot-in-r/
\item Ward, J. H., Jr. (1963), "Hierarchical Grouping to Optimize an Objective Function", Journal of the American Statistical Association, 58, 236–244.
\item Wickham, H. (2016) ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York.
\item Wickham, H., François, R., Henry, L., Müller, K. (2020). dplyr: A Grammar of Data Manipulation. R package version 0.8.5. https://CRAN.R-project.org/package=dplyr
\item Wickham, H., Lionel Henry, L. (2020). tidyr: Tidy Messy Data. R package version 1.0.2. https://CRAN.R-project.org/package=tidyr
\item NRC Word-Emotion Association Lexicon. (2010). http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
\item ProgGUIinR: support package for «Programming Graphical User Interfaces in R». (2014). https://rdrr.io/cran/ProgGUIinR/
}
}