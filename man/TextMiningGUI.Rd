\name{TextMiningGUI}
\alias{TextMiningGUI}
\title{TextMiningGUI}
\usage{
TextMiningGUI()
}
\description{
The text analysis, convert documents to corpus and lexical table. The base core  of the analysis its the HJ-Biplot and the caracteritation value.

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
\dontrun{
library(TextMiningGUI)
TextMiningGUI()
}
}
