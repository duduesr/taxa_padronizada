% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/padroniza_taxa.R
\name{padroniza_taxa}
\alias{padroniza_taxa}
\title{Funcao de Padronizacao de Taxas}
\usage{
padroniza_taxa(
  x,
  p,
  var.populacao = "populacao",
  var.evento = "obitos",
  var.area = c("cod_uf", "ano"),
  var.categoria = c("sexo", "gi"),
  escala = 1000,
  nome = "Taxa"
)
}
\arguments{
\item{dados}{banco de dados de resultados}
}
\value{
Lista com informações sobre erros
}
\description{
XXXXXXXXXXXXXXXXXXXXXXXXXX
}
\examples{
data(brasil2000)
}
