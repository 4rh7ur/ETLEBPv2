% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/executa_tratamento_incremental.R
\name{executa_tratamento_incremental}
\alias{executa_tratamento_incremental}
\title{executa o tratamento que gera um dataset para a carga incremental do sqlite}
\usage{
executa_tratamento_incremental(df, sqlite)
}
\arguments{
\item{df}{dataset intermediario}

\item{sqlite}{diretório do sqlite}
}
\value{

}
\description{
executa o tratamento que gera um dataset para a carga incremental do sqlite
}
\examples{
executa_tratamento_incremental(df_intermediario_aneel, diretorio_sqlite)
executa_tratamento_incremental(df_intermediario_anp, diretorio_sqlite)
executa_tratamento_incremental(df_intermediario_bndes, diretorio_sqlite)
executa_tratamento_incremental(df_intermediario_cnen, diretorio_sqlite)
}
