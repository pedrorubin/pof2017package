% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ler_pof_geral.R
\name{ler_pof_geral}
\alias{ler_pof_geral}
\title{Carregar microdados da POF}
\usage{
ler_pof_geral(arquivo_microdados)
}
\arguments{
\item{arquivo_microdados}{O caminho até o arquivo txt do registro a ser carregado}
}
\value{
Um dataframe com as informações do registro
}
\description{
Carregar microdados da POF - um registro
OBS: É preciso que o nome dos arquivos .txt esteja igual a como foi baixado!!
}
\examples{
ler_pof_despesa("./microdados/MORADOR.txt");
}
\seealso{
ler_pof_despesa, ler_pof_rendimento, baixar_pof
}
