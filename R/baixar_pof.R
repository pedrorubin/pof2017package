#' Baixar e descompactar microdados da POF
#'
#' Baixar e descompactar microdados da POF
#' @param pasta_destino Pasta na qual os microdados serão guardados (se não existir, será criada automaticamente)
#' @return Os microdados na pasta designada
#' @examples baixar_pof("./microdados");
#' @export

baixar_pof <- function(pasta_destino){

  url <- "https://ftp.ibge.gov.br/Orcamentos_Familiares/Pesquisa_de_Orcamentos_Familiares_2017_2018/Microdados/Dados_20210304.zip"

  tempFile <- tempfile()

  download.file(url,tempFile,quiet=TRUE,mode="wb")

  unzip(file.path(tempFile), exdir = pasta_destino)

}

