#' Download and unzip POF microdata
#'
#' Download and unzip POF microdata
#' @param destination_path The folder in which the microdata files are to be stored (if folder does not exist, it will be created)
#' @return The microdata files in the designated path
#' @examples baixar_pof("./microdata");
#' @export


baixar_pof <- function(destination_path){

  url <- "https://ftp.ibge.gov.br/Orcamentos_Familiares/Pesquisa_de_Orcamentos_Familiares_2017_2018/Microdados/Dados_20210304.zip"

  tempFile <- tempfile()

  download.file(url,tempFile,quiet=TRUE,mode="wb")

  unzip(file.path(tempFile), exdir = destination_path)

}

