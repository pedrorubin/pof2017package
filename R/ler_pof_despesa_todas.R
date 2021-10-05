#' Load POF microdata - all expenditure information
#'
#' Load POF microdata - expenditure information only
#' @param microdata_folder The path to the FOLDER that contains the microdata txt files
#' @return All expenditure information available
#' @examples ler_pof_despesa_todas ("./data_folder");
#' @export

ler_pof_despesa_todas <- function(microdata_folder){

  lista_pof_despesa <- tibble(num_pof = c(as.character(glue("{microdata_folder}/ALUGUEL_ESTIMADO.txt")),
                                          as.character(glue("{microdata_folder}/CADERNETA_COLETIVA.txt")),
                                          as.character(glue("{microdata_folder}/DESPESA_COLETIVA.txt")),
                                          as.character(glue("{microdata_folder}/DESPESA_INDIVIDUAL.txt")),
                                          as.character(glue("{microdata_folder}/OUTROS_RENDIMENTOS.txt")),
                                          as.character(glue("{microdata_folder}/RENDIMENTO_TRABALHO.txt"))))

  pof_desp <- map_dfr(lista_pof_despesa$num_pof,
                      ler_pof_despesa)
}
