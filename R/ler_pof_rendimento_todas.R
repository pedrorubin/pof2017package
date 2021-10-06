#' Load POF microdata - all income information
#'
#' Load POF microdata - income information only
#' @param microdata_folder The path to the FOLDER that contains the microdata txt files
#' @return All income information available (as a dataframe)
#' @examples ler_pof_rendimento_todas ("./data_folder");
#' @export

ler_pof_rendimento_todas <- function(microdata_folder){

  lista_pof_rendimento <- tibble(num_pof = c(as.character(glue("{microdata_folder}/ALUGUEL_ESTIMADO.txt")),
                                          as.character(glue("{microdata_folder}/CADERNETA_COLETIVA.txt")),
                                          as.character(glue("{microdata_folder}/DESPESA_COLETIVA.txt")),
                                          as.character(glue("{microdata_folder}/DESPESA_INDIVIDUAL.txt")),
                                          as.character(glue("{microdata_folder}/OUTROS_RENDIMENTOS.txt")),
                                          as.character(glue("{microdata_folder}/RENDIMENTO_TRABALHO.txt"))))

  pof_desp <- map_dfr(lista_pof_rendimento$num_pof,
                      ler_pof_rendimento)
}
