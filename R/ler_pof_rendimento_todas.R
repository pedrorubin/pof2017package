#' Carregar microdados da POF - todas as informações de rendimento
#'
#' Carregar microdados da POF - todos os registros com dados de rendimento
#' Aluguel estimado, caderneta coletiva, despesa coletiva, despesa individual, outros rendimentos e rendimento trabalho
#' OBS: É preciso que o nome dos arquivos .txt esteja igual a como foi baixado!!
#'
#' @param pasta_microdados O caminho para a pasta que contém os arquivos .txt de microdados
#' @return Um dataframe com as informações de rendimento
#' @examples ler_pof_rendimento_todas("./pasta_microdados");
#' @seealso ler_pof_rendimento, baixar_pof
#' @export

ler_pof_rendimento_todas <- function(pasta_microdados){

  lista_pof_rendimento <- tibble(num_pof = c(as.character(glue("{pasta_microdados}/ALUGUEL_ESTIMADO.txt")),
                                          as.character(glue("{pasta_microdados}/CADERNETA_COLETIVA.txt")),
                                          as.character(glue("{pasta_microdados}/DESPESA_COLETIVA.txt")),
                                          as.character(glue("{pasta_microdados}/DESPESA_INDIVIDUAL.txt")),
                                          as.character(glue("{pasta_microdados}/OUTROS_RENDIMENTOS.txt")),
                                          as.character(glue("{pasta_microdados}/RENDIMENTO_TRABALHO.txt"))))

  pof_desp <- map_dfr(lista_pof_rendimento$num_pof,
                      ler_pof_rendimento)
}
