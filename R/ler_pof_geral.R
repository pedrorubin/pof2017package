#' Load POF microdata
#'
#' Load POF microdata
#' @param microdata_file The specific microdata file to be loaded
#' @return The file-specific microdata as a dataframe (all cols to character)
#' @examples ler_pof_geral("./data/microdatafile.txt");
#' @export

ler_pof_geral <- function(microdata_file) {

  if(str_detect(microdata_file, regex("aluguel_estimado", ignore_case = TRUE))){
    data(leitor_aluguel_estimado)
  }

  leitor_aluguel_estimado$variavel <- as.character(leitor_aluguel_estimado$variavel)
  colpos <- fwf_widths(leitor_aluguel_estimado$tamanho,
                       col_names = leitor_aluguel_estimado$variavel)
  pof_mod <- read_fwf(file = as.character(microdata_file),
                      col_positions = colpos,
                      col_types = cols(.default = col_character()))

}
