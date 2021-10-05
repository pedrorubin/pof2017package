#' Load POF microdata
#'
#' Load POF microdata
#' @param microdata_file The specific microdata file to be loaded
#' @return The file-specific microdata as a dataframe (all cols to character)
#' @examples ler_pof_geral("./data/microdatafile.txt");
#' @export

ler_pof_geral <- function(microdata_file) {

  if(str_detect(microdata_file, regex("aluguel_estimado", ignore_case = TRUE))){
    leitor <- leitor_aluguel_estimado
  }
  else if(str_detect(microdata_file, regex("caderneta_coletiva", ignore_case = TRUE))){
    leitor <- leitor_caderneta_coletiva
  }
  else if(str_detect(microdata_file, regex("caracteristicas_dieta", ignore_case = TRUE))){
    leitor <- leitor_caracteristicas_dieta
  }
  else if(str_detect(microdata_file, regex("condicoes_vida", ignore_case = TRUE))){
    leitor <- leitor_condicoes_vida
  }
  else if(str_detect(microdata_file, regex("consumo_alimentar", ignore_case = TRUE))){
    leitor <- leitor_consumo_alimentar
  }
  else if(str_detect(microdata_file, regex("despesa_coletiva", ignore_case = TRUE))){
    leitor <- leitor_despesa_coletiva
  }
  else if(str_detect(microdata_file, regex("despesa_individual", ignore_case = TRUE))){
    leitor <- leitor_despesa_individual
  }
  else if(str_detect(microdata_file, regex("domicilio", ignore_case = TRUE))){
    leitor <- leitor_domicilio
  }
  else if(str_detect(microdata_file, regex("inventario", ignore_case = TRUE))){
    leitor <- leitor_inventario
  }
  else if(str_detect(microdata_file, regex("morador", ignore_case = TRUE))){
    leitor <- leitor_morador
  }
  else if(str_detect(microdata_file, regex("outros_rendimentos", ignore_case = TRUE))){
    leitor <- leitor_outros_rendimentos
  }
  else if(str_detect(microdata_file, regex("rendimento_trabalho", ignore_case = TRUE))){
    leitor <- leitor_rendimento_trabalho
  }
  else if(str_detect(microdata_file, regex("restricao_produtos_servicos_saude", ignore_case = TRUE))){
    leitor <- leitor_restricao_produtos_servicos_saude
  }
  else if(str_detect(microdata_file, regex("servico_nao_monetario_pof2", ignore_case = TRUE))){
    leitor <- leitor_servico_nao_monetario_pof2
  }
  else if(str_detect(microdata_file, regex("servico_nao_monetario_pof4", ignore_case = TRUE))){
    leitor <- leitor_servico_nao_monetario_pof4
  }

  leitor$variavel <- as.character(leitor$variavel)
  colpos <- fwf_widths(leitor$tamanho,
                       col_names = leitor$variavel)
  pof_mod <- read_fwf(file = as.character(microdata_file),
                      col_positions = colpos,
                      col_types = cols(.default = col_character()))

}
