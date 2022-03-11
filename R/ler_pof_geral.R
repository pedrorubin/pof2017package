#' Carregar microdados da POF
#'
#' Carregar microdados da POF - um registro
#' OBS: É preciso que o nome dos arquivos .txt esteja igual a como foi baixado!!
#'
#' @param arquivo_microdados O caminho até o arquivo txt do registro a ser carregado
#' @return Um dataframe com as informações do registro
#' @examples ler_pof_despesa("./microdados/MORADOR.txt");
#' @seealso ler_pof_despesa, ler_pof_rendimento, baixar_pof
#' @export

ler_pof_geral <- function(arquivo_microdados) {

  if(str_detect(arquivo_microdados, regex("aluguel_estimado", ignore_case = TRUE))){
    leitor <- leitor_aluguel_estimado
  }
  else if(str_detect(arquivo_microdados, regex("caderneta_coletiva", ignore_case = TRUE))){
    leitor <- leitor_caderneta_coletiva
  }
  else if(str_detect(arquivo_microdados, regex("caracteristicas_dieta", ignore_case = TRUE))){
    leitor <- leitor_caracteristicas_dieta
  }
  else if(str_detect(arquivo_microdados, regex("condicoes_vida", ignore_case = TRUE))){
    leitor <- leitor_condicoes_vida
  }
  else if(str_detect(arquivo_microdados, regex("consumo_alimentar", ignore_case = TRUE))){
    leitor <- leitor_consumo_alimentar
  }
  else if(str_detect(arquivo_microdados, regex("despesa_coletiva", ignore_case = TRUE))){
    leitor <- leitor_despesa_coletiva
  }
  else if(str_detect(arquivo_microdados, regex("despesa_individual", ignore_case = TRUE))){
    leitor <- leitor_despesa_individual
  }
  else if(str_detect(arquivo_microdados, regex("domicilio", ignore_case = TRUE))){
    leitor <- leitor_domicilio
  }
  else if(str_detect(arquivo_microdados, regex("inventario", ignore_case = TRUE))){
    leitor <- leitor_inventario
  }
  else if(str_detect(arquivo_microdados, regex("morador", ignore_case = TRUE))){
    leitor <- leitor_morador
  }
  else if(str_detect(arquivo_microdados, regex("outros_rendimentos", ignore_case = TRUE))){
    leitor <- leitor_outros_rendimentos
  }
  else if(str_detect(arquivo_microdados, regex("rendimento_trabalho", ignore_case = TRUE))){
    leitor <- leitor_rendimento_trabalho
  }
  else if(str_detect(arquivo_microdados, regex("restricao_produtos_servicos_saude", ignore_case = TRUE))){
    leitor <- leitor_restricao_produtos_servicos_saude
  }
  else if(str_detect(arquivo_microdados, regex("servico_nao_monetario_pof2", ignore_case = TRUE))){
    leitor <- leitor_servico_nao_monetario_pof2
  }
  else if(str_detect(arquivo_microdados, regex("servico_nao_monetario_pof4", ignore_case = TRUE))){
    leitor <- leitor_servico_nao_monetario_pof4
  }

  leitor$variavel <- as.character(leitor$variavel)
  colpos <- fwf_widths(leitor$tamanho,
                       col_names = leitor$variavel)
  pof_mod <- read_fwf(file = as.character(arquivo_microdados),
                      col_positions = colpos,
                      col_types = cols(.default = col_character()))

}
