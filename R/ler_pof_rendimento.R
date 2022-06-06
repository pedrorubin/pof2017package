#' Carregar microdados da POF - todas as informações de rendimento
#'
#' Carregar microdados da POF - todos os registros com dados de rendimento
#' Aluguel estimado, caderneta coletiva, despesa coletiva, individual, outros rendimentos e rendimento trabalho
#' OBS: É preciso que o nome dos arquivos .txt esteja igual a como foi baixado!!
#'
#' @param arquivo_microdados O caminho até o arquivo txt do registro de rendimento a ser carregado
#' @return Um dataframe com as informações de rendimento do registro
#' @examples ler_pof_rendimento("./microdados/ALUGUEL_ESTIMADO.txt");
#' @seealso ler_pof_rendimento_todas, baixar_pof
#' @export


ler_pof_rendimento <- function(arquivo_microdados){

  if(!(str_detect(arquivo_microdados,
                  regex("aluguel_estimado|caderneta_coletiva|despesa_coletiva|despesa_individual|outros_rendimentos|rendimento_trabalho",
                        ignore_case = TRUE)))){

    cat("Os registros de POF 2017-2018 aceitos para o rendimento sao: \n
          ALUGUEL_ESTIMADO, CADERNETA_COLETIVA, DESPESA_COLETIVA, \n
          DESPESA_INDIVIDUAL, OUTROS_RENDIMENTOS, RENDIMENTO_TRABALHO \n")
    stop()
  }


  if(str_detect(arquivo_microdados, regex("rendimento_trabalho", ignore_case = TRUE))){

    ler_pof_geral(arquivo_microdados) %>%
      mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
             NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
             COD_INFORMANTE = str_pad(COD_INFORMANTE, 2, "left", "0"),
             ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             ID_pes = str_c(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE),
             across(
               .cols = c(V9001, V8500_DEFLA,
                              V9011, FATOR_ANUALIZACAO, PESO_FINAL),
                    .fns = as.numeric)) %>%
      filter(is.na(V8500_DEFLA) == FALSE) %>%
      mutate(valor_mensal = V8500_DEFLA*V9011*FATOR_ANUALIZACAO/12,
             Codigo = trunc(as.numeric(V9001)/100),
             pof = "RENDIMENTO_TRABALHO",
             V9002 = NA) %>%
      select(ID_uc, ID_pes, PESO_FINAL, Codigo, V9002, valor_mensal, pof)
  }
  else if(str_detect(arquivo_microdados, regex("outros_rendimentos", ignore_case = TRUE))){

    ler_pof_geral(arquivo_microdados) %>%
      mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
             NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
             COD_INFORMANTE = str_pad(COD_INFORMANTE, 2, "left", "0"),
             ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             ID_pes = str_c(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE),
             across(
               .cols = c(V9001, V8500_DEFLA,
                              V9011, FATOR_ANUALIZACAO, PESO_FINAL),
               .fns = as.numeric)) %>%
      mutate(valor_mensal = ifelse( QUADRO==54,
                                    (V8500_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
                                    (V8500_DEFLA*FATOR_ANUALIZACAO)/12 ),
             Codigo = trunc(as.numeric(V9001)/100),
             pof = "OUTROS_RENDIMENTOS",
             V9002 = NA) %>%
      select(ID_uc, ID_pes, PESO_FINAL, Codigo, V9002, valor_mensal, pof)


  }
  else if(str_detect(arquivo_microdados, regex("despesa_coletiva", ignore_case = TRUE))){

    ler_pof_geral(arquivo_microdados) %>%
      mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
             NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
             ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             ID_pes = NA,
             across(.cols = c(V9001, V8000_DEFLA,
                              V9011, FATOR_ANUALIZACAO, PESO_FINAL),
                    .fns = as.numeric),
             valor_mensal = ifelse( QUADRO==10|QUADRO==19,
                                    (V8000_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
                                    (V8000_DEFLA*FATOR_ANUALIZACAO)/12),
             Codigo = trunc(V9001/100),
             pof = "DESPESA_COLETIVA") %>%
      select(ID_uc, ID_pes, PESO_FINAL, Codigo, V9002, valor_mensal, pof)
  }
  else if(str_detect(arquivo_microdados, regex("caderneta_coletiva", ignore_case = TRUE))){

    ler_pof_geral(arquivo_microdados) %>%
      mutate(ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
             NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
             ID_pes = NA,
             across(.cols = c(V9001, V8000_DEFLA,
                              FATOR_ANUALIZACAO, PESO_FINAL),
                    .fns = as.numeric)) %>%
      filter(V9002 >= 7) %>%
      mutate(valor_mensal = (V8000_DEFLA*FATOR_ANUALIZACAO)/12,
             Codigo = trunc(V9001/100),
             pof = "CADERNETA_COLETIVA") %>%
      select(ID_uc, ID_pes, PESO_FINAL, Codigo, V9002, valor_mensal, pof)

  }
  else if(str_detect(arquivo_microdados, regex("despesa_individual", ignore_case = TRUE))){

    ler_pof_geral(arquivo_microdados) %>%
      mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
             NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
             COD_INFORMANTE = str_pad(COD_INFORMANTE, 2, "left", "0"),
             ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             ID_pes = str_c(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE),
             across(.cols = c(V9001, V8000_DEFLA,
                              V9011, FATOR_ANUALIZACAO, PESO_FINAL),
                    .fns = as.numeric)) %>%
      filter(V9002 >= 7) %>%
      mutate(valor_mensal = ifelse( QUADRO==44|QUADRO==47|QUADRO==48|QUADRO==49|QUADRO==50 ,
                                    V8000_DEFLA*V9011*FATOR_ANUALIZACAO/12 ,
                                    V8000_DEFLA*FATOR_ANUALIZACAO/12),
             Codigo = trunc(V9001/100),
             pof = "DESPESA_INDIVIDUAL") %>%
      select(ID_uc, ID_pes, PESO_FINAL, Codigo, V9002, valor_mensal, pof)
  }
  else if(str_detect(arquivo_microdados, regex("aluguel_estimado", ignore_case = TRUE))){

    ler_pof_geral(arquivo_microdados) %>%
      mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
             NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
             ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             ID_pes = NA,
             across(.cols = c(V9001, V8000_DEFLA,
                              V9011, FATOR_ANUALIZACAO, PESO_FINAL),
                    .fns = as.numeric),
             valor_mensal = V8000_DEFLA*V9011*FATOR_ANUALIZACAO/12,
             Codigo = trunc(V9001/100),
             pof = "ALUGUEL_ESTIMADO") %>%
      select(ID_uc, ID_pes, PESO_FINAL, Codigo, V9002, valor_mensal, pof)
  }
  else{
    cat("Os registros POF 2017-2018 validos de rendimento sao: \n
    ALUGUEL_ESTIMADO, CADERNETA_COLETIVA, DESPESA_COLETIVA, \n
    DESPESA_INDIVIDUAL, OUTROS_RENDIMENTOS, RENDIMENTO_TRABALHO")
    stop()
  }
}
