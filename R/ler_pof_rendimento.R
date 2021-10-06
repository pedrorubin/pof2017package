#' Load POF microdata - income information only
#'
#' Load POF microdata - income information only
#' @param microdata_file The specific microdata file to be loaded
#' @return The file-specific microdata as a dataframe (all cols to character)
#' @examples ler_pof_rendimento("./data/microdatafile.txt");
#' @export


ler_pof_rendimento <- function(microdata_file){

  if(!(str_detect(microdata_file,
                  regex("aluguel_estimado|caderneta_coletiva|despesa_coletiva|despesa_individual|outros_rendimentos|rendimento_trabalho",
                        ignore_case = TRUE)))){

    cat("Os registros de POF 2017-2018 aceitos para a despesa sao: \n
          ALUGUEL_ESTIMADO, CADERNETA_COLETIVA, DESPESA_COLETIVA, \n
          DESPESA_INDIVIDUAL, OUTROS_RENDIMENTOS, RENDIMENTO_TRABALHO \n")
    stop()
  }


  if(str_detect(microdata_file, regex("rendimento_trabalho", ignore_case = TRUE))){

    ler_pof_geral(microdata_file) %>%
      mutate(ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             across(.cols = c(V9001, V8500_DEFLA,
                              V9011, FATOR_ANUALIZACAO, PESO_FINAL, ID_uc),
                    .fns = as.numeric)) %>%
      filter(is.na(V8500_DEFLA) == FALSE) %>%
      mutate(valor_mensal = V8500_DEFLA*V9011*FATOR_ANUALIZACAO/12,
             Codigo = trunc(as.numeric(V9001)/100),
             pof = "RENDIMENTO_TRABALHO",
             V9002 = NA) %>%
      select(ID_uc, PESO_FINAL, Codigo, V9002, valor_mensal, pof)
  }
  else if(str_detect(microdata_file, regex("outros_rendimentos", ignore_case = TRUE))){

    ler_pof_geral(microdata_file) %>%
      mutate(ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             across(.cols = c(V9001, V8500_DEFLA,
                              V9011, FATOR_ANUALIZACAO, PESO_FINAL, ID_uc),
                    .fns = as.numeric)) %>%
      mutate(valor_mensal = ifelse( QUADRO==54,
                                    (V8500_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
                                    (V8500_DEFLA*FATOR_ANUALIZACAO)/12 ),
             Codigo = trunc(as.numeric(V9001)/100),
             pof = "OUTROS_RENDIMENTOS",
             V9002 = NA) %>%
      select(ID_uc, PESO_FINAL, Codigo, V9002, valor_mensal, pof)


  }
  else if(str_detect(microdata_file, regex("despesa_coletiva", ignore_case = TRUE))){

    ler_pof_geral(microdata_file) %>%
      mutate(ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             across(.fns = as.numeric),
             valor_mensal = ifelse( QUADRO==10|QUADRO==19,
                                    (V8000_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
                                    (V8000_DEFLA*FATOR_ANUALIZACAO)/12),
             Codigo = trunc(V9001/100),
             pof = "DESPESA_COLETIVA") %>%
      select(ID_uc, PESO_FINAL, Codigo, V9002, valor_mensal, pof)
  }
  else if(str_detect(microdata_file, regex("caderneta_coletiva", ignore_case = TRUE))){

    ler_pof_geral(microdata_file) %>%
      mutate(ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             across(.fns = as.numeric)) %>%
      filter(V9002 >= 7) %>%
      mutate(valor_mensal = (V8000_DEFLA*FATOR_ANUALIZACAO)/12,
             Codigo = trunc(V9001/100),
             pof = "CADERNETA_COLETIVA") %>%
      select(ID_uc, PESO_FINAL, Codigo, V9002, valor_mensal, pof)

  }
  else if(str_detect(microdata_file, regex("despesa_individual", ignore_case = TRUE))){

    ler_pof_geral(microdata_file) %>%
      mutate(ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             across(.fns = as.numeric)) %>%
      filter(V9002 >= 7) %>%
      mutate(valor_mensal = ifelse( QUADRO==44|QUADRO==47|QUADRO==48|QUADRO==49|QUADRO==50 ,
                                    V8000_DEFLA*V9011*FATOR_ANUALIZACAO/12 ,
                                    V8000_DEFLA*FATOR_ANUALIZACAO/12),
             Codigo = trunc(V9001/100),
             pof = "DESPESA_INDIVIDUAL") %>%
      select(ID_uc, PESO_FINAL, Codigo, V9002, valor_mensal, pof)
  }
  else if(str_detect(microdata_file, regex("aluguel_estimado", ignore_case = TRUE))){

    ler_pof_geral(microdata_file) %>%
      mutate(ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             across(.fns = as.numeric),
             valor_mensal = V8000_DEFLA*V9011*FATOR_ANUALIZACAO/12,
             Codigo = trunc(V9001/100),
             pof = "ALUGUEL_ESTIMADO") %>%
      select(ID_uc, PESO_FINAL, Codigo, V9002, valor_mensal, pof)
  }
  else{
    cat("Os registros POF 2017-2018 validos de rendimento sao: \n
    ALUGUEL_ESTIMADO, CADERNETA_COLETIVA, DESPESA_COLETIVA, \n
    DESPESA_INDIVIDUAL, OUTROS_RENDIMENTOS, RENDIMENTO_TRABALHO")
    stop()
  }
}
