#' Carregar microdados da POF - todas as informações de despesa
#'
#' Carregar microdados da POF - todos os registros com dados de despesa
#' Aluguel estimado, caderneta coletiva, despesa coletiva, despesa individual, outros rendimentos e rendimento trabalho
#' OBS: É preciso que o nome dos arquivos .txt esteja igual a como foi baixado!!
#'
#' @param arquivo_microdados O caminho até o arquivo txt do registro de despesa a ser carregado
#' @return Um dataframe com as informações de despesa do registro
#' @examples ler_pof_despesa("./microdados/ALUGUEL_ESTIMADO.txt");
#' @seealso ler_pof_despesa_todas, baixar_pof
#' @export

ler_pof_despesa <- function(arquivo_microdados){

  if(!(str_detect(arquivo_microdados,
                regex("aluguel_estimado|caderneta_coletiva|despesa_coletiva|despesa_individual|outros_rendimentos|rendimento_trabalho",
                                      ignore_case = TRUE)))){

    cat("Os registros de POF 2017-2018 aceitos para a despesa sao: \n
          ALUGUEL_ESTIMADO, CADERNETA_COLETIVA, DESPESA_COLETIVA, \n
          DESPESA_INDIVIDUAL, OUTROS_RENDIMENTOS, RENDIMENTO_TRABALHO \n")
    stop()
  }

  if(str_detect(arquivo_microdados, regex("aluguel_estimado", ignore_case = TRUE))){

    pof <- ler_pof_geral(arquivo_microdados) %>%
      mutate(across(.cols = c(V8000_DEFLA,V9011,FATOR_ANUALIZACAO,PESO_FINAL),
                    .fns = as.numeric)) %>%
      mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
             NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
             ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             valor_mensal=(V8000_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
             inss_mensal = NA,
             prev_pub_mensal = NA,
             ir_mensal = NA,
             iss_mensal = NA,
             deducao_mensal = NA,
             pof = "ALUGUEL_ESTIMADO") %>%
      select(ID_uc, PESO_FINAL, V9001, pof,
             valor_mensal, inss_mensal, prev_pub_mensal, ir_mensal, iss_mensal, deducao_mensal)
  }
  else if(str_detect(arquivo_microdados, regex("despesa_coletiva", ignore_case = TRUE))){

    pof <- ler_pof_geral(arquivo_microdados) %>%
      mutate(across(.cols = c(QUADRO,
                              V8000_DEFLA,V1904_DEFLA,V9011,
                              FATOR_ANUALIZACAO,PESO_FINAL),
                    .fns = as.numeric)) %>%
      mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
             NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
             ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             valor_mensal = ifelse( QUADRO==10|QUADRO==19,
                                    (V8000_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
                                    (V8000_DEFLA*FATOR_ANUALIZACAO)/12),
             inss_mensal = (V1904_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
             prev_pub_mensal = NA,
             ir_mensal = NA,
             iss_mensal = NA,
             deducao_mensal = NA,
             pof = "DESPESA_COLETIVA") %>%
      select(ID_uc, PESO_FINAL, V9001, pof,
             valor_mensal, inss_mensal, prev_pub_mensal, ir_mensal, iss_mensal, deducao_mensal)
  }
  else if(str_detect(arquivo_microdados, regex("caderneta_coletiva", ignore_case = TRUE))){

    pof <- ler_pof_geral(arquivo_microdados) %>%
      mutate(across(.cols = c(V8000_DEFLA,FATOR_ANUALIZACAO,PESO_FINAL),
                    .fns = as.numeric)) %>%
      mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
             NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
             ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             valor_mensal=(V8000_DEFLA*FATOR_ANUALIZACAO)/12,
             inss_mensal = NA,
             prev_pub_mensal = NA,
             ir_mensal = NA,
             iss_mensal = NA,
             deducao_mensal = NA,
             pof = "CADERNETA_COLETIVA") %>%
      select(ID_uc, PESO_FINAL, V9001, pof,
             valor_mensal, inss_mensal, prev_pub_mensal, ir_mensal, iss_mensal, deducao_mensal)
  }
  else if(str_detect(arquivo_microdados, regex("despesa_individual", ignore_case = TRUE))){

    pof <- ler_pof_geral(arquivo_microdados) %>%
      mutate(across(.cols = c(V8000_DEFLA,FATOR_ANUALIZACAO,PESO_FINAL, V9011),
                    .fns = as.numeric)) %>%
      mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
             NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
             ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             valor_mensal = ifelse( QUADRO==44|QUADRO==47|QUADRO==48|QUADRO==49|QUADRO==50,
                                    (V8000_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
                                    (V8000_DEFLA*FATOR_ANUALIZACAO)/12),
             inss_mensal = NA,
             prev_pub_mensal = NA,
             ir_mensal = NA,
             iss_mensal = NA,
             deducao_mensal = NA,
             pof = "DESPESA_INDIVIDUAL") %>%
      select(ID_uc, PESO_FINAL, V9001, pof,
             valor_mensal, inss_mensal, prev_pub_mensal, ir_mensal, iss_mensal, deducao_mensal)
  }
  else if(str_detect(arquivo_microdados, regex("rendimento_trabalho", ignore_case = TRUE))){

    pof <- ler_pof_geral(arquivo_microdados) %>%
      mutate(across(.cols = c(V531112_DEFLA,V531122_DEFLA,V531132_DEFLA,
                              V9011,FATOR_ANUALIZACAO,PESO_FINAL),
                    .fns = as.numeric)) %>%
      mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
             NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
             ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             valor_mensal = NA,
             inss_mensal = NA,
             prev_pub_mensal=(V531112_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
             ir_mensal=(V531122_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
             iss_mensal=(V531132_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
             deducao_mensal = NA,
             pof = "RENDIMENTO_TRABALHO") %>%
      select(ID_uc, PESO_FINAL, V9001, pof,
             valor_mensal, inss_mensal, prev_pub_mensal, ir_mensal, iss_mensal, deducao_mensal)
  }
  else if(str_detect(arquivo_microdados, regex("outros_rendimentos", ignore_case = TRUE))){

    pof <- ler_pof_geral(arquivo_microdados) %>%
      mutate(across(.cols = c(V8501_DEFLA,V9011,FATOR_ANUALIZACAO,PESO_FINAL),
                    .fns = as.numeric)) %>%
      mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
             NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
             ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             deducao_mensal = ifelse( QUADRO==54,
                                      (V8501_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
                                      (V8501_DEFLA*FATOR_ANUALIZACAO)/12),
             valor_mensal = NA,
             inss_mensal = NA,
             prev_pub_mensal = NA,
             ir_mensal = NA,
             iss_mensal = NA,
             pof = "OUTROS_RENDIMENTOS",
             variavel = "V8501_DEFLA") %>%
      select(ID_uc, PESO_FINAL, V9001, pof,
             valor_mensal, inss_mensal, prev_pub_mensal, ir_mensal, iss_mensal, deducao_mensal)
  }

  # tradutor_despesa <-
  #   read_excel("./tradutores/Tradutor_Despesa_Geral.xls") %>%
  #   mutate(across(.cols = starts_with("Descricao"),
  #                 .fns = toupper))

  pof <- pof %>%
    mutate(Codigo = trunc(as.numeric(V9001)/100)) %>%
    left_join(tradutor_despesa,
              by = "Codigo") %>%
    mutate(valor = case_when(
      Variavel == "V8000_DEFLA"    ~ as.numeric(valor_mensal),
      Variavel == "V1904_DEFLA"    ~ as.numeric(inss_mensal),
      Variavel == "V531112_DEFLA"  ~ as.numeric(prev_pub_mensal),
      Variavel == "V531122_DEFLA"  ~ as.numeric(ir_mensal),
      Variavel == "V531132_DEFLA"  ~ as.numeric(iss_mensal),
      Variavel == "V8501_DEFLA"    ~ as.numeric(deducao_mensal),
      TRUE ~ 0
    )) %>%
    select(-c(ends_with("mensal"), ends_with("DEFLA"))) %>%
    mutate(ID_uc = as.numeric(ID_uc))
}
