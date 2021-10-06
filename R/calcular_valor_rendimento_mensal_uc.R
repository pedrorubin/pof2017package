calcular_valor_rendimento_mensal_uc_one <- function(pof_rendimento,
                                                    tipo_rendimento = 0,
                                                    path_microdata,
                                                    uf = "all",
                                                    regiao = "all"){

  indicador_rend <- ifelse(tipo_rendimento %in% c(0,1,11,12,13,14), 1, 0)
  indicador_naomonet <- ifelse(tipo_rendimento %in% c(0,1,15), 1, 0)
  indicador_patrimonial <- ifelse(tipo_rendimento %in% c(0,2), 1, 0)

  #rendimento
  if(indicador_rend == 1){
      pof_rendimentox <- get(pof_rendimento)

    if(tipo_rendimento %in% c(11,12,13,14)){
      pof_rendimento_grupo <- pof_rendimentox %>%
        left_join(tradutor_rendimento, by = "Codigo") %>%
        filter(Nivel_2 %in% tipo_rendimento) %>%
        group_by(ID_uc, Descricao_2) %>%
        summarise(valor = sum(valor_mensal, na.rm = T),
                  PESO_FINAL = unique(PESO_FINAL)) %>%
        ungroup() %>%
        mutate(ID_uc = as.numeric(ID_uc))
    }
    else{
      pof_rendimento_grupo <- pof_rendimentox %>%
        left_join(tradutor_rendimento, by = "Codigo") %>%
        filter(Nivel_2 %in% c(11,12,13,14)) %>%
        group_by(ID_uc) %>%
        summarise(rendimento_monet = sum(valor_mensal, na.rm = T),
                  PESO_FINAL = unique(PESO_FINAL)) %>%
        ungroup() %>%
        mutate(ID_uc = as.numeric(ID_uc))
    }

  }
  #rendimento nao monetario
  if(indicador_naomonet == 1){

    pof_rendimentox1 <- get(pof_rendimento)

    parte1 <- pof_rendimentox1 %>%
      filter(pof %in% c("DESPESA_COLETIVA",
                        "DESPESA_INDIVIDUAL",
                        "CADERNETA_COLETIVA"),
             V9002 >= 7) %>%
      group_by(ID_uc) %>%
      summarise(dif1 = sum(valor_mensal,na.rm=T),
                PESO_FINAL = unique(PESO_FINAL)) %>%
      ungroup()

    # print(parte1 %>% dim())

    aluguel <- get(pof_rendimento) %>%
      filter(pof == "ALUGUEL_ESTIMADO")

    parte2a <- aluguel %>%
      group_by(ID_uc) %>%
      summarise(dif2a = sum(valor_mensal),
                PESO_FINAL = unique(PESO_FINAL))

    codigos_rend_naomonet <- c(8001:8024, 8026:8068, 8999,
                               10006, 10011,
                               12005:12008, 12010:12015, 12017:12020,
                               12023:12025, 12027:12036,12999)



    despesa_coletiva <- get(pof_rendimento) %>%
      filter(pof == "DESPESA_COLETIVA")

    parte2b <- despesa_coletiva %>%
      mutate(V9002 = as.numeric(V9002)) %>%
      filter(V9002 <= 6,
             Codigo %in% codigos_rend_naomonet) %>%
      group_by(ID_uc) %>%
      summarise(dif2b = sum(valor_mensal),
                PESO_FINAL = unique(PESO_FINAL))

    parte2 <- parte2a %>%
      left_join(parte2b, by = c("ID_uc", "PESO_FINAL")) %>%
      replace(is.na(.),0) %>%
      mutate(dif2 = dif2a - dif2b) %>%
      filter(dif2 > 0) %>%
      ungroup()#%>%
    # summarise(dif22 = sum(dif2))

    pof_rendimento_naomonet <- parte1 %>%
      full_join(parte2, by = c("ID_uc", "PESO_FINAL")) %>%
      ungroup() %>%
      replace(is.na(.), 0) %>%
      rowwise() %>%
      mutate(rendimento_naomonet = sum(dif1, dif2, na.rm = T)) %>%
      ungroup() %>%
      select(-starts_with("dif"))  %>%
      mutate(ID_uc = as.numeric(ID_uc))
  }
  # variacao patrimonial
  if(indicador_patrimonial == 1){

    path_outros <- str_c(path_microdata,"/OUTROS_RENDIMENTOS.txt")

    outros_rendimentos <- ler_pof_geral(path_outros) %>%
      mutate(ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             ID_pes = str_c(COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE),
             across(.cols = c(V9001, V8500_DEFLA,
                              V9011, FATOR_ANUALIZACAO, PESO_FINAL, ID_uc),
                    .fns = as.numeric)) %>%
      mutate(valor_mensal = ifelse( QUADRO==54,
                                    (V8500_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
                                    (V8500_DEFLA*FATOR_ANUALIZACAO)/12 ),
             Codigo = trunc(as.numeric(V9001)/100),
             pof = "OUTROS_RENDIMENTOS",
             V9002 = NA) %>%
      select(ID_uc, ID_pes, PESO_FINAL, Codigo, V9002, valor_mensal, pof)



    codigos_parte1 <-  c(55008, 55010, 55016, 55020, 55021,
                         55022, 55023, 55024, 55025, 55026,
                         55035, 55037, 55044, 55053, 55061)

    parte1 <- outros_rendimentos %>%
      filter(Codigo %in% codigos_parte1) %>%
      group_by(ID_uc) %>%
      summarise(dif1 = sum(valor_mensal),
                PESO_FINAL = unique(PESO_FINAL)) %>%
      ungroup()

    parte2a <- outros_rendimentos %>%
      filter(Codigo == 57001) %>%
      group_by(ID_pes) %>%
      summarise(dif2a = sum(valor_mensal),
                ID_uc = unique(ID_uc),
                PESO_FINAL = unique(PESO_FINAL))

    parte2b <- outros_rendimentos %>%
      filter(Codigo == 56001) %>%
      group_by(ID_pes) %>%
      summarise(dif2b = sum(valor_mensal))

    parte2 <- parte2a %>%
      left_join(parte2b, by = "ID_pes") %>%
      replace(is.na(.),0) %>%
      mutate(dif2 = dif2a - dif2b) %>%
      filter(dif2 > 0) %>%
      group_by(ID_uc) %>%
      summarise(dif2 = sum(dif2),
                PESO_FINAL = unique(PESO_FINAL)) %>%
      ungroup()

    parte3a <- outros_rendimentos %>%
      filter(Codigo == 57002) %>%
      group_by(ID_pes) %>%
      summarise(dif3a = sum(valor_mensal),
                ID_uc = unique(ID_uc),
                PESO_FINAL = unique(PESO_FINAL))

    parte3b <- outros_rendimentos %>%
      filter(Codigo == 56002) %>%
      group_by(ID_pes) %>%
      summarise(dif3b = sum(valor_mensal))

    parte3 <- parte3a %>%
      left_join(parte3b, by = "ID_pes") %>%
      replace(is.na(.),0) %>%
      mutate(dif3 = dif3a - dif3b) %>%
      filter(dif3 > 0) %>%
      group_by(ID_uc) %>%
      summarise(dif3 = sum(dif3),
                PESO_FINAL = unique(PESO_FINAL)) %>%
      ungroup()


    parte4a <- outros_rendimentos %>%
      filter(Codigo == 57003) %>%
      group_by(ID_pes) %>%
      summarise(dif4a = sum(valor_mensal),
                ID_uc = unique(ID_uc),
                PESO_FINAL = unique(PESO_FINAL))

    parte4b <- outros_rendimentos %>%
      filter(Codigo == 56003) %>%
      group_by(ID_pes) %>%
      summarise(dif4b = sum(valor_mensal))

    parte4 <- parte4a %>%
      left_join(parte4b, by = "ID_pes") %>%
      replace(is.na(.),0) %>%
      mutate(dif4 = dif4a - dif4b) %>%
      filter(dif4 > 0) %>%
      group_by(ID_uc) %>%
      summarise(dif4 = sum(dif4),
                PESO_FINAL = unique(PESO_FINAL)) %>%
      ungroup()


    parte5a <- outros_rendimentos %>%
      filter(Codigo == 57004) %>%
      group_by(ID_pes) %>%
      summarise(dif5a = sum(valor_mensal),
                ID_uc = unique(ID_uc),
                PESO_FINAL = unique(PESO_FINAL))

    parte5b <- outros_rendimentos %>%
      filter(Codigo == 56004) %>%
      group_by(ID_pes) %>%
      summarise(dif5b = sum(valor_mensal))

    parte5 <- parte5a %>%
      left_join(parte5b, by = "ID_pes") %>%
      replace(is.na(.),0) %>%
      mutate(dif5 = dif5a - dif5b) %>%
      filter(dif5 > 0) %>%
      group_by(ID_uc) %>%
      summarise(dif5 = sum(dif5),
                PESO_FINAL = unique(PESO_FINAL)) %>%
      ungroup()

    pof_variacao_patrimonial <- parte1 %>%
      full_join(parte2, by = c("ID_uc", "PESO_FINAL")) %>%
      full_join(parte3, by = c("ID_uc", "PESO_FINAL")) %>%
      full_join(parte4, by = c("ID_uc", "PESO_FINAL")) %>%
      full_join(parte5, by = c("ID_uc", "PESO_FINAL")) %>%
      replace(is.na(.),0) %>%
      rowwise() %>%
      mutate(variacao_patrimonial = sum(dif1,dif2,dif3,dif4,dif5)) %>%
      ungroup() %>%
      select(-starts_with("dif")) %>%
      mutate(ID_uc = as.numeric(ID_uc))

  }


  if(tipo_rendimento %in% c(11,12,13,14)){
    pof_final <- pof_rendimento_grupo %>%
      rename(tipo = Descricao_2)
  }
  else if(tipo_rendimento == 15){
    pof_final <- pof_rendimento_naomonet %>%
      rename(valor = rendimento_naomonet) %>%
      mutate(tipo = "rendimento_nao_monetario")
  }
  else if(tipo_rendimento == 2){
    pof_final <- pof_variacao_patrimonial %>%
      rename(valor = variacao_patrimonial) %>%
      mutate(tipo = "variacao_patrimonial")
  }
  else if(tipo_rendimento == 1){
    pof_final <- pof_rendimento_grupo %>%
      full_join(pof_rendimento_naomonet, by = c("ID_uc", "PESO_FINAL")) %>%
      rowwise() %>%
      mutate(valor = sum(rendimento_monet, rendimento_naomonet, na.rm = T)) %>%
      ungroup() %>%
      mutate(tipo = "rendimento_total")
  }
  else if(tipo_rendimento == 0){
    pof_final <- pof_rendimento_grupo %>%
      full_join(pof_rendimento_naomonet, by = c("ID_uc", "PESO_FINAL")) %>%
      full_join(pof_variacao_patrimonial, by = c("ID_uc", "PESO_FINAL")) %>%
      rowwise() %>%
      mutate(valor =
               sum(rendimento_monet, rendimento_naomonet, variacao_patrimonial,
                   na.rm = T)) %>%
      ungroup() %>%
      mutate(tipo = "rendimento_total_variacao_patrimonial")
  }
  else{
    cat("Favor escolher um valor valido: \n
        0 - rendimento total + variacao patrimonial \n
        1 - rendimento total \n
        2 - variacao patrimonial \n
        11 - rendimento do trabalho \n
        12 - transferencias \n
        13 - renda de aluguel \n
        14 - outras rendas \n
        15 - rendimento nao monetario")
    stop()
  }

  path_morador <- str_c(path_microdata,"/MORADOR.txt")

  pof_calculo <- ler_pof_geral(path_morador) %>%
    mutate(ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC)) %>%
    filter(V0306 == "1") %>%
    select(ID_uc, PESO_FINAL, UF) %>%
    mutate(across(.fns = as.numeric)) %>%
    left_join(pof_final, by = c("ID_uc", "PESO_FINAL"))


  pof_calculo %>%
    summarise(Nivel = tipo_rendimento,
              Descricao = unique(tipo),
              media_mensal_uc = sum(valor*PESO_FINAL, na.rm = T)/sum(PESO_FINAL)) %>%
    filter(is.na(Descricao) == FALSE)




}

#' Mean monthly income values (by type of income)
#'
#' Mean monthly income values (by type of income)
#' @param pof_rendimento The name of the df with the income data (string). See ler_pof_rendimento.
#' @param tipo_rendimento=0 The type (or types) of income. Default to total income. See indice_rendimento
#' @param path_microdata The path to the microdata folder
#' @param uf="all" The relevant federal unit (numeric). NOT IMPLEMENTED YET
#' @param regiao="all" The relevant macroregion (character code). NOT IMPLEMENTED YET
#' @return The mean deflated monthly income
#' @examples
#' calcular_valor_rendimento_mensal_uc(pof_rendimento = "df_income", tipo_despesa = c(0,1,2), path_midrodata = "./microdata_folder");
#' @export

calcular_valor_rendimento_mensal_uc <- function(pof_rendimento,
                                                tipo_rendimento = 0,
                                                path_microdata,
                                                uf = "all",
                                                regiao = "all"){

  lista_pof <- list(pof_rendimento = pof_rendimento,
                    tipo_rendimento = tipo_rendimento,
                    path_microdata = path_microdata)

  lista_rendimento_uc <- pmap_dfr(lista_pof,
                                  calcular_valor_rendimento_mensal_uc_one)

  lista_rendimento_uc

}
