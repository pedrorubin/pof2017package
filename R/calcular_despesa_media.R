calcular_valor_despesa_mensal_uc_one <- function(df_pof_despesa_base,
                                                 tipo_despesa = 0,
                                                 df_pof_morador_base,
                                                 uf = "all",
                                                 regiao = "all"){

  pof_uc <- get(df_pof_morador_base) %>%
    mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
           NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
           ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC)) %>%
    filter(V0306 == "1") %>%
    select(ID_uc, UF, RENDA_TOTAL, PESO_FINAL) %>%
    mutate(across(.cols = -c(ID_uc),
                  .fns = as.numeric)) %>%
    mutate(numero_familias = sum(PESO_FINAL))

  pof_despesax <- get(df_pof_despesa_base) %>%
    right_join(pof_uc, by = c("ID_uc", "PESO_FINAL"))


  if(tipo_despesa == 0){

    despesa_nivel <- pof_despesax %>%
      filter(Nivel_0 == tipo_despesa) %>%
      group_by(ID_uc, PESO_FINAL, Nivel_0) %>%
      summarise(Descricao_0 = unique(Descricao_0),
                valor_uc = sum(valor,na.rm = T),
                numero_familias = unique(numero_familias)) %>%
      ungroup() %>%
      group_by(Nivel_0) %>%
      summarise(Descricao_0 = unique(Descricao_0),
                valor_medio_uc = sum(valor_uc*PESO_FINAL)/
                  unique(numero_familias)) %>%
      ungroup() %>%
      rename(Nivel = Nivel_0,
             Descricao = Descricao_0)
  }
  else if(tipo_despesa %in% c(1,2,3)){

    despesa_nivel <- pof_despesax %>%
      filter(Nivel_1 == tipo_despesa) %>%
      group_by(ID_uc, PESO_FINAL, Nivel_1) %>%
      summarise(Descricao_1 = unique(Descricao_1),
                valor_uc = sum(valor,na.rm = T),
                numero_familias = unique(numero_familias)) %>%
      ungroup() %>%
      group_by(Nivel_1) %>%
      summarise(Descricao_1 = unique(Descricao_1),
                valor_medio_uc = sum(valor_uc*PESO_FINAL)/
                  unique(numero_familias)) %>%
      ungroup() %>%
      rename(Nivel = Nivel_1,
             Descricao = Descricao_1)
  }
  else if(tipo_despesa %in% c(11, 12, 21, 22, 23, 31, 32)){

    despesa_nivel <- pof_despesax %>%
      filter(Nivel_2 == tipo_despesa) %>%
      group_by(ID_uc, PESO_FINAL, Nivel_2) %>%
      summarise(Descricao_2 = unique(Descricao_2),
                valor_uc = sum(valor,na.rm = T),
                numero_familias = unique(numero_familias)) %>%
      ungroup() %>%
      group_by(Nivel_2) %>%
      summarise(Descricao_2 = unique(Descricao_2),
                valor_medio_uc = sum(valor_uc*PESO_FINAL)/
                  unique(numero_familias)) %>%
      ungroup() %>%
      rename(Nivel = Nivel_2,
             Descricao = Descricao_2)
  }
  else if(tipo_despesa %in% c(1101:1111, 1201:1206)){

    despesa_nivel <- pof_despesax %>%
      filter(Nivel_3 == tipo_despesa) %>%
      group_by(ID_uc, PESO_FINAL, Nivel_3) %>%
      summarise(Descricao_3 = unique(Descricao_3),
                valor_uc = sum(valor,na.rm = T),
                numero_familias = unique(numero_familias)) %>%
      ungroup() %>%
      group_by(Nivel_3) %>%
      summarise(Descricao_3 = unique(Descricao_3),
                valor_medio_uc = sum(valor_uc*PESO_FINAL)/
                  unique(numero_familias)) %>%
      ungroup() %>%
      rename(Nivel = Nivel_3,
             Descricao = Descricao_3)
  }
  else if(tipo_despesa %in% c(110201:110208, 110301:110306, 110401:110407,
                              110501:110504, 110601:110610, 110701:110706,
                              110801:110805, 111001:111004, 111101:111106)){

    despesa_nivel <- pof_despesax %>%
      filter(Nivel_4 == tipo_despesa) %>%
      group_by(ID_uc, PESO_FINAL, Nivel_4) %>%
      summarise(Descricao_4 = unique(Descricao_4),
                valor_uc = sum(valor,na.rm = T),
                numero_familias = unique(numero_familias)) %>%
      ungroup() %>%
      group_by(Nivel_4) %>%
      summarise(Descricao_4 = unique(Descricao_4),
                valor_medio_uc = sum(valor_uc*PESO_FINAL)/
                  unique(numero_familias)) %>%
      ungroup() %>%
      rename(Nivel = Nivel_4,
             Descricao = Descricao_4)
  }
  else if(tipo_despesa %in% c(1102011, 1102012, 1102031:1102037)){

    despesa_nivel <- pof_despesax %>%
      filter(Nivel_5 == tipo_despesa) %>%
      group_by(ID_uc, PESO_FINAL, Nivel_5) %>%
      summarise(Descricao_5 = unique(Descricao_5),
                valor_uc = sum(valor,na.rm = T),
                numero_familias = unique(numero_familias)) %>%
      ungroup() %>%
      group_by(Nivel_5) %>%
      summarise(Descricao_5 = unique(Descricao_5),
                valor_medio_uc = sum(valor_uc*PESO_FINAL)/
                  unique(numero_familias)) %>%
      ungroup() %>%
      rename(Nivel = Nivel_5,
             Descricao = Descricao_5)
  }
  else{
    cat("ver tabela indice_despesa para os codigos de despesa")
    stop()
  }



  despesa_nivel
}

#' Calcular médias mensais de despesa (por tipo de despesa)
#'
#' Calcular médias mensais de despesa (por tipo de despesa)
#' @param df_pof_despesa_base O nome (string) do dataframe com os dados de despesa. Ver ler_pof_despesa
#' @param tipo_despesa=0 Tipo (ou tipos) de despesa. Ver indice_despesa
#' @param df_pof_morador_base O nome (string) do dataframe com o registro MORADOR. Ver ler_pof_geral
#' @return O valor médio real mensal do tipo de despesa escolhido
#' @seealso ler_pof_despesa, ler_pof_despesa_todas, montar_tabela_despesa_uc
#' @examples
#' calcular_despesa_media(df_pof_despesa_base = "pof_despesa", tipo_despesa = c(0,1,2), df_pof_morador_base = "pof_morador");
#' @export

calcular_despesa_media <- function(df_pof_despesa_base,
                                             tipo_despesa = 0,
                                             df_pof_morador_base){

  lista_pof <- list(df_pof_despesa_base = df_pof_despesa_base,
                    tipo_despesa = tipo_despesa,
                    df_pof_morador_base = df_pof_morador_base)

  lista_despesa_uc <- pmap_dfr(lista_pof,
                               calcular_valor_despesa_mensal_uc_one)

  lista_despesa_uc

}
