montar_tabela_despesa_uc_one <- function(df_pof_despesa_base,
                                         tipo_despesa = 0,
                                         df_pof_morador_base){

  # pof_uc <- get(df_pof_morador_base) %>%
  #   mutate(ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC)) %>%
  #   filter(V0306 == "1") %>%
  #   # select(ID_uc, UF, RENDA_TOTAL, PESO_FINAL) %>%
  #   mutate(across(.fns = as.numeric)) %>%
  #   mutate(numero_familias = sum(PESO_FINAL))


  pof_despesa <- get(df_pof_despesa_base) #%>%
    # right_join(pof_uc, by = c("ID_uc", "PESO_FINAL"))

  if(tipo_despesa == 0){

    despesa_nivel <- pof_despesa %>%
      filter(Nivel_0 == tipo_despesa) %>%
      group_by(ID_uc, PESO_FINAL, Nivel_0) %>%
      summarise(Descricao_0 = unique(Descricao_0),
                valor_uc = sum(valor,na.rm = T),
                # numero_familias = unique(numero_familias)
                ) %>%
      ungroup() %>%
      mutate(Descricao = str_c(Descricao_0,"_",Nivel_0)) %>%
      select(-c(Nivel_0,Descricao_0)) %>%
      pivot_wider(names_from = Descricao, values_from = valor_uc)

  }
  else if(tipo_despesa %in% c(1,2,3)){

    despesa_nivel <- pof_despesa %>%
      filter(Nivel_1 == tipo_despesa) %>%
      group_by(ID_uc, PESO_FINAL, Nivel_1) %>%
      summarise(Descricao_1 = unique(Descricao_1),
                valor_uc = sum(valor,na.rm = T),
                # numero_familias = unique(numero_familias)
                ) %>%
      ungroup() %>%
      mutate(Descricao = str_c(Descricao_1,"_",Nivel_1)) %>%
      select(-c(Nivel_1,Descricao_1)) %>%
      pivot_wider(names_from = Descricao, values_from = valor_uc)

  }
  else if(tipo_despesa %in% c(11,12,21,22,23,31,32)){

    despesa_nivel <- pof_despesa %>%
      filter(Nivel_2 == tipo_despesa) %>%
      group_by(ID_uc, PESO_FINAL, Nivel_2) %>%
      summarise(Descricao_2 = unique(Descricao_2),
                valor_uc = sum(valor,na.rm = T),
                # numero_familias = unique(numero_familias)
                ) %>%
      ungroup() %>%
      mutate(Descricao = str_c(Descricao_2,"_",Nivel_2)) %>%
      select(-c(Nivel_2,Descricao_2)) %>%
      pivot_wider(names_from = Descricao, values_from = valor_uc)

  }
  else if(tipo_despesa %in% c(1101:1111, 1201:1206)){

    despesa_nivel <- pof_despesa %>%
      filter(Nivel_3 == tipo_despesa) %>%
      group_by(ID_uc, PESO_FINAL, Nivel_3) %>%
      summarise(Descricao_3 = unique(Descricao_3),
                valor_uc = sum(valor,na.rm = T),
                # numero_familias = unique(numero_familias)
                ) %>%
      ungroup() %>%
      mutate(Descricao = str_c(Descricao_3,"_",Nivel_3)) %>%
      select(-c(Nivel_3,Descricao_3)) %>%
      pivot_wider(names_from = Descricao, values_from = valor_uc)


  }
  else if(tipo_despesa %in% c(110201:110208, 110301:110306,
                              110401:110407, 110501:110504,
                              110601:110610, 110701:110706,
                              110801:110805, 110901:110904,
                              111001:111004, 111101:111106)){

    despesa_nivel <- pof_despesa %>%
      filter(Nivel_4 == tipo_despesa) %>%
      group_by(ID_uc, PESO_FINAL, Nivel_4) %>%
      summarise(Descricao_4 = unique(Descricao_4),
                valor_uc = sum(valor,na.rm = T),
                # numero_familias = unique(numero_familias)
                ) %>%
      ungroup() %>%
      mutate(Descricao = str_c(Descricao_4,"_",Nivel_4)) %>%
      select(-c(Nivel_4,Descricao_4)) %>%
      pivot_wider(names_from = Descricao, values_from = valor_uc)

  }
  else if(tipo_despesa %in% c(1102011:1102012, 1102031:1102037)){

    despesa_nivel <- pof_despesa %>%
      filter(Nivel_5 == tipo_despesa) %>%
      group_by(ID_uc, PESO_FINAL, Nivel_5) %>%
      summarise(Descricao_5 = unique(Descricao_5),
                valor_uc = sum(valor,na.rm = T),
                # numero_familias = unique(numero_familias)
                ) %>%
      ungroup() %>%
      mutate(Descricao = str_c(Descricao_5,"_",Nivel_5)) %>%
      select(-c(Nivel_5,Descricao_5)) %>%
      pivot_wider(names_from = Descricao, values_from = valor_uc)
  }
}

#' Montar tabela com despesa mensal de cada UC (por tipo de despesa)
#'
#' Montar tabela com despesa mensal de cada UC (por tipo de despesa)
#' @param df_pof_despesa_base O nome (string) do dataframe com os dados de despesa. Ver ler_pof_despesa
#' @param tipo_despesa=0 Tipo (ou tipos) de despesa. Ver indice_despesa
#' @param df_pof_morador_base O nome (string) do dataframe com o registro MORADOR. Ver ler_pof_geral
#' @return Um dataframe com todas as UC (registro MORADOR) com os valores de cada tipo de despesa como colunas
#' @seealso ler_pof_despesa, ler_pof_despesa_todas, calcular_despesa_media
#' @examples
#' montar_tabela_despesa_uc(df_pof_despesa_base = "pof_despesa",
#' tipo_despesa = c(0,1,2),
#' df_pof_morador_base = "pof_morador");
#' @export

montar_tabela_despesa_uc <- function(df_pof_despesa_base,
                                     tipo_despesa = 0,
                                     df_pof_morador_base){

  # lista_pof <- list(pof = pof_despesa,
  #                   tipo_despesa_escolha = tipo_despesa)

  lista_pof <- list(df_pof_despesa_base = df_pof_despesa_base,
                    tipo_despesa = tipo_despesa#,
                    # df_pof_morador_base = df_pof_morador_base
                    )

  lista_despesa_uc <- pmap(lista_pof,
                           montar_tabela_despesa_uc_one)

  df_despesa_uc <- lista_despesa_uc %>%
    reduce(full_join, by = c("ID_uc", "PESO_FINAL"))

  pof_uc <- get(df_pof_morador_base) %>%
    mutate(ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC)) %>%
    filter(V0306 == "1") %>%
    # select(ID_uc, UF, RENDA_TOTAL, PESO_FINAL) %>%
    mutate(across(.fns = as.numeric)) %>%
    mutate(numero_familias = sum(PESO_FINAL)) %>%
    left_join(df_despesa_uc, by = c("ID_uc", "PESO_FINAL"))



}
