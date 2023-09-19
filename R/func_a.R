#' Calcula o valor dos dispêndios do projeto por ano em que o projeto foi executado

#' @import tidyr
#' @import lubridate
#' @import dplyr
#' @param df dataframe que contem os projetos de energia
#' @param processo atributo de id unico do projeto de energia
#' @param data_inicio atributo que mede a data de inicio do contrato do projeto de energia
#' @param prazo_utilizacao atributo que mede a data de finalização do contrato do projeto de energia
#' @param valor_projeto atributo que mede o valor de financiamento do projeto de energia
#'
#' @return
#' @export
#'
#' @examples
#' func_a(dataset, id, data_inicio, prazo_utilizacao, valor_projeto)

func_a<-function(df,
               processo,
               data_inicio,
               prazo_utilizacao,
               valor_projeto){

  ano_inicio <- 2013
  ano_fim <- unlist(df %>% mutate(year=lubridate::year({{prazo_utilizacao}})) %>% filter(year==max(year))%>%
                      dplyr::select(year) %>% unique())

  anos_periodos <- dplyr::tibble(
    ano_contagem_dias = ano_inicio:ano_fim,
    inicio_ano_contagem_dias = lubridate::make_date(ano_contagem_dias, 1, 1 ),
    fim_ano_contagem_dias = lubridate::make_date(ano_contagem_dias, 12, 31 )
  )

  df_resumido <-select(df, {{processo}}, {{data_inicio}},
                       {{prazo_utilizacao}},{{valor_projeto}})


  calculos <- df_resumido %>%
    tidyr::crossing(
      anos_periodos
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      data_inicio_contagem_projeto = max({{data_inicio}}, inicio_ano_contagem_dias),
      data_fim_contagem_projeto = min({{prazo_utilizacao}}, fim_ano_contagem_dias)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      duracao_dias = time_length(data_fim_contagem_projeto - data_inicio_contagem_projeto, unit = "days"),
      #inseri aqui uma nova linha
      duracao_dias = ifelse(duracao_dias >= 1, duracao_dias+1, duracao_dias)
    ) %>%
    dplyr::group_by({{processo}}) %>%
    dplyr::mutate(
      duracao_dias = if_else(duracao_dias >= 0, duracao_dias, NA_real_),
      gasto_ano = paste0("gasto_", ano_contagem_dias),
      total_dias_projeto = sum(duracao_dias, na.rm = T)

    ) %>%
    dplyr::ungroup() %>%
    #Aqui eu inseri uma alteração
    #para duração dias >1 total_dias_projeto-1
    #para duração dias ==1 total_dias_projeto
    dplyr::mutate(gasto_executado = case_when(duracao_dias > 1 ~ ((total_dias_projeto-1)/time_length({{prazo_utilizacao}}-{{data_inicio}}, "days"))*{{valor_projeto}},
                                              duracao_dias == 1 ~ ((total_dias_projeto)/time_length({{prazo_utilizacao}}-{{data_inicio}}, "days"))*{{valor_projeto}},
                                       duracao_dias == 0 ~ {{valor_projeto}}),
           valor_gasto_ano = (gasto_executado/total_dias_projeto)*duracao_dias,
           gasto_executado = case_when(is.na(gasto_executado) == T ~ gasto_executado)) %>%
    tidyr::replace_na(list(valor_gasto_ano = 0, duracao_dias =0 )) %>%

    dplyr::relocate(
      {{processo}},
      duracao_dias,
      ano_contagem_dias,
      gasto_ano,
      gasto_executado,
      valor_gasto_ano,
      total_dias_projeto,
      .after = {{prazo_utilizacao}}
    )  %>%
    dplyr::select({{processo}}, gasto_ano,valor_gasto_ano)%>%
    tidyr::pivot_wider(names_from =  gasto_ano,
                       values_from = valor_gasto_ano)%>%
    dplyr::group_by({{processo}}) %>%
    dplyr::mutate(gasto_executado = sum(c_across(starts_with("gasto_2")), na.rm = T)) %>%
    dplyr::ungroup()

  df <-left_join(df, calculos)

  corrige_valores <-function(df){
    data <- df

    data <- data %>%
      mutate(proxy = ifelse({{data_inicio}} == {{prazo_utilizacao}} & gasto_executado == 0, "sim", "não"),
             gasto_executado = ifelse(proxy == "sim" , {{valor_projeto}}, gasto_executado)) %>%
      mutate_at(vars(starts_with("gasto_2")),funs(ifelse(proxy == "sim",{{valor_projeto}},.)))

    data

    data<-data %>%
      select(-proxy)
  }

  df <- corrige_valores(df)


}

usethis::use_data(func_a, overwrite = T)
