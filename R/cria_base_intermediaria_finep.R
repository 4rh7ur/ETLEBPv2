#' Cria a base intemediária para a finep  criando um dataframe
#'
#' @param origem_processos dataset que contém os casos da fonte finep
#'
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import stringr
#' @import readODS
#' @return
#' @export
#'
#' @examples
#' cria_base_intermediaria_finep()
cria_base_intermediaria_finep <- function(origem_processos = here::here("data/FINEP/LiberacaoFINEP2023.ods")){

  finep <- readODS::read_ods(path = origem_processos,
                    skip = 4,
                    sheet = "Projetos_Finep")

  names(finep)<-finep[1,]

  finep <- finep %>% janitor::clean_names()%>% dplyr::slice(-1)


  finep <- finep %>% dplyr::mutate(
    motor            = tolower(stringi::stri_trans_general(titulo, "Latin-ASCII")),
    valor_finep      = as.numeric(valor_finep),
    valor_liberado   = as.numeric(valor_liberado),
    data_liberacao   = lubridate::ymd(lubridate::dmy(data_liberacao)),
    data_assinatura  = lubridate::ymd(lubridate::dmy(data_assinatura)),
    prazo_utilizacao = lubridate::ymd(lubridate::dmy(prazo_utilizacao)),
    periodo_meses    = lubridate::time_length(prazo_utilizacao- data_assinatura, "months"),
    periodo_dias     = lubridate::time_length(prazo_utilizacao - data_assinatura, "days"),
    periodo_anos     = as.integer(lubridate::time_length(prazo_utilizacao - data_assinatura, "years") ),
    contrato2 = paste(contrato, 1:nrow(finep) )
  ) %>%
    dplyr::filter(
    prazo_utilizacao >= "2013-01-01") %>%
    tidyr::drop_na(valor_finep)

  finep <- func_a(df = finep,
                processo = contrato2,
                data_inicio = data_assinatura,
                prazo_utilizacao = prazo_utilizacao,
                valor_projeto = valor_finep)

  finep <- dtc_categorias(finep,processo = contrato2, motor = motor)
  finep <- finep %>% dplyr::mutate(
    categorias = dplyr::recode(categorias,
                               "character(0" = "nenhuma categoria encontrada"))
  finep <- finep %>% dplyr::mutate(regiao_ag_executor = dplyr::recode(uf,
                                                        "AC" = "N",
                                                        "AL" = "NE",
                                                        "AM" = "N",
                                                        "BA" = "NE",
                                                        "CE" = "NE",
                                                        "DF" = "CO",
                                                        "ES" = "SE",
                                                        "GO" = "CO",
                                                        "MA" = "NE",
                                                        "MG" = "SE",
                                                        "MS" = "CO",
                                                        "MT" = "CO",
                                                        "PA" = "N",
                                                        "PB" = "NE",
                                                        "PE" = "NE",
                                                        "PI" = "NE",
                                                        "PR" = "S",
                                                        "RJ" = "SE",
                                                        "RN" = "NE",
                                                        "RO" = "N",
                                                        "RS" = "S",
                                                        "SC" = "S",
                                                        "SE" = "NE",
                                                        "SP" = "SE",
                                                        "TO" = "N"
  ))

    finep <- finep %>%
      dplyr::mutate(
        id                           = paste("FINEP",
                                             contrato, sep = "-"),
        fonte_de_dados                 = "FINEP",
        data_limite                    = prazo_utilizacao,
        duracao_dias                   = periodo_dias,
        duracao_meses                  = periodo_meses,
        duracao_anos                   = periodo_anos,
        valor_contratado               = valor_finep,
        titulo_projeto                 = titulo,
        status_projeto                 = status,
        nome_agente_financiador        = "Finep",
        natureza_agente_financiador    = "Empresa Pública",
        natureza_financiamento         = "pública",
        modalidade_financiamento       = instrumento,
        nome_agente_executor           = proponente,
        natureza_agente_executor       = "Empresa Privada", #confirmar natureza juridica proponente
        'p&d_ou_demonstracao'          = case_when(
                                            str_detect(toupper(instrumento),paste(c("NÃO REEMBOLSÁVEL","SUBVENÇÃO"),collapse = "|")) ~ 0,
                                            instrumento == "" ~ 9,
                                            TRUE ~ 1),
        uf_ag_executor                 = uf,
        valor_executado               = gasto_executado)

    names(finep)=str_replace_all(names(finep),"gasto_2","valor_executado_2")

    vars=c("id","fonte_de_dados","data_assinatura","data_limite","duracao_dias",
          "titulo_projeto","status_projeto","valor_contratado","valor_executado",
          "nome_agente_financiador","natureza_financiamento","natureza_agente_financiador",
          "modalidade_financiamento","nome_agente_executor","natureza_agente_executor",
          "uf_ag_executor","regiao_ag_executor","p&d_ou_demonstracao",
          names(finep)[str_detect(names(finep),"valor_executado_")],"motor","categorias")


  finep <- finep %>%
    dplyr::select(vars)

  finep
}
