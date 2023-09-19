#' Cria a base intemediária para o bndes criando um dataframe
#'
#' @param origem_processos dataset que contém os projetos de energia da fonte bndes
#'
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import stringr
#' @return
#' @export
#'
#' @examples
#' cria_base_intermediaria_bndes()
cria_base_intermediaria_bndes <- function(origem_processos = here::here("data/BNDES/BNDESnaoautomaticas2023.xlsx")) {


  bndes <- readxl::read_excel(origem_processos, skip = 4)%>%
           janitor::clean_names()


  bndes <- bndes %>%
    dplyr::mutate(
           prazo_execucao_meses  = as.numeric(prazo_carencia_meses) + as.numeric(prazo_amortizacao_meses),
           data_da_contratacao   = lubridate::ymd(data_da_contratacao),
           motor = tolower(stringi::stri_trans_general(descricao_do_projeto, "Latin-ASCII")),
           prazo_utilizacao      = lubridate::ymd(data_da_contratacao) %m+% months(prazo_execucao_meses),
           prazo_decorrido_anos  = as.integer(lubridate::time_length(prazo_utilizacao- data_da_contratacao, "years")),
           prazo_decorrido_dias  = lubridate::time_length(prazo_utilizacao- data_da_contratacao, "days"),
           numero_do_contrato2    = paste(numero_do_contrato, 1:nrow(bndes))

    )%>%
    dplyr::filter(prazo_utilizacao >= "2013-01-01",
           inovacao         == "SIM") %>%
    tidyr::drop_na(valor_contratado_r) %>%
    unique()


 bndes <- func_a(df = bndes,
               processo = numero_do_contrato2,
               data_inicio = data_da_contratacao,
               prazo_utilizacao = prazo_utilizacao,
               valor_projeto = valor_contratado_r)

 bndes <- dtc_categorias(bndes, numero_do_contrato2, motor)
 bndes <- bndes %>% dplyr::mutate(categorias = dplyr::recode(categorias,
                                                        "character(0" = "nenhuma categoria encontrada"))

  bndes <-bndes %>%
    dplyr::mutate(regiao_ag_executor = dplyr::recode(uf,
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
                                                       "TO" = "N"))


  bndes <- bndes %>% dplyr::mutate(
    id                           = paste("BNDES",
                                         numero_do_contrato, sep = "-"),
    titulo_projeto = descricao_do_projeto,
    fonte_de_dados                 = "BNDES",
    data_assinatura                = data_da_contratacao,
    data_limite                    = prazo_utilizacao,
    duracao_dias                   = prazo_decorrido_dias,
    status_projeto                 = situacao_do_contrato,
    duracao_meses                  = prazo_execucao_meses,
    duracao_anos                   = prazo_decorrido_anos,
    valor_contratado               = valor_contratado_r,
    valor_executado                = gasto_executado,
    nome_agente_financiador     = "Bndes",
    natureza_agente_financiador = "empresa pública",
    natureza_financiamento      = "pública",
    modalidade_financiamento    = modalidade_de_apoio,
    nome_agente_executor        = cliente,
    natureza_agente_executor    = natureza_do_cliente,
    'p&d_ou_demonstracao'          = NA ,
    uf_ag_executor                  = uf)

  names(bndes)=str_replace_all(names(bndes),"gasto_2","valor_executado_2")

  vars=c("id","fonte_de_dados","data_assinatura","data_limite","duracao_dias",
         "titulo_projeto","status_projeto","valor_contratado","valor_executado",
         "nome_agente_financiador","natureza_agente_financiador","modalidade_financiamento",
         "nome_agente_executor","natureza_agente_executor","uf_ag_executor",
         "regiao_ag_executor","natureza_agente_executor","natureza_financiamento",
         'p&d_ou_demonstracao',"modalidade_financiamento",
         names(bndes)[str_detect(names(bndes),"valor_executado_")],"motor","categorias")



  bndes <- bndes%>%
    dplyr::select(vars)




  bndes
}
