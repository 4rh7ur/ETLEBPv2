
#' Cria a base intemediária para a aneel criando um dataframe
#' @param origem_processos dataset PD Busca Textual que contém os projetos de energia da fonte aneel
#'
#' @param origem_equipes dataset que 5.PD RF EQUIPE contém informações complementares da fonte aneel
#'
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import stringr
#' @import readr
#' @return
#' @export
#'
#' @examples
#' cria_base_intermediaria_aneel()
cria_base_intermediaria_aneel <- function(
  origem_processos = here::here("data/SGPED_BI/ANEEL_proj-ped-energia-eletrica.csv")
  #origem_equipes = here::here("data/SGPED_BI/5.PD RF EQUIPE.csv")
){


  options(scipen=999)
  ##get the data ##

  #importando o dataset
  anel_pd <- read.csv2(origem_processos,encoding = "latin1") %>%
    janitor::clean_names()

  anel_pd <- anel_pd %>%
    mutate(data_de_carregamento = as.Date(paste0(ano_cadastro_proposta_projeto,"-01-01")),
           dat_conclusao_projeto = as.Date(dat_conclusao_projeto,format="%d/%m/%Y")) %>%
    rename("data_de_conclusao" = "dat_conclusao_projeto",
           "duracao_prevista_meses" = "qtd_meses_duracao_prevista",
           "custo_total_previsto" = "vlr_custo_total_previsto",
           "custo_total_realizado" = "vlr_custo_total_auditado",
           "titulo" = "dsc_titulo_projeto",
           "segmento" = "sig_segmento_setor_eletrico",
           "tema" = "sig_tema_projeto",
           "proponente" = "nom_agente",
           "situacao" = "idc_situacao_projeto") %>%


    dplyr::mutate(duracao_prevista              = case_when(!is.na(data_de_conclusao) ~ data_de_conclusao,
                                                     TRUE ~ lubridate::date(data_de_carregamento+lubridate::dmonths(duracao_prevista_meses))),
           custo_total_previsto          = as.numeric(stringr::str_replace_all(
                                           stringr::str_replace_all(custo_total_previsto, "[.$]", ""), "[,]", "." )),
           custo_total_realizado         = as.numeric(stringr::str_replace_all(
                                           stringr::str_replace_all(custo_total_realizado, "[.$]", ""), "[,]", "." )),
           custo_total_realizado         = dplyr::case_when(is.na(custo_total_realizado) ~ custo_total_previsto,
                                           TRUE~custo_total_realizado),
           duracao_dias                  = lubridate::time_length(data_de_conclusao - data_de_carregamento, "days"),
           duracao_anos                  = as.integer(lubridate::interval(data_de_carregamento, data_de_conclusao)/lubridate::dyears()),
           data_de_conclusao             = duracao_prevista,
           motor                         = stringi::stri_trans_general(paste(titulo,segmento,tema),
                                                                "Latin-ASCII"),
           motor                         = tolower(motor)) %>%
    dplyr::filter(duracao_prevista >= "2013-01-01") %>%
    tidyr::drop_na(custo_total_previsto)

   anel_pd <- func_a(anel_pd, cod_proj, data_de_carregamento,
                     data_de_conclusao,custo_total_previsto)

   anel_pd <- dtc_categorias(anel_pd,cod_proj, motor)

   anel_pd <- anel_pd %>% dplyr::mutate(categorias = dplyr::recode(categorias,
                                                        "character(0" = "nenhuma categoria encontrada"))

   anel_pd <- anel_pd %>%
     mutate('p&d_ou_demonstracao' = ifelse(str_detect(sig_fas_inovacao_projeto,paste(c("CS","LP","IM"), collapse = "|")),1,0))

   anel_pd <- anel_pd %>%
    dplyr::mutate(
           id                          = paste("ANEEL", cod_proj, sep = "-"),
           fonte_de_dados              = "ANEEL",
           data_assinatura             = data_de_carregamento,
           data_limite                 = data_de_conclusao,
           duracao_dias                = duracao_dias,
           duracao_meses               = duracao_prevista,
           valor_contratado            = custo_total_previsto,
           valor_executado             = gasto_executado,
           nome_agente_financiador     = proponente,
           natureza_agente_financiador = "Empresa Privada", # confirmar
           natureza_financiamento      = "Publicamente Orientado",
           modalidade_financiamento    = "Não se Aplica",
           nome_agente_executor        = proponente,
           natureza_agente_executor    = "Empresa Privada", # confirmar
           titulo_projeto              = titulo,
           status_projeto              = situacao
    )

  names(anel_pd)=str_replace_all(names(anel_pd),"gasto_2","valor_executado_2")

  vars=c("id","fonte_de_dados","data_assinatura","data_limite","duracao_dias",
         "titulo_projeto","status_projeto","valor_contratado","valor_executado",
         "nome_agente_financiador","natureza_agente_financiador","modalidade_financiamento",
         "nome_agente_executor","natureza_agente_executor","natureza_financiamento","p&d_ou_demonstracao",
         names(anel_pd)[str_detect(names(anel_pd),"valor_executado_")],"motor","categorias")



  anel_pd <- anel_pd %>%
    dplyr::select(vars)

  #write.csv(anel_pd, here::here("inst/intermediarias/aneel_interm_06_10_2021.csv"))

  anel_pd

}


