#' Cria a base intemediária para a anp criando um dataframe
#'
#' @param origem_processos dataset que contem os projetos da fonte anp
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
#' cria_base_intermediaria_anp()
cria_base_intermediaria_anp <- function(origem_processos = here::here("data/ANP/projetos-rt-3-2015_2023.csv")#,
                                        #origem_enriquecimento = here::here("data/ANP/4.anp.csv")
                                        ) {

  anp <- readr::read_delim(origem_processos,
                         ";", escape_double = FALSE, trim_ws = TRUE) %>%
              janitor::clean_names()

  anp <- anp %>%
    dplyr::mutate(
      valor_projeto       = as.numeric(stringr::str_replace_all(
                             stringr::str_remove_all(valor_clausula, "[R$ ]"), "[,]", "")),
      data_inicio          = lubridate::dmy(data_inicio),
      prazo_utilizacao     = data_inicio + months(prazo),
      prazo_decorrido_dias = lubridate::time_length(prazo_utilizacao - data_inicio, "days"),
      prazo_decorrido_anos = as.integer(prazo_decorrido_dias/365),
      motor                = stringi::stri_trans_general(paste(titulo, objetivo, tema, subtema),
                                                         "Latin-ASCII"),
      motor                = tolower(motor)
    ) %>%
    dplyr::filter(prazo_utilizacao >= "2013-01-01") %>%
    tidyr::drop_na(valor_projeto) %>%
    func_a(#df = anp_2015,
           processo = no_anp,
           data_inicio = data_inicio,
           prazo_utilizacao = prazo_utilizacao,
           valor_projeto = valor_projeto)


anp <-dtc_categorias(anp,no_anp, motor)
anp <- anp %>% dplyr::mutate(categorias = dplyr::recode(categorias,
                                                                 "character(0" = "nenhuma categoria encontrada"))

anp <- valida_termos_anp(anp, anp$categorias)

  anp<-anp %>%
    dplyr::mutate(
    id                = paste("ANP", no_anp, sep = "-"),
    fonte_de_dados              = "ANP",
    titulo_projeto              = titulo,
    data_assinatura             = data_inicio,
    data_limite                 = prazo_utilizacao,
    duracao_meses               = prazo,
    duracao_dias                = prazo_decorrido_dias,
    duracao_anos                = prazo_decorrido_anos,
    valor_contratado            = valor_projeto,
    valor_executado             = gasto_executado,
    nome_agente_financiador     = empresa_responsavel,
    natureza_agente_financiador = "Empresa Privada", # confirmar
    natureza_financiamento      = "publicamente orientado",
    modalidade_financiamento    = NA,
    nome_agente_executor        = executor_1,
    natureza_agente_executor    = 'Empresa Privada', # confirmar
    'p&d_ou_demonstracao'       = NA,
    uf_ag_executor              = NA,
    regiao_ag_executor          = NA,
    status_projeto              = NA)

  names(anp)=str_replace_all(names(anp),"gasto_2","valor_executado_2")

  vars=c("id","fonte_de_dados","data_assinatura","data_limite","duracao_dias",
         "titulo_projeto","status_projeto","valor_contratado","valor_executado",
         "nome_agente_financiador","natureza_financiamento","natureza_agente_financiador",
         "modalidade_financiamento","nome_agente_executor","natureza_agente_executor",
         "uf_ag_executor","regiao_ag_executor","p&d_ou_demonstracao",
         names(anp)[str_detect(names(anp),"valor_executado_")],"motor","categorias")

  anp<-anp %>%
    dplyr::select(vars)

  anp
}
