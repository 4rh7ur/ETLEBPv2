#' Cria a base intemediária para o CNEN criando um dataframe
#'
#' @param origem_processos dataset que contém os projetos da fonte cnen
#'
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import stringr
#' @return
#' @export
#'
#' @examples
#' cria_base_intermediaria_cnen()

origem_processos="/Users/silvanooliveira/Library/Mobile Documents/com~apple~CloudDocs/Consultoria/CEPAL/ETLEBP2/data/CNEN/Projeto CNEN_extra_2023.xlsx"

cria_base_intermediaria_cnen <- function(origem_processos = here::here("data/CNEN/CNEN_primario_2021_2022.xlsx")){

  cnen <- readxl::read_excel(origem_processos,
                     col_types = c("text", "text", "text",
                                   "text", "date", "date", "text", "numeric",
                                   "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text")) %>%
    janitor::clean_names() %>%
    dplyr::slice(-c(1,2,3)) %>%
    dplyr::mutate(data_assinatura = lubridate::ymd(data_assinatura),
           data_limite     = lubridate::ymd(data_limite),
           duracao_dias    = lubridate::time_length(data_limite - data_assinatura, "days"),
           categorias = categoria_da_tecnologia_digito2
          ) %>%
    func_a(id, data_assinatura, data_limite, valor_contratado)


  cnen<-cnen %>%
    dplyr::mutate(
    id                              = paste("CNEN",id,lubridate::year(data_assinatura),sep = "-"),
    titulo_projeto                  = titulo,
    status_projeto                  = NA,
    nome_agente_financiador         = "Sem informação",
    nome_agente_executor            = nome_do_agente_executor,
    natureza_agente_financiador     = ifelse(is.na(natureza_do_agente_financiador),"sem informação",natureza_do_agente_financiador),
    natureza_financiamento          = natureza_do_financiamento,
    natureza_agente_executor        = natureza_do_agente_executor,
    modalidade_financiamento        = modalidade_do_financiamento,
    uf_ag_executor                  = uf_execucao,
    valor_executado                 = gasto_executado,
    `p&d_ou_demonstracao`           = case_when(str_detect(p_d_ou_demonstracao,"P&D")~0,
                                                str_detect(p_d_ou_demonstracao,"Demonstração")~1,
                                                TRUE ~ 9)
  )

  names(cnen)=str_replace_all(names(cnen),"gasto_2","valor_executado_2")

  vars=c("id","fonte_de_dados","data_assinatura","data_limite","duracao_dias",
         "titulo_projeto","status_projeto","valor_contratado","valor_executado",
         "nome_agente_financiador","natureza_agente_financiador","modalidade_financiamento",
         "nome_agente_executor","natureza_agente_executor","uf_ag_executor","p&d_ou_demonstracao",
         "regiao_ag_executor","natureza_agente_executor","natureza_financiamento",
         "modalidade_financiamento",names(cnen)[str_detect(names(cnen),"valor_executado_")],
         "categorias")

  cnen<-cnen %>%
    dplyr::mutate(regiao_ag_executor = dplyr::recode(uf_ag_executor,
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
                                       "TO" = "N")) %>%
    dplyr::select(vars) %>%
    tibble()

    cnen
}
