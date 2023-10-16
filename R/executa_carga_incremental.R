#' executa carga incremental no SQLite, adicionando apenas os casos que não foram identificados no SQlite e atualizando os atributos
#'
#' @param df dataset validado
#' @param sqlite diretório do sqlite
#'
#' @return
#' @export
#'
#' @examples
#' executa_carga_incremental(df_validado_aneel, diretorio_sqlite)
#' executa_carga_incremental(df_validado_anp, diretorio_sqlite)
#' executa_carga_incremental(df_validado_bndes, diretorio_sqlite)
#' executa_carga_incremental(df_validado_cnen, diretorio_sqlite)
executa_carga_incremental <- function(df, sqlite){
  data <- {{df}}
  fonte <- {{sqlite}}

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        ":memory:",
                        dbname = fonte)

## Etapa de Carga na tabela dm_agente_empresa ----------------------------------
#

  tbl_agente_empresa <- DBI::dbReadTable(con,"dm_agente_empresa")
  tbl_dm_categoria <- DBI::dbReadTable(con,"dm_categoria")
  tbl_dm_fmt <- DBI::dbReadTable(con,"dm_formentador")
  tbl_mod_finan <- DBI::dbReadTable(con,"dm_mod_finan")
  tbl_nat_disp <- DBI::dbReadTable(con,"dm_nat_disp")
  tbl_dm_projeto <- DBI::dbReadTable(con,"dm_projeto")
  tbl_ft_dispendio <- DBI::dbReadTable(con,"ft_dispendio")

  dm_agente_empresa <- data %>%
    dplyr::filter(categorias %in% c(1.1, 1.2, 1.3, 1.4, 1.9,
                                    2.1, 2.2, 2.3, 2.9,
                                    3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.9,
                                    4.1, 4.2, 4.9,
                                    5.1, 5.2, 5.9,
                                    6.1, 6.2, 6.3, 6.9,
                                    7.1, 7.2, 7.3),
      !id %in% tbl_dm_projeto$id_item)  %>%

    dplyr::select(
      nome_agente_executor,
      nome_agente_financiador,
      uf_ag_executor,
      fonte_de_dados,
      natureza_agente_executor) %>% unique()



  dm_agente_empresa <- dm_agente_empresa %>%
    dplyr::mutate(

      nme_agente = nome_agente_executor,
      uf = uf_ag_executor,
      municipio = NA,
      cnpj = NA,
      ntz_agente = natureza_agente_executor
    ) %>%
    dplyr::select(nme_agente,
                  ntz_agente,
                  uf,
                  municipio,
                  cnpj) %>% unique()

  inicio<-(max(tbl_agente_empresa$id_agente)+1)

  fim<-(inicio+nrow(dm_agente_empresa)-1)

  dm_agente_empresa <- dm_agente_empresa %>%
    dplyr::mutate(id_agente = inicio:fim,)

  DBI::dbExecute(con, 'INSERT INTO dm_agente_empresa (id_agente, nme_agente,ntz_agente, uf, municipio, cnpj)
          VALUES (:id_agente, :nme_agente, :ntz_agente, :uf, :municipio, :cnpj);', dm_agente_empresa)

## Etapa de carga na tabela dm_projeto -----------------------------------------
#
  dm_projeto <- data %>%
    dplyr::filter(categorias %in% c(1.1, 1.2, 1.3, 1.4, 1.9,
                                    2.1, 2.2, 2.3, 2.9,
                                    3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.9,
                                    4.1, 4.2, 4.9,
                                    5.1, 5.2, 5.9,
                                    6.1, 6.2, 6.3, 6.9,
                                    7.1, 7.2, 7.3) ,
      !abjutils::rm_accent(titulo_projeto) %in% abjutils::rm_accent(
        stringr::str_squish(tbl_dm_projeto$título)
      )
    )  %>%
    dplyr::select(
      id, data_assinatura,
      data_limite,titulo_projeto,status_projeto)


  inicio<-(max(tbl_dm_projeto$id_projeto)+1)

  fim<-(inicio+nrow(dm_projeto)-1)

  dm_projeto <- dm_projeto %>%
    dplyr::mutate(
      id_projeto = inicio:fim,
      id_item = id,
      dta_inicio = as.character(data_assinatura),
      dta_limite = as.character(data_limite),
      'título'     = titulo_projeto,
      'situação'   = status_projeto) %>%
    dplyr::select(
      id_projeto,id_item,
      dta_inicio,dta_limite,'título',
      'situação')


  DBI::dbExecute(con, 'INSERT INTO dm_projeto (id_projeto, id_item,dta_inicio, dta_limite, título, situação)
          VALUES (:id_projeto, :id_item, :dta_inicio, :dta_limite, :título, :situação);', dm_projeto)

## Atualizando o Status dos Projetos -------------------------------------------
#

  DBI::dbExecute(con, "UPDATE dm_projeto SET situação = :situação where id_item = :id_item",
            params=data.frame(situação = data$status_projeto,
                              id_item = data$id))

## Executando a Carga na tabela ft_dispendio -----------------------------------
#

  vlr_res <- data %>%
    dplyr::filter(categorias %in% c(1.1, 1.2, 1.3, 1.4, 1.9,
                                    2.1, 2.2, 2.3, 2.9,
                                    3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.9,
                                    4.1, 4.2, 4.9,
                                    5.1, 5.2, 5.9,
                                    6.1, 6.2, 6.3, 6.9,
                                    7.1, 7.2, 7.3) ,
      !abjutils::rm_accent(titulo_projeto) %in% abjutils::rm_accent(
        stringr::str_squish(tbl_dm_projeto$título)
        )
      ) %>%
    dplyr::select(id, valor_executado_2013:valor_executado_2020) %>%
    tidyr::gather(ano, vlr, -id) %>%
    dplyr::mutate(
      ano = dplyr::recode(ano,
                          "valor_executado_2013" = 2013,
                          "valor_executado_2014" = 2014,
                          "valor_executado_2015" = 2015,
                          "valor_executado_2016" = 2016,
                          "valor_executado_2017" = 2017,
                          "valor_executado_2018" = 2018,
                          "valor_executado_2019" = 2019,
                          "valor_executado_2020" = 2020)) %>%
    dplyr::rename(id_item = id)

  bs_res <- data %>%
    dplyr::filter(categorias %in% c(1.1, 1.2, 1.3, 1.4, 1.9,
                                    2.1, 2.2, 2.3, 2.9,
                                    3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.9,
                                    4.1, 4.2, 4.9,
                                    5.1, 5.2, 5.9,
                                    6.1, 6.2, 6.3, 6.9,
                                    7.1, 7.2, 7.3)  ,
      !abjutils::rm_accent(titulo_projeto) %in% abjutils::rm_accent(
        stringr::str_squish(tbl_dm_projeto$título)
      )
    ) %>%
    dplyr::mutate(categorias = as.character(categorias)) %>%
    dplyr::select(id, natureza_agente_financiador,
                  data_assinatura,categorias,nome_agente_executor,
                  fonte_de_dados, modalidade_financiamento)

  outra<- bs_res %>% dplyr::select(nome_agente_executor) %>%
    na.omit(nome_agente_executor) %>% unique()

  outra<- dplyr::left_join(outra, tbl_agente_empresa[,c(1,2)],
                           by = c("nome_agente_executor"="nme_agente")) %>%
    dplyr::rename(id_exec = id_agente) %>% unique()

  bs_res <- dplyr::left_join(bs_res, outra) %>% unique()


  bs_res <- dplyr::left_join(bs_res, tbl_dm_categoria[,c(1,3)],
                             by =  c("categorias" = "cat2")) %>%
    dplyr::rename(id_item = id.x,
                  id_cat2 = id.y,
                  dta_inicio = data_assinatura) %>% unique()


  inicio <- (max(tbl_ft_dispendio$id_disp)+1)

  fim <- (inicio+nrow(bs_res)-1)

  bs_res <- bs_res %>%
    dplyr::mutate(
      fonte_de_dados = dplyr::recode(fonte_de_dados,
                                     "ANEEL"  = 5,
                                     "BNDES"  = 6,
                                     "FINEP"  = 7,
                                     "Finep"  = 7,
                                     "FNDCT"  = 8,
                                     "CNPq"   = 9,
                                     "CNPQ"   = 9,
                                     "FAPESP" = 10,
                                     "ANP"    = 11,
                                     "CNEN"   = 12),
      modalidade_financiamento = as.character(modalidade_financiamento),
      modalidade_financiamento = dplyr::recode(modalidade_financiamento,
        "Reembolsável"  = 1,
        "Não-reembolsável" = 2,
        "NÃO REEMBOLSÁVEL" = 2,
        "Não Reembolsável" = 2,
        "Não-reembolsavel (acordo de parceria)"=2,
        "Não-reembolsável (acordo de parceria)" =2,
        "Não-reembolsável (acordo)" =2,
        "Não-reembolsável (contrato)" =2,
        "Não-reembolsavel (convênio)" =2,
        "Não-reembolsável (termo de execução descentralizada -TED)" =2,
        "Não-reembolsável (termo de outorga)" =2,
        "Não-reembolsável (convênio)" =2,
        "não-reembosável (contrato de pesquisa)"=2,
        "não-reembosável (termo de outorga)"=2,
        "não reembolsável" =2,
        "não-reembolsavel" =2,
        "Orçamento próprio" = 4,
        "REEMBOLSÁVEL" = 1,
        "Subvenção" =3,
        "Não se Aplica" = 4,
        "Não informado" =5),
      natureza_agente_financiador = dplyr::recode(natureza_agente_financiador,
        "Empresa Privada" = 0,
        "Empresa privada" = 0,
        "empresa pública" = 1,
        "Empresa pública" = 1,
        "Empresa Pública" = 1,
        "Ministério / Governo Federal"=1,
        "Ministério/Governo Federal" =1,
        "Empresa economia mista" =1,
        "Fundação de Amparo (FAP)" = 1,
        "ICT pública" =1,
        "ONU" =0),
      chamada = NA,
      id_disp = inicio:fim
    ) %>%
    dplyr::rename(id_formnt = fonte_de_dados,
                  mod_finan = modalidade_financiamento,
                  ntz_finan = natureza_agente_financiador)

  #id_prop e id_finan e id_exec medem a mesma coisa
  bs_res <- dplyr::left_join(vlr_res, bs_res ) %>% unique()

  bs_res <- bs_res %>% dplyr::select(-nome_agente_executor,-categorias) %>%
    dplyr::mutate(dta_inicio = NA,
                  vlr = as.numeric(vlr),
                  ano = as.integer(ano),
                  ntz_finan = as.integer(ntz_finan))

  DBI::dbExecute(con, 'INSERT INTO ft_dispendio (id_item, ano, vlr, ntz_finan, dta_inicio,
                                          id_exec, id_formnt, mod_finan, id_cat2,chamada, id_disp)
          VALUES (:id_item, :ano, :vlr, :ntz_finan, :dta_inicio,
                  :id_exec, :id_formnt, :mod_finan, :id_cat2, :chamada, :id_disp);', bs_res)
}
