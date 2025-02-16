#' executa a carga completa no SQLite, removendo todos os casos da fonte inserida e inserindo todos os casos do dataset
#'
#' @param df dataset tratado e validado
#' @param sqlite diretório do sqlite
#'
#' @return
#' @export
#'
#' @examples
#' executa_carga_completa(df_validado_aneel, diretorio_sqlite)
#' executa_carga_completa(df_validado_anp, diretorio_sqlite)
#' executa_carga_completa(df_validado_bndes, diretorio_sqlite)
#' executa_carga_completa(df_validado_cnen, diretorio_sqlite)
#'
executa_carga_completa <- function(df, sqlite){

  data <- {{df}}
  fonte <- {{sqlite}}

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        ":memory:",
                        dbname = fonte)
# Etapa da Carga na tabela dm_agente_empresa -----------------------------
#

  mytbl1 <- DBI::dbReadTable(con,"dm_agente_empresa")
  tbl_dm_agente_empresa <- DBI::dbReadTable(con,"dm_agente_empresa")
  mytbl6 <- DBI::dbReadTable(con,"dm_projeto")
  mytbl7 <- DBI::dbReadTable(con,"ft_dispendio")
  tbl_ft_dispendio <- DBI::dbReadTable(con,"ft_dispendio")

  # remove projetos anteriores da tabela dm_agente_empresa

  tbl_res <- tbl_ft_dispendio %>%
    select(id_exec, id_formnt) %>%
    left_join(tbl_dm_agente_empresa,
              by = c("id_exec" = "id_agente")) %>% unique()

  fmt <- dplyr::case_when(
    data$fonte_de_dados[1] == "ANEEL" ~ 5,
    data$fonte_de_dados[1] == "BNDES" ~ 6,
    data$fonte_de_dados[1] == "FINEP" ~ 7,
    data$fonte_de_dados[1] == "FNDCT" ~ 8,
    data$fonte_de_dados[1] == "CNPq" ~ 9,
    data$fonte_de_dados[1] == "CNPQ" ~ 9,
    data$fonte_de_dados[1] == "FAPESP" ~ 10,
    data$fonte_de_dados[1] == "ANP" ~ 11,
    data$fonte_de_dados[1] == "CNEM" ~ 12)

  obj <- tbl_res %>% filter(id_formnt == fmt) %>% select(id_exec) %>%
    mutate(id_item = stringr::str_glue("'{id_exec}'"))

  ex <- obj %>% pull(id_item)

  ex <-  paste0(ex, collapse = ", ")

  txt <- sprintf("DELETE FROM dm_agente_empresa WHERE id_agente IN (%s)",
                 ex)

  RSQLite::dbExecute(con, txt)


  dm_agente_empresa <- data %>%
    dplyr::filter(categorias %in% c(1.1, 1.2, 1.3, 1.4, 1.9,
                                    2.1, 2.2, 2.3, 2.9,
                                    3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.9,
                                    4.1, 4.2, 4.9,
                                    5.1, 5.2, 5.9,
                                    6.1, 6.2, 6.3, 6.9,
                                    7.1, 7.2, 7.3) ) %>%

    dplyr::select(nome_agente_executor,
                  nome_agente_financiador,
                  uf_ag_executor,
                  fonte_de_dados,
                  natureza_agente_executor) %>% unique()



  dm_agente_empresa <- dm_agente_empresa %>%
    dplyr::mutate(nme_agente = nome_agente_executor,
      uf = uf_ag_executor,
      municipio = NA,
      cnpj = NA,
      ntz_agente = natureza_agente_executor
    ) %>%
    dplyr::select(nme_agente,
                  ntz_agente,
                  uf,
                  municipio,
                  cnpj)%>% unique()

  inicio<-(max(mytbl1$id_agente)+1)

  fim<-(inicio+nrow(dm_agente_empresa)-1)

  dm_agente_empresa <- dm_agente_empresa %>%
    dplyr::mutate(id_agente = inicio:fim)

  DBI::dbExecute(con, 'INSERT INTO dm_agente_empresa (id_agente, nme_agente,ntz_agente, uf, municipio, cnpj)
          VALUES (:id_agente, :nme_agente, :ntz_agente, :uf, :municipio, :cnpj);', dm_agente_empresa)

# Etapa da carga na tabela dm_projeto ---------------------------------------
#
   mytbl6 <- DBI::dbReadTable(con,"dm_projeto")
   mytbl7 <- DBI::dbReadTable(con,"ft_dispendio")

  #Removendo projetos antigos da tabela dm_projeto
  tbl_res <- mytbl7 %>%
    select(id_item, id_formnt) %>%
    left_join(mytbl6,
              by = c("id_item" = "id_item")) %>%
    unique()

  fmt <- dplyr::case_when(
    data$fonte_de_dados[1] == "ANEEL" ~ 5,
    data$fonte_de_dados[1] == "BNDES" ~ 6,
    data$fonte_de_dados[1] == "FINEP" ~ 7,
    data$fonte_de_dados[1] == "FNDCT" ~ 8,
    data$fonte_de_dados[1] == "CNPq" ~ 9,
    data$fonte_de_dados[1] == "CNPQ" ~ 9,
    data$fonte_de_dados[1] == "FAPESP" ~ 10,
    data$fonte_de_dados[1] == "ANP" ~ 11,
    data$fonte_de_dados[1] == "CNEM" ~ 12)

  obj <- tbl_res %>% filter(id_formnt == fmt) %>% select(id_item) %>%
    mutate(id_item = stringr::str_glue("'{id_item}'"))

  ex <- obj %>% pull(id_item)

  ex <-  paste0(ex, collapse = ", ")

  txt <- sprintf("DELETE FROM dm_projeto WHERE id_item IN (%s)",
                 ex)

  RSQLite::dbExecute(con, txt)

   dm_projeto <- data %>%
     dplyr::filter(categorias %in% c(1.1, 1.2, 1.3, 1.4, 1.9,
                                     2.1, 2.2, 2.3, 2.9,
                                     3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.9,
                                     4.1, 4.2, 4.9,
                                     5.1, 5.2, 5.9,
                                     6.1, 6.2, 6.3, 6.9,
                                     7.1, 7.2, 7.3) ) %>%
    dplyr::select(id, data_assinatura,
                  data_limite,titulo_projeto,status_projeto,uf_ag_executor)


  inicio<-(max(mytbl6$id_projeto)+1)

  fim<-(inicio+nrow(dm_projeto)-1)

  dm_projeto <- dm_projeto %>%
    dplyr::mutate(id_projeto = inicio:fim,
                  id_item = id,
                  dta_inicio = as.character(data_assinatura),
                  dta_limite = as.character(data_limite),
                  'título'     = titulo_projeto,
                  'situação'   = status_projeto) %>%
    dplyr::select(id_projeto,id_item,
                  dta_inicio,dta_limite,'título',
                  'situação')


  DBI::dbExecute(con, 'INSERT INTO dm_projeto (id_projeto, id_item,dta_inicio, dta_limite, título, situação)
          VALUES (:id_projeto, :id_item, :dta_inicio, :dta_limite, :título, :situação);', dm_projeto)

## Etapa que executa a carga na tabela ft_dispendio -------------------------
#

  mytbl1 <- DBI::dbReadTable(con,"dm_agente_empresa")
  mytbl2 <- DBI::dbReadTable(con,"dm_categoria")
  mytbl3 <- DBI::dbReadTable(con,"dm_formentador")
  mytbl4 <- DBI::dbReadTable(con,"dm_mod_finan")
  mytbl5 <- DBI::dbReadTable(con,"dm_nat_disp")
  mytbl6 <- DBI::dbReadTable(con,"dm_projeto")
  mytbl7 <- DBI::dbReadTable(con,"ft_dispendio")

# Removendo casos existentes da fonte

  fmt <- dplyr::case_when(
    data$fonte_de_dados[1] == "ANEEL" ~ 5,
    data$fonte_de_dados[1] == "BNDES" ~ 6,
    data$fonte_de_dados[1] == "FINEP" ~ 7,
    data$fonte_de_dados[1] == "FNDCT" ~ 8,
    data$fonte_de_dados[1] == "CNPq" ~ 9,
    data$fonte_de_dados[1] == "CNPQ" ~ 9,
    data$fonte_de_dados[1] == "FAPESP" ~ 10,
    data$fonte_de_dados[1] == "ANP" ~ 11,
    data$fonte_de_dados[1] == "CNEN" ~ 12)

  txt <-sprintf(
    "DELETE FROM ft_dispendio WHERE id_formnt = %s",
    fmt)
  RSQLite::dbExecute(con, txt)

  vlr_res <- data %>%
    dplyr::filter(categorias %in% c(1.1, 1.2, 1.3, 1.4, 1.9,
                                    2.1, 2.2, 2.3, 2.9,
                                    3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.9,
                                    4.1, 4.2, 4.9,
                                    5.1, 5.2, 5.9,
                                    6.1, 6.2, 6.3, 6.9,
                                    7.1, 7.2, 7.3) ) %>%
    dplyr::select(id, starts_with("valor_executado_")) %>%
    tidyr::gather(ano, vlr, -id) %>%
    dplyr::mutate(ano = as.numeric(str_replace_all(ano,"valor_executado_",""))) %>%
    dplyr::rename(id_item = id)


  bs_res <- data %>%
    dplyr::filter(categorias %in% c(1.1, 1.2, 1.3, 1.4, 1.9,
                                    2.1, 2.2, 2.3, 2.9,
                                    3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.9,
                                    4.1, 4.2, 4.9,
                                    5.1, 5.2, 5.9,
                                    6.1, 6.2, 6.3, 6.9,
                                    7.1, 7.2, 7.3) ) %>%
    dplyr::mutate(categorias = as.character(categorias)) %>%
    dplyr::select(id, natureza_agente_financiador,uf_ag_executor,
                  data_assinatura,categorias,nome_agente_executor,
                  fonte_de_dados, modalidade_financiamento)

  outra<- bs_res %>% dplyr::select(nome_agente_executor,uf_ag_executor) %>%
    na.omit(nome_agente_executor)

  outra<- dplyr::left_join(outra, mytbl1[,c(1,2,4)],
                           by = c("nome_agente_executor"="nme_agente","uf_ag_executor"="uf")) %>%
    dplyr::rename(id_exec = id_agente) %>%
    unique()

  bs_res <- dplyr::left_join(bs_res, outra,by=c("nome_agente_executor","uf_ag_executor")) %>%
    unique() %>%
    filter(!duplicated(id))


  bs_res <- dplyr::left_join(bs_res, mytbl2[,c(1,3)],
                             by =  c("categorias" = "cat2")) %>%
    dplyr::rename(id_item = id.x,
                  id_cat2 = id.y,
                  dta_inicio = data_assinatura) %>%
    unique()


  inicio<-(max(mytbl7$id_disp)+1)

  fim<-(inicio+nrow(bs_res)-1)

  bs_res<- bs_res %>%
    dplyr::mutate(
    fonte_de_dados = dplyr::recode(fonte_de_dados,
    "ANEEL"  = 5,
    "BNDES"  = 6,
    "FINEP"  = 7,
    "Finep"  = 7,
    "FNDCT"  = 8,
    "CNPq"   = 9,
    "CNPQ"   = 9,
    "CNPQ"   = 9,
    "FAPESP" = 10,
    "ANP"    = 11,
    "CNEN"   = 12),
    modalidade_financiamento = as.character(modalidade_financiamento),
    modalidade_financiamento = dplyr::recode(modalidade_financiamento,
    "Reembolsável"  = 1,
    "REEMBOLSÁVEL" = 1,
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
    "Subvenção" = 3,
    "Não se Aplica" = 4,
    "Não informado" = 5),
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
    dplyr::rename(
      id_formnt = fonte_de_dados,
      mod_finan = modalidade_financiamento,
      ntz_finan = natureza_agente_financiador)

  #id_prop e id_finan e id_exec medem a mesma coisa
  bs_res<-dplyr::left_join(vlr_res, bs_res ) %>% unique()

  bs_res<-bs_res %>% dplyr::select(-nome_agente_executor,-categorias,-uf_ag_executor) %>%
    dplyr::mutate(dta_inicio = NA,
                  vlr = as.numeric(vlr),
                  ano = as.integer(ano),
                  ntz_finan = as.integer(ntz_finan))

  DBI::dbExecute(con, 'INSERT INTO ft_dispendio (id_item, ano, vlr, ntz_finan, dta_inicio,
                                          id_exec, id_formnt, mod_finan, id_cat2,chamada, id_disp)
          VALUES (:id_item, :ano, :vlr, :ntz_finan, :dta_inicio,
                  :id_exec, :id_formnt, :mod_finan, :id_cat2, :chamada, :id_disp);', bs_res)

  DBI::dbDisconnect(con)
}
