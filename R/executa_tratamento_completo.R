#' executa o tratamento que gera um dataset para a carga completa do sqlite
#'
#' @param df dataset tratado
#' @param sqlite diretório do .db do sqlite
#'
#' @return
#' @export
#'
#' @examples
#' executa_tratamento_completo(df_intermediario_aneel, diretorio_sqlite)
#' executa_tratamento_completo(df_intermediario_anp, diretorio_sqlite)
#' executa_tratamento_completo(df_intermediario_bndes, diretorio_sqlite)
#' executa_tratamento_completo(df_intermediario_cnen, diretorio_sqlite)
executa_tratamento_completo <- function(df, sqlite){

  data <- {{df}}
  fonte <- {{sqlite}}

  con <- DBI::dbConnect(RSQLite::SQLite(),
                        ":memory:",
                        dbname = fonte)
  tbl_dm_categoria <- DBI::dbReadTable(con,"dm_categoria")
  tbl_dm_fmt <- DBI::dbReadTable(con,"dm_formentador")
  tbl_dm_projeto <- DBI::dbReadTable(con,"dm_projeto")
  tbl_ft_dispendio <- DBI::dbReadTable(con,"ft_dispendio")


  fonte <- dplyr::case_when(data$fonte_de_dados[1] == "ANEEL" ~ 5,
                     data$fonte_de_dados[1] == "BNDES" ~ 6,
                     data$fonte_de_dados[1] == "FINEP" ~ 7,
                     data$fonte_de_dados[1] == "FNDCT" ~ 8,
                     data$fonte_de_dados[1] == "CNPQ" ~ 9,
                     data$fonte_de_dados[1] == "FAPESP" ~ 10,
                     data$fonte_de_dados[1] == "ANP" ~ 11,
                     data$fonte_de_dados[1] == "CNEN" ~ 12)

  #inserir categorias IEA do sqlite na base

  #criando um objeto com informações sobre as categorias de IEA
  consulta <- dplyr::select(tbl_ft_dispendio, id_item, id_cat2, id_formnt)

  #puxando as descrições das categorias
  consulta <- dplyr::left_join(consulta, tbl_dm_categoria[,c("id","cat2")],
                               by = c("id_cat2" = "id"))
  #trazendo as informações de fomentador
  consulta <- dplyr::left_join(consulta, tbl_dm_fmt[,c("id_formentador","nme_form")],
                               by = c("id_formnt"= "id_formentador"))

  #trazendo as informações de titulo
  consulta <- dplyr::left_join(consulta, tbl_dm_projeto[,c("título", "id_item")])

  consulta <- consulta %>% dplyr::mutate(título = stringr::str_trim(título),
                                   título = abjutils::rm_accent(título)) %>%
    dplyr::filter(id_formnt == fonte)

  #fazendo o merge
  vetor_remocao <- dplyr::case_when(
    df$fonte_de_dados[1] == "ANEEL" ~ "ANEEL-",
    df$fonte_de_dados[1] == "BNDES" ~ "BNDES-",
    df$fonte_de_dados[1] == "FINEP" ~ "FINEP-",
    df$fonte_de_dados[1] == "FNDCT" ~ "FNDCT-",
    df$fonte_de_dados[1] == "CNPQ" ~ "CNPQ-",
    df$fonte_de_dados[1] == "FAPESP" ~ "FAPESP-",
    df$fonte_de_dados[1] == "ANP" ~ "ANP-",
    df$fonte_de_dados[1] == "CNEN" ~ "CNEN-"
  )
   data <- data %>%
     dplyr::mutate(titulo_projeto2 = abjutils::rm_accent(titulo_projeto),
                   id = stringr::str_remove_all(id, vetor_remocao))

  data <- dplyr::left_join(data, consulta[,c ("título", "cat2") ],
                           by = c("titulo_projeto2"= "título"))  %>%
    unique() %>%
    dplyr::rename(categoria_sqlite = cat2) %>%
    dplyr::select(-titulo_projeto2)


  # criar uma coluna que diz se o caso já existe no sqlite
  #
  # data <- dplyr::left_join(data, consulta, by = c("id"= "id_item")) %>% unique() %>%
  #   select(-cat2.x, -id_cat2, -id_formnt, -cat2.y,
  #          -nme_form, -título)

  data
}
