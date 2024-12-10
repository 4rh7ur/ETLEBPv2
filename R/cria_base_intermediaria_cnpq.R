#' Cria Base Intermediária CNPQ
#'
#' @param origem_processos1 dataset com os projetos de bolsas no exterior de 2004 a 2020
#' @param origem_processos2 dataset com os projetos de bolsas no pais de 2004 a 2020
#' @param origem_processos3 dataset com projetos de fomento de 2010 a 2021
#' @return
#' @export
#'
#' @examples
#' cria_base_intermediaria_cnpq()
cria_base_intermediaria_cnpq<- function(origem_processos
    # origem_processos = here::here("data/CNPQ/CNPQ_primario_2021.xlsx")
    ){




#-------------------------- termos filtrados --------------------------
  termos_a<- c("Botânica","Ciência e Tecnologia de Alimentos",
               "Direito", "Ecologia", "Educação Física", "Enfermagem",
               "Farmacologia", "Fisiologia", "Fisioterapia", "Fonoaudiologia",
               "Genética", "Imunologia", "Medicina", "Medicina Veterinária",
               "Museologia", "Oceoanografia", "Parasitologia", "Saude Coletiva",
               "Serviço Social", "Turismo", "Zoologia", "Zootecnia", "Letras",
               "Lingüística", "Artes", "Educação", "Filosofia", "Geografia","História",
               "Psicologia", "Sociologia", "Teologia", "Administração", "Comunicação",
               "Desenho Industrial", "Ciência da Informação", "Demografia",
               "Economia Doméstica","Recursos Pesqueiros e Engenharia de Pesca",
               "Morfologia", "Fisiologia", "Farmacologia",
               "Desenho de Moda", "Decoração", "Carreira Religiosa",
               "Carreira Militar", "Diplomacia", "Farmácia", "Odontologia", "Relações Públicas" )


  termos_sa<- c("Administração de Empresas","Administração de Setores Específicos",
                "Administração Pública", "Análise Nutricional de População",
                "Arquivologia","Biblioteconomia","Bioquímica da Nutrição",
                "Ciência do Solo", "Ciências Contábeis", "Comunicação Visual",
                "Conservação da Natureza", "Desnutrição e Desenvolvimento Fisiológico",
                "Dietética", "Engenharia Médica", "Enzimologia", "Estruturas",
                "Estruturas Aeroespaciais", "Estruturas Navais", "Fitossanidade",
                "Fitotecnia", "Floricultura", "Geodésia", "Geometria e Topologia",
                "Lavra", "Manejo Florestal", "Metabolismo e Bioenergética",
                "Meteorologia", "Métodos e Técnicas do Planejamento Urbano e Regional",
                "Microbiologia Aplicada", "Paisagismo", "Projeto de Arquitetura e Urbanismo",
                "Relações Públicas e Propaganda","Saneamento Ambiental", "Saneamento Básico",
                "Técnicas e Operações Florestais")

#---------- Inicio do Tratamento ---------------------------------------------


  cnpq<-origem_processos
    # readxl::read_excel(path = origem_processos,sheet = 1, skip = 6) %>%
    # janitor::clean_names()


  cnpq <- cnpq %>%
  dplyr::filter(!area %in% termos_a,
                !subarea %in% termos_sa)  %>%
  dplyr::mutate(
    inicio_processo = lubridate::as_date(paste0(ano_referencia,"01-01")),
    termino_processo = lubridate::as_date(paste0(ano_referencia,"12-31")),
    prazo_dias = lubridate::time_length(termino_processo - inicio_processo, "days"),
    motor = stringi::stri_trans_general(paste(titulo_do_projeto, area, subarea),
                                        "Latin-ASCII"),
    categoria_nivel = NA,
    sigla_uf_destino = NA,
    regiao_destino = NA) %>%
  dplyr::relocate(categoria_nivel, .before = grande_area) %>%
  dplyr::relocate(sigla_uf_destino, regiao_destino, .before = pais_destino) %>%
    filter(ano_referencia >=2013)



  cnpq_categorizado<- cnpq %>%
    dplyr::mutate(
      inicio_processo = dplyr::case_when(
        is.na(inicio_processo) ~ lubridate::make_date(2013, 1, 1),
        TRUE ~ inicio_processo
      ),
      termino_processo = dplyr::case_when(is.na(termino_processo) ~ lubridate::make_date(max(ano_referencia), 12, 31),
                                          TRUE~ termino_processo),
      prazo_dias = lubridate::time_length(termino_processo - inicio_processo, "days"),
      processo2  = paste(processo, 1:nrow(cnpq))
    ) %>%
    func_a(processo2, inicio_processo,
           termino_processo, valor_pago) %>% dtc_categorias(processo2, motor) %>%
    dplyr::mutate(categorias = dplyr::recode(categorias,
                                             "character(0" = "nenhuma categoria encontrada"))


  cnpq_categorizado <- cnpq_categorizado %>%
    dplyr::mutate(
      regiao_ag_executor = dplyr::recode(
        sigla_uf_origem,
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
      )
    )

  cnpq_categorizado <- cnpq_categorizado %>%
    mutate(
      id                           = paste("CNPQ",
                                           processo, sep = "-"),
      fonte_de_dados                 = "CNPQ",
      data_assinatura                    = inicio_processo,
      data_limite                    = termino_processo,
      duracao_dias                   = prazo_dias,
      #duracao_meses                  = periodo_meses,
      #duracao_anos                   = periodo_anos,
      valor_contratado               = valor_pago,
      titulo_projeto                 = titulo_do_projeto,
      status_projeto                 = NA,
      nome_agente_financiador        = "CNPQ",
      natureza_agente_financiador    = "Empresa Pública",
      natureza_financiamento         = "pública",
      modalidade_financiamento       = "não reembolsável",
      nome_agente_executor           = instituicao_destino,
      natureza_agente_executor       = NA,
      #confirmar natureza juridica proponente
      'p&d_ou_demonstracao'          = 0,
      uf_ag_executor                 = sigla_uf_destino,
      valor_executado                = gasto_executado
    )

  names(cnpq_categorizado)=str_replace_all(names(cnpq_categorizado),"gasto_2","valor_executado_2")

  vars=c("id","fonte_de_dados","data_assinatura","data_limite","duracao_dias",
         "titulo_projeto","status_projeto","valor_contratado","valor_executado",
         "nome_agente_financiador","natureza_financiamento","natureza_agente_financiador",
         "modalidade_financiamento","nome_agente_executor","natureza_agente_executor",
         "uf_ag_executor","regiao_ag_executor","uf_ag_executor","p&d_ou_demonstracao",
         names(cnpq_categorizado)[str_detect(names(cnpq_categorizado),"valor_executado_")],"motor","categorias")

  cnpq_categorizado <- cnpq_categorizado %>%
    dplyr::select(vars)

  cnpq_categorizado
}
