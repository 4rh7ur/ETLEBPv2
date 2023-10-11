#' Cria Base Intermediária da Fapesp
#'
#' @param origem_processos dataset que contém os projetos da fonte fapesp
#'
#' @return
#' @export
#'
#' @examples
#' cria_base_intermediaria_fapesp()
cria_base_intermediaria_fapesp <- function(origem_processos = here::here("data/FAPESP/auxilios-bolsas-vigentes-2021-e-2022-assuntos-solicitados-valores-concedidos.xlsx")){
#Termos de área -----------------------------------
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

#-----------------

  fapesp2 <- readxl::read_excel(origem_processos) %>%
    janitor::clean_names() %>%
    dplyr::mutate(n_processo = paste("FAPESP", n_processo, sep = "-"),
                  data_inicio = lubridate::ymd(data_de_inicio),
                  data_termino = lubridate::ymd(data_de_termino),
                  duracao_dias = lubridate::time_length(data_termino- data_inicio, "days"),
                  motor = stringi::stri_trans_general(paste(titulo_portugues),
                                                      "Latin-ASCII"),
                  motor = tolower(motor)) %>%
    dplyr::filter(!area_do_conhecimento %in% termos_a,
                  !subarea_do_conhecimento %in% termos_sa) %>%
    tidyr::drop_na(valor_concedido)

  fapesp2 <- fapesp2 %>%
    #dplyr::slice(1:7488) %>%
    func_a(n_processo, data_inicio,
           data_termino,valor_concedido)

  fapesp2 <- fapesp2 %>% dtc_categorias(n_processo,motor)

  fapesp2 <- fapesp2 %>% dplyr::mutate(categorias = dplyr::recode(categorias,
                                                                  "character(0" = "nenhuma categoria encontrada"))

  fapesp2 <- fapesp2 %>%
    dplyr::mutate(
      id                          = n_processo,
      fonte_de_dados              = "FAPESP",
      data_assinatura             = data_inicio,
      data_limite                 = data_termino,
      duracao_dias                = duracao_dias,
      valor_contratado            = valor_concedido,
      valor_executado             = gasto_executado,
      nome_agente_financiador     = "FAPESP",
      natureza_agente_financiador = "Empresa Pública", # confirmar
      natureza_financiamento    = "Público",
      modalidade_financiamento    = NA,
      nome_agente_executor        = beneficiario,
      natureza_agente_executor    = "Instituição Pública", # confirmar
      'p&d_ou_demonstracao'       = 0,
      titulo_projeto              = titulo_portugues,
      status_projeto              = NA,
      uf_ag_executor              = "SP",
      regiao_ag_executor          = "SUDESTE")

  names(fapesp2)=str_replace_all(names(fapesp2),"gasto_2","valor_executado_2")

  vars=c("id","fonte_de_dados","data_assinatura","data_limite","duracao_dias",
         "titulo_projeto","status_projeto","valor_contratado","valor_executado",
         "nome_agente_financiador","natureza_agente_financiador","modalidade_financiamento",
         "nome_agente_executor","natureza_agente_executor","uf_ag_executor",
         "regiao_ag_executor","natureza_financiamento","p&d_ou_demonstracao",
         names(fapesp2)[str_detect(names(fapesp2),"valor_executado_")],"motor","categorias")


  fapesp2 <- fapesp2 %>%
    dplyr::select(vars)


  fapesp2 <- dplyr::tibble(fapesp2)

  fapesp2
}
