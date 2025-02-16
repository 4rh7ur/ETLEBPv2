library(testthat)
library(ETLEBP)

## Teste cria_base_intermediaria_aneel

test_that("testa a função cria_base_intermediaria_aneel() com um dataset gabarito",
          {
            df_test <- data.frame(
              id = as.character(0001),
              fonte_de_dados = "ANEEL",
              data_assinatura = lubridate::ymd("2013-01-01"),
              data_limite = lubridate::ymd("2020-12-31"),
              duracao_dias = 2921,
              titulo_projeto = "Dados Simulados para o teste automático",
              status_projeto = NA,
              valor_contratado = 100000,
              valor_executado_2013_2020 = 100000,
              nome_agente_financiador = "",
              natureza_agente_financiador = "",
              modalidade_financiamento = "",
              nome_agente_executor = "",
              natureza_agente_executor = "",
              uf_ag_executor = "",
              regiao_ag_executor = "",
              natureza_financiamento = NA,
              "p&d_ou_demonstracao" = "P&D",
              valor_executado_2013 = 100000,
              valor_executado_2014 = 100000,
              valor_executado_2015 = 100000,
              valor_executado_2016 = 100000,
              valor_executado_2017= 100000,
              valor_executado_2018 = 100000,
              valor_executado_2019 = 100000,
              valor_executado_2020 = 100000,
              motor = "projeto a é de origem b e tem objetivo c",
              categorias = ""
            )

            expect_match(cria_base_intermediaria_aneel() %>% str(),
                         df_test %>% str)
          })


