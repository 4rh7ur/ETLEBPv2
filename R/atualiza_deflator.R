pacman::p_load("dplyr","tidyr","lubridate","stringr","purrr","RSQLite")

deflator<-readxl::read_excel("/Users/silvanooliveira/Documents/Pessoal/CEPAL/ETLEBP/Validados/tabela_deflator.xlsx") %>%
  dplyr::select(ano,deflator) %>%
  filter(ano>=2011 & ano<=2023) %>%
  mutate(deflator=ifelse(is.na(deflator),100,deflator)) %>%
  mutate(ano=as.integer(ano))


fonte="/Users/silvanooliveira/Documents/Pessoal/CEPAL/ETLEBP/Validados/ebp_final.db"

con <- DBI::dbConnect(RSQLite::SQLite(),
                      ":memory:",
                      dbname = fonte)

tbl_deflator<-DBI::dbReadTable(con,"deflatores")

RSQLite::dbExecute(con, "DELETE FROM deflatores WHERE ano>=2011")

DBI::dbExecute(con, 'INSERT INTO deflatores (ano,deflator)
          VALUES (:ano, :deflator);', deflator)
