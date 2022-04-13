
# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(naniar)
library(caret)
library(dplyr)
library(tidytext)
library(jsonlite)
library(stringr)
library(writexl)
library(DBI)
library(odbc)
library(RPostgresSQL)
library(RODBC)


# carregando csv ----------------------------------------------------------
setwd('C:\\Users/Pichau/Desktop/Processamen_linguagem')

# df <-read.csv('Out_Dez_40.csv', encoding = 'UTF-8') #Dezembro
# df3 <-readxl::read_excel('output-40-out-e-Nov-22.xlsx') #outubro e novembro e janeiro
# df4 <- readxl::read_excel('portal transparencia FEV.xlsx')
# 
# df <- read_csv2('Agosto.csv')



# Import PostGres -------------------------------------------------------------


library(dbplyr)
psql <- DBI::dbDriver("PostgreSQL")
# con <- DBI::dbConnect(psql, 
#                       dbname = "postgres",
#                       host = "192.168.15.74", 
#                       port = 5432,
#                       user = "user", 
#                       password = 'PELX8215')


# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
  "PELX8215"
}
# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database


con <- dbConnect(odbc::odbc(), database = "postgres",password = pw, Uid = "postgres",
                 .connection_string = "Driver={PostgreSQL Unicode};", timeout = 10)



dbGetQuery(con, "SHOW CLIENT_ENCODING")


#Função para transformar tudo em UTF8

set_utf8 <- function(x) {
  # Declare UTF-8 encoding on all character columns:
  chr <- sapply(x, is.character)
  x[, chr] <- lapply(x[, chr, drop = FALSE], `Encoding<-`, "UTF8")
  # Same on column names:
  Encoding(names(x)) <- "UTF8"
  x
}


df_postgres<-set_utf8(dbGetQuery(con, "SELECT * FROM transparency_portal1"))



# +Favorecidos ------------------------------------------------------------


Favorecidos_v2<- c("\\bGRAPHO-PRODUTOS E SERVICOS EM COMPUTACAO LTDA\\b",
                   "\\bSOFTWAREONE COMERCIO E SERVICOS DE INFORMATICA LTDA\\b","\\bBRASOFTWARE INFORMATICA LTDA\\b",
                   "\\bMAPDATA-TECNOLOGIA,INFORMATICA E COMERCIO LTDA\\b",
                      "MAPDATA", "\\bTECHSYSTEMS INFORMATICA LTDA\\b","\\bG & K INTERNATIONAL GROUP S.A.C.","CIT S.R.L\\b",
                   "\\bENREAR SA DBA AEC RESOURCE\\b",
                   "\\bMULTICAM S.R.L\\b","\\bPRIUX SRL\\b","\\bGEMINI SISTEMAS LTDA\\b","\\bGEMINIS COMPUTER SA\\b","BUILDTECH",
                   "\\bSKYNET COM. REPRES.DE INFORMÁTICA LTDA\\b",
                   "\\bENG COMERCIO DE COMPUTADORES LIMITADA\\b","\\bENG COMERCIO DE COMPUTADORES LTDA\\b",
                   "\\bBEST SUL COMERCIO E PRESTAÇÃO\\b","\\bMCR SISTEMAS E CONSULTORIA LTDA\\b","\\bAX4B INOVACOES EM TECNOLOGIA LTDA\\b",
                   "\\bREALIZE TECNOLOGIA LTDA\\b","\\bBUYSOFT DO BRASIL LTDA\\b", "\\bTECGRAF TECN. COMPUTAÇÃO GRÁFICA LT\\b",
                   "\\bTECGRAF TECNOLOGIA EM COMPUTACAO GRAFICA LTDA\\b","\\bFRAZILLIO SOLUCOES DE TECNOLOGIA LTDA\\b",
                   "\\bBEST SOFTWARE TECNOLOGIA DA INFORMACAO LTDA\\b","\\bCAMSERV SERVICOS DE SOFTWARE LTD\\b",
                   "\\bCAMSERV TECNOLOGIA SERVICOS E SOFTWARE EIRELI\\b",
                   "CAMSERV","\\bPRO-SYSTEMS INFORMATICA LTDA\\b",
                   "\\bIPX TECNOLOGIA EIRELI\\b","\\bIPX TECNOLOGIA LTDA\\b","\\bSOLO NETWORK BRASIL LTDA\\b",
                   "\\bAUTODESK\\b", "\\bAUTO DESK\\b", "\\bACCA\\b", "\\bBENTLEY\\b", "\\bTQS\\b", "\\bMULTIPLUS\\b",
                   "MN", '22.993.665/0001-86 - GRAPHISOFT'
                   
                   )



# Filtragem Fav V2 -----------------------------------------------------------

Favorecidos1 <- df_postgres %>% 
  filter(str_detect(favorecido, paste(Favorecidos_v2, collapse = "|")))


# queries_Favorecidos3 <- df3 %>% 
#   filter(str_detect(Favorecido, paste(Favorecidos_v2, collapse = "|")))
# 
# queries_Favorecidos4 <- df4 %>% 
#   filter(str_detect(Favorecido, paste(Favorecidos_v2, collapse = "|")))

x<-colnames(Favorecidos1)


# names(queries_Favorecidos3)<- c(x)
# names(queries_Favorecidos4)<- c(x)
# colnames(queries_Favorecidos1)
# colnames(queries_Favorecidos3)
# colnames(queries_Favorecidos4)


# Filtros V2 --------------------------------------------------------------


Negar<- c("SAAS","MICROSOFT","TELEFONICA","ANULACAO","\\bADOBE\\b","SISTEMA OPERACIONAL WINDOWS", "WIN SERVER",
          "\\bCANCELAMENTO DE SALDO NAO UTILIZADO.\\b", "WINDOWS SERVER STD 2019 CORE OLP", "CALL WINDOWS SERVER 2019 OLP.
", " WINDOWS SERVER STANDARD PER CORE 16 LICENSES SOFTWARE LICENSE.", "AZURE", "POWER BI",
          "RENOVACAO LICENCA DE ANTIVIRUS KASPERSKY.", "CAMPUS POUSO ALEGRE IFSULDEMINAS",
          "CANCELAMENTO DE SALDO NAO UTILIZADO.", "PROJECT ON-LINE ESSENTIALS",
          "CONTRATO 01/2022 - DTI/PF - SUBSCRICAO - 3.323 UNIDADES DO ITEM 04 DO PREGAO 08/2021 - DTI/PF.",
          "CONTRATO 01/2022 - DTI/PF - SUBSCRICAO - 4154 UNIDADES DO ITEM 04 DO PREGAO 08/2022 - DTI/PF.",
          "CONTRATO 01/2022 - DTI/PF - SUBSCRICAO - 4985 UNIDADES DO ITEM 04 DO PREGAO 08/2022 - DTI/PF.",
          "CONTRATO 01/2022 - DTI/PF - ITENS 01, 02, 03, 05, 06, 07, 08, 09 E 10 DO PREGAO 08/2021 - SUBSCRICOES E GARANTIA DE ATUALIZACAO.",
          "DESPESA REF AO CT Nº 031/2020 BRASOFTWARE INFORMATICA LTDA - ITENS 1, 2, 3, 4, 5, 6, 8, 9, 10 PERIODO DE 23/12/2021 A 22/12/2022."
          )

Favorecidos1<-Favorecidos1 %>% 
  filter(str_detect(detalhamento_item,paste(Negar, collapse = "|"), negate = TRUE))

Favorecidos1<-Favorecidos1 %>% 
  filter(str_detect(observacao,paste(Negar, collapse = "|"), negate = TRUE))


Fav_cancel<- c('42.422.253/0001-01 - EMPRESA DE TECNOLOGIA E INFORMACOES DA PREVIDENCIA - DATAPREV S.A.', 
               '58.069.360/0010-10 - STEFANINI CONSULTORIA E ASSESSORIA EM INFORMATICA S.A.',
               '40.432.544/0001-47 - CLARO S.A.','01.657.353/0001-21 - AMAZONAS COPIADORAS LTDA',
               '33.000.118/0001-79 - TELEMAR NORTE LESTE S/A. - EM RECUPERACAO JUDICIAL',
               '26.201.167/0001-04 - DIGITAL PAPER LTDA',
               '01.765.213/0001-77 - COPYSYSTEMS-COPIADORAS SISTEMAS E SERVICOS LTDA',
               '59.456.277/0004-19 - ORACLE DO BRASIL SISTEMAS LTDA')

Favorecidos1<-Favorecidos1 %>% 
   filter(str_detect(favorecido,paste(Fav_cancel, collapse = "|"), negate = TRUE))
#

TEST <- Favorecidos1 %>% select(favorecido, detalhamento_item, observacao) %>%  filter(str_detect(favorecido, "57.142.978/0001-05 - BRASOFTWARE INFORMATICA LTDA"))

x<-TEST %>% count(detalhamento_item, observacao)


# queries_Favorecidos3<-queries_Favorecidos3 %>% 
#   filter(str_detect(observacao,paste(Negar, collapse = "|"), negate = TRUE))
# 
# queries_Favorecidos4<-queries_Favorecidos4 %>% 
#   filter(str_detect(detalhamento.item,paste(Negar, collapse = "|"), negate = TRUE))
# 
# queries_Favorecidos4<-queries_Favorecidos4 %>% 
#   filter(str_detect(observacao,paste(Negar, collapse = "|"), negate = TRUE))
# 
# final<- rbind(Favorecidos1,queries_Favorecidos3, queries_Favorecidos4)

# Fav_cancel<- c('42.422.253/0001-01 - EMPRESA DE TECNOLOGIA E INFORMACOES DA PREVIDENCIA - DATAPREV S.A.', 
#                '58.069.360/0010-10 - STEFANINI CONSULTORIA E ASSESSORIA EM INFORMATICA S.A.',
#                '40.432.544/0001-47 - CLARO S.A.','01.657.353/0001-21 - AMAZONAS COPIADORAS LTDA',
#                '33.000.118/0001-79 - TELEMAR NORTE LESTE S/A. - EM RECUPERACAO JUDICIAL',
#                '26.201.167/0001-04 - DIGITAL PAPER LTDA',
#                '01.765.213/0001-77 - COPYSYSTEMS-COPIADORAS SISTEMAS E SERVICOS LTDA',
#                '59.456.277/0004-19 - ORACLE DO BRASIL SISTEMAS LTDA',
#                '806030 - SERPRO - SEDE - BRASILIA',
#                '21.142.448/0001-10 - LENI S SILVA DE LUCENA',
#                '09.240.519/0001-11 - TARGETWARE INFORMATICA LTDA',
#                '27.778.168/0001-89 - K2 IT LTDA')
# 
# final<-final %>% 
#   filter(str_detect(Favorecido,paste(Fav_cancel, collapse = "|"), negate = TRUE))


# Favorecidos V2 extract --------------------------------------------------


#                '806030 - SERPRO - SEDE - BRASILIA',
#                '21.142.448/0001-10 - LENI S SILVA DE LUCENA',
#                '09.240.519/0001-11 - TARGETWARE INFORMATICA LTDA',
#                '27.778.168/0001-89 - K2 IT LTDA')
# Obs_cancel<- c('ANULACAO', 'MATLAB','ALTIUM', 'SAP POWER DESIGNER')



# Mudar fases da despesa para 40, 39 , 52, 92


# Usar ... ----------------------------------------------------------------

# queries_detalhamento.item
# Queries_observacao
# Queries_favorecidos



#Tentar ajeitar e reduzir Software_pronto



write_xlsx(x = Favorecidos1, path = '../AltoQI_BI/final.xlsx', col_names = TRUE)

# Filtrando por Aquisição de software pronto ------------------------------

# Software_pronto<-df %>% 
  # filter(str_detect(detalhamento.subelement, '05 - AQUISICAO DE SOFTWARE PRONTO'))

# Tirar windows, wifi, seguros, microsoft ... 
softwares_cancel <-c('WINDOWS','CAPACITAÇÃO ', 'SEGURO', 'MICROSOFT', 'SOFTWARE DE GERENCIAMENTO DE BACKUP', 
                     'SWITCH','VELOCIDADE COMUNICAÇÃO 1.000 MBPS', 'ANTIVÍRUS', 'SAS', 'OFFICE','STATA', 
                     'TELEFÔNICO','EVIEWS','TENABLE NETWORK','JAVA','CAMTASIA') 





