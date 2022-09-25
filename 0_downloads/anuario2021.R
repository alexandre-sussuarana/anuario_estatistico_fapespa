# Você está no script de download das tabelas do Anuário estatístico 2021 do Es
# tado do Pará. Para acessar o painel de informações, execute o comando abaixo.
browseURL('https://www.fapespa.pa.gov.br/sistemas/anuario2021/')

# Observações -----------------------------------------------------------------

 # O anuário deste ano disponibiliza a totalidade das tabelas em formato zip.

# Pacotes ---------------------------------------------------------------------

library(tidyverse)
library(rvest)

# DOWNLOAD --------------------------------------------------------------------

link =                         # LINK DA PASTA ZIP COM TODAS AS PLANILHAS EXCEL
  'https://www.fapespa.pa.gov.br/sistemas/anuario2021/planilhas/planilhas.zip'


temp = tempfile(                 # OBJETO TEMPORARIO PARA DOWNLOAD DA PASTA ZIP
  pattern = 'an21_',
  fileext = '.zip'
)


download.file(                                          # DOWNLOAD DA PASTA ZIP
  url = link,
  destfile = temp,
  method = 'libcurl',
  mode = 'wb'
)


untar(               # EXTRAIR AS PLANILHAS EXCEL PARA UMA PASTA CHAMADA TABS21
  tarfile = temp,
  exdir = paste0(
    temp |> dirname(),
    '/tabs21/'
    )
  )


tabs <- dir(            # LEITURA DO ENDERECO DAS PLANILHAS NA PASTA TEMPORARIA
  path = paste0(
    temp |> dirname(),
    '/tabs21/'
  ),
  pattern = '.xlsx',
  full.names = T,
  recursive = T,
  include.dirs = T
    
)


anuario21 <- NULL                    # OBJETO DE ACONDICIONAMENTO DAS PLANILHAS


for (i in 1:length(tabs)) {                     # LOOP DE LEITURA DAS PLANILHAS
  
  d = readxl::read_xlsx(
    path = tabs[i],
    col_names = F
  )
  
  nome = d$...1[1]
  
  secao = gsub('/t.*', '', gsub('.*//', '', tabs[i]), T)
  
  if(grep('/', secao) |> length() == 1) {
    subsecao = gsub('.*/', '', secao)
    secao = gsub('/.*', '', secao)
    anuario21[[secao]][[subsecao]][[nome]] = d
    rm(subsecao)
  
  } else {
    anuario21[[secao]][[nome]] = d
  }
  
  rm(d, i, secao, nome)
  
}                 # LOOP DE LEITURA DAS PLANILHAS


# SALVAR O ARQUIVO EM OBJETO LIST ---------------------------------------------


saveRDS(
  object = anuario21,
  file = '01_base_de_dados/anuario2021.RDS'
)
