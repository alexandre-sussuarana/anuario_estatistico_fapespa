# ANUARIO 2021 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# PACOTES ---------------------------------------------------------------------

library(dplyr)

# DOWNLOAD E EXTRACAO ---------------------------------------------------------


temp <- tempfile(                   # arquivo temporario pra abrigar o download
  pattern = 'fapespa_',
  fileext = '.zip'
)


download.file(                                            # Download do arquivo
  url = paste0('https://www.fapespa.pa.gov.br/sistemas/anuario2021/',
               'planilhas/planilhas.zip'),
  # uso o paste0 para respeitar o limite de 80 colunas no script.
  destfile = temp,
  method = 'libcurl',
  mode = 'wb'
)

  # OBS1: funcao unzip nao funciona - - - - - - - - - - - - - - - - - - - - - - 
  #
  #  Como a funcao unzip() nao esta funcionando para este arquivo, utilizei um
  # metodo mais direto:
  #
  #  O seguinte comando abrirá a pasta temporaria que contem o arquivo zip.
  # Identifique o arquivo que comeca com o nome 'fapespa', clique com o botao
  # direito do mouse e selecione 'extract to NOME DO ARQUIVO' ou 'extrair para
  # NOME DO ARQUIVO'.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

temp |> dirname() |> shell.exec()                         # Abrir o arquivo zip


unlink(temp)                        # remover o arquivo zip da pasta temporaria


# TABELAMENTO DOS ARQUIVOS DA PASTA ZIP ---------------------------------------


pasta <- temp %>%                               # caminho da pasta de planilhas
  dirname() %>%
  dir(pattern = 'fapespa',
      full.names = T)


arquivos <- dir(pasta,                      # leitura dos arquivos xls da pasta
                pattern = '.xls',
                recursive = T,
                full.names = T)

  # Os proximos comandos tabelam os arquivos por tema


tema <- c('demografia',                                        # Lista de temas
          'economia',
          'infraestrutura',
          'meio-ambiente',
          'social')


tab <- NULL                                           # objeto list das tabelas



for (i in 1:length(tema)) {      # Loop de obtencao das informacoes disponiveis
  
  # local por tema
  linhas <- grep(paste0(tema[i],'/'), arquivos, T)
  
  # construcao do dataframe a ser incluido no objeto list tab
  tab[[tema[i]]] = data.frame(
    
    # busca por subtemas
    {subtema = grep('/\\d', arquivos[linhas])
    if(length(subtema) != 0) {
      subtema = gsub('/.*', '',
                     gsub(paste0('.*/', tema[i], '/'), '', arquivos[linhas]))
    } else {
      subtema = rep(NA, length(linhas))
    }},
    
    # codigo da tabela
    gsub('-.*', '', gsub('.*tab-', '', arquivos[linhas])),
    
    # descricao da tabela
    gsub('-', ' ',
         gsub('.xls.*', '',
              gsub('.*\\.\\d-', '',
                   gsub('.*\\.\\d\\d-', '',
                        gsub('.*tab', '', arquivos[linhas]))))),
    
    # endereco local das tabelas
    arquivos[linhas]
  )
  # nomeacao das colunas da tabela
  names(tab[[tema[i]]]) = c('subtema', 'tb.cod', 'tb.nome', 'local')
  rm(i, linhas)
}  # Loop de obtencao das informacoes disponiveis


saveRDS(object = tab,                        # SALVAR O MENU (LISTA DE TABELAS)
        file = '0_menu_tabelas.RDS')


# MONTAGEM DE TABELAS POR TEMA ------------------------------------------------

  #    A montagem de tabelas segue especificidades particulares a cada tema. Ao
  # longo do tempo, atualizarei este script na ordem alfabetica. Por coinciden-
  # cia, a demografia e a que mais me interessa neste momento do mestrado.

## ├─ DEMOGRAFIA --------------------------------------------------------------


dm = NULL                         # OBJETO QUE GUARDA AS TABELAS EM CLASSE LIST


for (j in 1:nrow(tab[[1]])) {
  
  
  a = readxl::read_excel(                   # DATAFRAME LIDO DO ARQUIVO XLS/CSV
    path = tab[[1]][j, 4],
    skip = 4,
    col_names = F,
    na = c('-', '..', '...')
  )
  
  
  if (is.na(a[2,1])) {          # CONDICAO PARA TRATAMENTO DE CELULAS MESCLADAS
    
    
    linhas = which(a[, 1] == 'Pará')
    linhas = c(linhas-2, linhas-1)
    
    
    nomes = t(a[linhas, ])      # dataframe transversa com as celulas mescladas
    
    m = data.frame(                           # Celulas preenchidas na 1ª linha
      rot = nomes[which(is.na(nomes[, 1]) == F)],
      pos = which(is.na(nomes[, 1]) == F)
    ) 
    
    #  o = which(is.na(nomes[, 1]))       # lista de celulas vazias na 1º linha
    
    for (k in 1:nrow(nomes)) {       # loop de juncao das informacoes mescladas
      nomes[k, 1] = paste0(
        m$rot[which(m$pos <= k) |> last()],
        ' - ',
        nomes[k, 2]
      ) 
      rm(k)
    } # loop de juncao das informacoes mescladas
    
    names(a) = nomes[, 1]                              # renomeando as colunas
    
    a = a[-c(1:last(linhas)), ]        # retirada de linhas mescladas da tabela
    
    rm(nomes, m)
    
  } else {
    
    names(a) = a[1, ] |> as.character()        # INCLUSAO DO ROTULO DAS COLUNAS
    a = a[-1, ]                       # RETIRADA DA LINHA DE ROTULO DAS COLUNAS
    
    
  } # CONDICAO PARA TRATAMENTO DE CELULAS MESCLADAS
  
  
  a = tidyr::pivot_longer(        # TRANSFORMACAO DA TABELA DE WIDE PARA LONGER
    data = a,
    cols = 2:length(a),
    names_to = 'periodo - variavel',
    values_to = 'valor',
    values_transform = 'as.numeric'
  )
  
  
  dm[[names(tab[1])]] [[tab[[1]][j,3]]] = a |>               # INCLUSAO NO LIST
    filter(is.na(valor) == F)
  
  
  rm(j, a)
  
  
} # LOOP DE MONTAGEM DAS TABELAS EM LIST


saveRDS(object = dm,                                  # SALVAR O ARQUIVO EM RDS
        file = '1_demografia.RDS')