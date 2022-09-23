
# PACOTES ---------------------------------------------------------------------

library(dplyr)

# ANUARIO 2015 ----------------------------------------------------------------

# an15 <- NULL                         # OBJETO LIST COM A TOTALIDADE DAS TABELAS

baixar = function(                # FUNCAO DE DOWNLOAD E TRATAMENTO DAS TABELAS
    extensao = NA,
    setor = NA,
    subsetor = NA,
    link = NA
) {
  
  x = NULL
  
  temp <- tempfile(pattern = 'tabela_', fileext = '.xlsx')  # OBJETO TEMPORARIO
  
  for (i in extensao) {                       # DOWNLOAD E TRATAMENTO DA TABELA
    
    download.file(                                        # DOWNLOAD  DA TABELA
      url = paste0(gsub('Tabela..*', 'Tabela', link),
                   i,
                   gsub('.*Tabela.', '', link)),
      destfile = temp,
      method = 'libcurl',
      mode = 'wb'
    )
    
    d = readxl::read_excel(
      path = temp,
      col_names = F,
      na = c('-', '\\*') #,
      #skip = 2,
      #n_max = 145
    )
    
    nometab = d$...1[1]
    
    inicio.col = grep('^Estado', d$...1)
    
    if (length(inicio.col) == 0) {
      inicio.col = which(is.na(d$...2) == F) |> dplyr::first()
      d$...1[inicio.col] = 'Estado/Município'
    }
    
    fim = which(is.na(d$...1)) |> dplyr::last()-1
    
    if (ncol(d) > 8) {
      
      nomecol = d[inicio.col:(inicio.col+1), ] |> t() |> data.frame()
      
      guias = which(is.na(nomecol$X1) == F)
      
      for (j in which(is.na(nomecol$X1))) {
        nomecol$X1[j] = nomecol$X1[guias[which(guias < j)] |> dplyr::last()]
        rm(j)
      }
      
      nomecol = c(nomecol$X1[1], paste(nomecol$X1[-1], '-', nomecol$X2[-1]))
      
      names(d) = nomecol
      
      rm(nomecol, guias)
      
    } else {
      
      names(d) = gsub('\\*',
                      '',
                      d[inicio.col, ])
    }
    
    d = d[(inicio.col+1):fim, ]
    
    if (is.na(subsetor)) {
      x[[paste0(setor, ' - ', nometab)]] = d
    } else {
      x[[paste0(setor, ' - ', subsetor)]][[nometab]] = d
    }
    
    
    rm(i, d, nometab, inicio.col, fim)
    
  }                   # DOWNLOAD E TRATAMENTO DA TABELA
  
  x
  
}

## demografia -----------------------------------------------------------------

 # O Loop a seguir baixa as tabelas e as acomoda em um objeto list.

demo = baixar(
  extensao = 1:9,
  setor = 'Demografia',
  link = 'https://www.fapespa.pa.gov.br/sistemas/anuario2015/relatorios/demografia/xlsx/Tabela1_De.xlsx')

for (i in 1:9) {                                        # VISUALIZAR AS TABELAS
  View(demo[[i]], title = paste0('tab_', i))
  rm(i)
}                                    # VISUALIZAR AS TABELAS


d |> tidyr::pivot_longer(cols = c(-1),
                      names_to = 'periodo',
                      values_to = 'valor') |> View()

saveRDS(object = an15, file = 'Anuarios_RDS/Anuario_2015.RDS')

## economia -------------------------------------------------------------------

  # OBS: Aparentemente, as tabelas se repetem a partir da 6ª. Logo, o list con-
  # tém apenas 5 tabelas, já que 3 estão repetidas.

econ = baixar(
  extensao = 1:8,
  setor = 'Economia',
  subsetor = 'PIB',
  link = 'https://www.fapespa.pa.gov.br/sistemas/anuario2015/relatorios/economia/pib/xlsx/Tabela1_Pib.xlsx'
  )

for (i in 1:length(econ[[1]])) {
  View(econ[[1]][[i]], title = paste0('tab_', i))
  rm(i)
}


## salvar arquivos ------------------------------------------------------------

an15 = list(
  'demografia' = demo,
  'economia' = econ
  )

saveRDS(object = an15, file = 'Anuarios_RDS/Anuario_2015.RDS')


# ANUARIO 2018 ----------------------------------------------------------------

## demografia -----------------------------------------------------------------

links = c(
  'https://www.fapespa.pa.gov.br/sistemas/anuario2018//xls/demografia/tab_1.1_estimativas_populacionais_para_e_municipios_2013_a_2017.xlsx',
  'https://www.fapespa.pa.gov.br/sistemas/anuario2018/xls/demografia/tab_1.2_populacao_por_faixa_etaria_para_e_municipios_2011_a_2015.xlsx',
  'https://www.fapespa.pa.gov.br/sistemas/anuario2018/xls/demografia/tab_1.3_populacao_por_sexo_para_e_municipios_2011_a_2015.xlsx',
  'https://www.fapespa.pa.gov.br/sistemas/anuario2018//xls/demografia/tab_1.4_razao_de_sexos_para_e_municipios_2011_a_2015.xlsx',
  'https://www.fapespa.pa.gov.br/sistemas/anuario2018//xls/demografia/tab_1.5_proporcao_de_idosos_para_e_municipios_2011_a_2015.xlsx',
  'https://www.fapespa.pa.gov.br/sistemas/anuario2018//xls/demografia/tab_1.6_indice_de_envelhecimento_para_e_municipios_2011_a_2015.xlsx',
  'https://www.fapespa.pa.gov.br/sistemas/anuario2018/xls/demografia/tab_1.7_razao_de_dependencia_para_e_municipios_2011_a_2015.xlsx',
  'https://www.fapespa.pa.gov.br/sistemas/anuario2018/tabelas/demografia/tab_1.8_taxa_de_fecundidade_total_para_e_municipios_2011_a_2015.htm',
  'https://www.fapespa.pa.gov.br/sistemas/anuario2018/xls/demografia/tab_1.9_taxa_especifica_de_fecundidade_por_faixa_etaria_para_e_municipios_2011_a_2015.xlsx'
)



# ANUARIO 2021 ----------------------------------------------------------------

# Percebi que cada anuario tem sua particularidade. Preciso vence-los um a um

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

writexl::write_xlsx(     # SALVAR O ARQUIVO EM XLSX. CADA PLANILHA É UMA TABELA
  x = dm$demografia,
  path = '1_demografia.xlsx',
  col_names = T,
  format_headers = T
  )
