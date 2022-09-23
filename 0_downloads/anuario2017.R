
library(tidyverse)
library(rvest)

# DEMOGRAFIA ------------------------------------------------------------------

link = 'https://www.fapespa.pa.gov.br/sistemas/anuario2017/demografia.html'

t0 <- link |>
  read_html(encoding = 'UTF-8') |> 
  html_elements(
    xpath = '/html/body/nav/div/ul/li'
  ) |> 
  html_children() |>
  html_attr(name = 'href')

demografia = data.frame(
  nome = rep(NA, length(t0)),
  tabela = rep(NA, length(t0))
)

for (i in 1:length(t0)) {         # LOOP DE OBTENCAO DAS TABELAS POR TABELA
  
  retirar = gsub('\\..*', '', gsub('.*/', '', link)) |> paste0('.*')
  
  d = gsub(retirar, t0[i], link)
  
  d = d |>
    read_html(encoding = 'UTF-8') |> 
    html_elements(
      xpath = '//tr/td[4]'
    ) |> html_children() |> html_attr(name = 'href')
  
  d = paste0(
    gsub(retirar, '', link),
    ifelse(
      grep('\\./', d) |> length() == 0,
      paste0('relatorios/demografia/', '/', d),
      gsub('.*\\./', '', d)
    )
  )
  
  
  temp = tempfile(
    pattern = paste0('tab_', i, '-'),
    fileext = '.xlsx'
  )
  
  download.file(
    url = d,
    destfile = temp,
    method = 'libcurl',
    mode = 'wb'
  )
  
  d = readxl::read_xlsx(
    path = temp,
    col_names = F,
    na = c('-', '\\*')
  )
  
  demografia$nome[i] = d$...1[1]
  demografia$tabela[i] = list(d)
  
  rm(i, d, temp, retirar)
  
}     # LOOP DE OBTENCAO DAS TABELAS POR TABELA

# ECONOMIA --------------------------------------------------------------------

div <- seq(1,13,2)          # numero das listas (subsecoes) na pagina principal

link = 'https://www.fapespa.pa.gov.br/sistemas/anuario2017/economia.html'

economia = NULL

for (j in div) {
  t0 <- link |>
    read_html(encoding = 'UTF-8') |> 
    html_elements(
      xpath = paste0('/html/body/nav/div[',j,']/ul/li')
    ) |> 
    html_children() |>
    html_attr(name = 'href')
  
  subsecao = gsub('/.*', '', gsub(paste0('.*economia','/'), '', t0[1]))
  
  tab = data.frame(
    nome = rep(NA, length(t0)),
    tabela = rep(NA, length(t0))
  )
  
  for (i in 1:length(t0)) {         # LOOP DE OBTENCAO DAS TABELAS POR SUBSECAO
    
    retirar = gsub('\\..*', '.*', gsub('.*/', '', link))
    
    d = gsub(retirar, t0[i], link)
    
    d = d |>
      read_html(encoding = 'UTF-8') |> 
      html_elements(
        xpath = '//tr/td[4]'
      ) |> html_children() |> html_attr(name = 'href')
    
    d = paste0(
      gsub('eco.*', '', link),
      ifelse(
        grep('\\./', d) |> length() == 0,
        paste0('relatorios/economia/', subsecao, '/', d),
        gsub('.*\\./', '', d)
      )
    )
    
    
    temp = tempfile(
      pattern = paste0('tab_', i, '-'),
      fileext = '.xlsx'
    )
    
    download.file(
      url = d,
      destfile = temp,
      method = 'libcurl',
      mode = 'wb'
    )
    
    d = readxl::read_xlsx(
      path = temp,
      col_names = F,
      na = c('-', '\\*')
    )
    
    tab$nome[i] = d$...1[1]
    tab$tabela[i] = list(d)
    
    rm(i, d, temp, retirar)
    
  }     # LOOP DE OBTENCAO DAS TABELAS POR SUBSECAO
  
  rm(j, tab)
  
  economia[[subsecao]] = tab
}

rm(link, div, t0, subsecao)

# INFRAESTRUTURA --------------------------------------------------------------

link <-'https://www.fapespa.pa.gov.br/sistemas/anuario2017/infraestrutura.html'

t0 <- link |>
  read_html(encoding = 'UTF-8') |> 
  html_elements(
    xpath = '/html/body/nav/div/ul/li'
  ) |> 
  html_children() |>
  html_attr(name = 'href')

infra = data.frame(
  nome = rep(NA, length(t0)),
  tabela = rep(NA, length(t0))
)

for (i in 1:length(t0)) {         # LOOP DE OBTENCAO DAS TABELAS POR TABELA
  
  retirar = gsub('\\..*', '', gsub('.*/', '', link)) |> paste0('.*')
  
  d = gsub(retirar, t0[i], link)
  
  d = d |>
    read_html(encoding = 'UTF-8') |> 
    html_elements(
      xpath = '//tr/td[4]'
    ) |> html_children() |> html_attr(name = 'href')
  
  d = paste0(
    gsub(retirar, '', link),
    ifelse(
      grep('\\./', d) |> length() == 0,
      paste0('relatorios/infraestrutura/', '/', d),
      gsub('.*\\./', '', d)
    )
  )
  
  
  temp = tempfile(
    pattern = paste0('tab_', i, '-'),
    fileext = '.xlsx'
  )
  
  download.file(
    url = d,
    destfile = temp,
    method = 'libcurl',
    mode = 'wb'
  )
  
  d = readxl::read_xlsx(
    path = temp,
    col_names = F,
    na = c('-', '\\*')
  )
  
  infra$nome[i] = d$...1[1]
  infra$tabela[i] = list(d)
  
  rm(i, d, temp, retirar)
  
}         # LOOP DE OBTENCAO DAS TABELAS POR TABELA

rm(t0, link)

# MEIO AMBIENTE ---------------------------------------------------------------

link <- 'https://www.fapespa.pa.gov.br/sistemas/anuario2017/meioambiente.html'

t0 <- link |>
  read_html(encoding = 'UTF-8') |> 
  html_elements(
    xpath = '/html/body/nav/div/ul/li'
  ) |> 
  html_children() |>
  html_attr(name = 'href')

meio = data.frame(
  nome = rep(NA, length(t0)),
  tabela = rep(NA, length(t0))
)

for (i in 1:length(t0)) {         # LOOP DE OBTENCAO DAS TABELAS POR TABELA
  
  retirar = gsub('\\..*', '', gsub('.*/', '', link)) |> paste0('.*')
  
  d = gsub(retirar, t0[i], link)
  
  d = d |>
    read_html(encoding = 'UTF-8') |> 
    html_elements(
      xpath = '//tr/td[4]'
    ) |> html_children() |>
    html_attr(name = 'href')
  
  d = paste0(
    gsub(retirar, '', link),
    ifelse(
      grep('\\./', d) |> length() == 0,
      paste0('relatorios/meio_ambiente/', '/', d),
      gsub('.*\\./', '', d)
    )
  )
  
  
  temp = tempfile(
    pattern = paste0('tab_', i, '-'),
    fileext = '.xlsx'
  )
  
  download.file(
    url = d,
    destfile = temp,
    method = 'libcurl',
    mode = 'wb'
  )
  
  d = readxl::read_xlsx(
    path = temp,
    col_names = F,
    na = c('-', '\\*')
  )
  
  meio$nome[i] = d$...1[1]
  meio$tabela[i] = list(d)
  
  rm(i, d, temp, retirar)
  
}         # LOOP DE OBTENCAO DAS TABELAS POR TABELA

rm(t0, link)

# SOCIAL ----------------------------------------------------------------------

div <- seq(1, 11, 2)                  # numero das listas (subsecoes) na pagina principal

link = 'https://www.fapespa.pa.gov.br/sistemas/anuario2017/social.html'

social = NULL

for (j in div) {
  t0 <- link |>
    read_html(encoding = 'UTF-8') |> 
    html_elements(
      xpath = paste0('/html/body/nav/div[',j,']/ul/li')
    ) |> 
    html_children() |>
    html_attr(name = 'href')
  
  subsecao = gsub('/.*', '', gsub('.*/social/', '', t0[1]))
  
  tab = data.frame(
    nome = rep(NA, length(t0)),
    tabela = rep(NA, length(t0))
  )
  
  for (i in 1:length(t0)) {         # LOOP DE OBTENCAO DAS TABELAS POR SUBSECAO
    
    retirar = gsub('\\..*', '', gsub('.*/', '', link)) |> paste0('.*')
    
    d = gsub(retirar, t0[i], link)
    
    d = d |>
      read_html(encoding = 'UTF-8') |> 
      html_elements(
        xpath = '//tr/td[4]'
      ) |> html_children() |>
      html_attr(name = 'href')
    
    d = paste0(
      gsub(retirar, '', link),
      ifelse(
        grep('\\./', d) |> length() == 0,
        paste0('relatorios/social/', subsecao, '/', d),
        gsub('.*\\./', '', d)
      )
    )
    
    
    temp = tempfile(
      pattern = paste0('tab_', i, '-'),
      fileext = '.xlsx'
    )
    
    download.file(
      url = d,
      destfile = temp,
      method = 'libcurl',
      mode = 'wb'
    )
    
    tabela = try(
      readxl::read_xlsx(
        path = temp,
        col_names = F,
        na = c('-', '\\*')
      ),
      silent = T
    )
    
    if(is.data.frame(tabela)) {
      d = tabela
      rm(tabela)
    } else {
      d = gsub(retirar, t0[i], link)
      d = d |>
        read_html(encoding = 'UTF-8') |>
        html_table(
          d,
          header = F,
          na.strings = c('\\*', '-$')
        )
      d = as.data.frame(d)
    }
    
    tab$nome[i] = d[1, 1]
    tab$tabela[i] = list(d)
    
    rm(i, d, temp, retirar)
    
  }     # LOOP DE OBTENCAO DAS TABELAS POR SUBSECAO
  
  rm(j, t0)
  
  social[[subsecao]] = tab
}

rm(div, link, tab, subsecao)

# OBJETO LIST -----------------------------------------------------------------

anuario17 = list(
  'demografia' = demografia,
  'economia' = economia,
  'meio_ambiente' = meio,
  'infraestrutura' = infra,
  'social' = social
)


saveRDS(
  object = anuario17,
  file = '01_base_de_dados/anuario2017.RDS'
)
