
ano = 2015

# incluir um ifelse para considerar todos os arquivos list

d = readRDS(
  file = paste0(
    '01_base_de_dados/anuario',
    ano,
    '.RDS'
  )  
)


for (i in 1:length(d)) {
  
  secao = d[i] |> names()
  d0 = NULL
  
  if(class(d[[i]]) == 'data.frame') {
    
    for (j in 1:length(d[[secao]][['tabela']])) {
      
      nome = d[[secao]][['nome']][j] |> as.character()
      if (nchar(nome) > 31) {
        nome = gsub(
          paste0(rep('.', nchar(nome)-31), collapse = '') |> paste0('$'),
          '',
          nome
        )
      }
      
      d0[[nome]] = d[[secao]][['tabela']][[j]]
      rm(j, nome)
    }
    
    writexl::write_xlsx(
      x = d0,
      path = paste0(
        '01_base_de_dados/00_conversao_files/',
        'anuario',
        ano, '_',
        secao, '.xlsx'))
    
  } else {
    
    
    for (k in 1:length(d[[i]])) {
      
      subsecao = d[[secao]][k] |> names()
      
      for (j in 1:length(d[[secao]][[k]][['tabela']])) {
        
        
        nome = d[[secao]][[k]][['nome']][j] |> as.character()
        if (nchar(nome) > 31) {
          nome = gsub(
            paste0(rep('.', nchar(nome)-31), collapse = '') |> paste0('$'),
            '',
            nome
          )
        }
        
        d0[[nome]] = d[[secao]][[k]][['tabela']][[j]]
        rm(j, nome)
      }
      
      writexl::write_xlsx(
        x = d0,
        path = paste0(
          '01_base_de_dados/00_conversao_files/',
          'anuario',
          ano, '_',
          secao,'--',subsecao, '.xlsx'))
      
      rm(k, subsecao)
      
    }
    
    
  }
  
  rm(d0, secao, i)
  
}
