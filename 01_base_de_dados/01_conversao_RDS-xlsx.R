# Observações:
#   Este script converte os arquivos RDS em EXCEL. A pasta '00_conversao_files'
# está vazia para receber os aquivos. Você decide se mantém os arquivos. Este 
# script também serve de base para a conversão em arquivos pickle (python).

# Este script funciona para os anos de 2015 a 2020. COmo a estrutura do ano de
# 2021 é diferente, precisarei atualizá-lo depois.


ano = 2019

# incluir um ifelse para considerar todos os arquivos list

d = readRDS(                                        # Base de dados a converter
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
      
      if(all(d[[secao]][['tabela']][[j]] |> class() != 'data.frame')) {
        print(
          paste0(
            'Seção: ', secao, '; subseção: ', subsecao, '; tabela ', j,
            ' não existe!'
          )
        )
        rm(j, nome)
        next
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
        
        if(all(d[[secao]][[k]][['tabela']][[j]] |> class() != 'data.frame')) {
          print(
            paste0(
              'Seção: ', secao, '; subseção: ', subsecao, '; tabela ', j,
              ' não existe!'
            )
          )
          rm(j, nome)
          next
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
