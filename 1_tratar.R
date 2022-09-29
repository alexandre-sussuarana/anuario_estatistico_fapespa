# Este script apresenta uma função de tratamento para as tabelas dos anuarios

library(tidyverse)

tratar <- function(x) {
  
  
  inicio.col = grep('^Estado', x$...1)                       # inicio da tabela
  
  
  if (length(inicio.col) == 0) {      # condicional para verificar o inicio.col
    inicio.col = which(is.na(x$...2) == F) |> dplyr::first()
    x$...1[inicio.col] = 'Estado/Município'
  }  # condicional para verificar o inicio.col
  
  
  fim = grep('fonte', x$...1, T)-2                              # Fim da tabela
  
  
  if (ncol(x) > 8) {           # condicional para titulos com celulas mescladas
    
    nomecol = x[inicio.col:(inicio.col+1), ] |> t() |> data.frame()
    
    names(nomecol) = paste0('X', 1:ncol(nomecol))
    
    guias = which(is.na(nomecol$X1) == F)
    
    for (j in which(is.na(nomecol$X1))) {
      nomecol$X1[j] = nomecol$X1[guias[which(guias < j)] |> dplyr::last()]
      rm(j)
    }
    
    nomecol = c(nomecol$X1[1], paste(nomecol$X1[-1], '-', nomecol$X2[-1]))
    
    names(x) = nomecol
    
    rm(nomecol, guias)
    
  } else {
    
    names(x) = gsub('\\*',
                    '',
                    x[inicio.col, ])
  } # condicional para titulos com celulas mescladas
  
  
  x = x[c(inicio.col + ifelse(ncol(x)>8, 2, 1)) : fim, ]   # retirada de linhas
                                                     # desnecessarias ou vazias
  
  rm(inicio.col, fim)
  
  x
}
