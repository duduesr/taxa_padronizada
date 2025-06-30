
###############################################################################
#' Funcao de Padronizacao de Taxas
#'
#' @description XXXXXXXXXXXXXXXXXXXXXXXXXX
#' @param dados banco de dados de resultados
#' @return Lista com informações sobre erros
#' @examples
#' data(brasil2000)
#' @export

padroniza_taxa = function(
  x, #db geral
  p, #db com a pop padrao
  #definicao de variaveis dos dataframes
  var.populacao = "populacao",
  var.evento = 'obitos',
  var.area = c("cod_uf", "ano"),
  var.categoria = c("sexo", 'gi'),
  escala = 1000,
  nome = "Taxa"

){

  #trata pop padrao
  # if (length(var.categoria)>1){p$categoria <- apply( p[ , var.categoria ] , 1 , paste , collapse = "-" ) #cria variavel de grupo
  # }else{ p$categoria = as.character(p[, var.categoria])}
  p$categoria <- apply( p[ , var.categoria ] , 1 , paste , collapse = "-" )

  p = p %>% rename(
    pop.padrao = var.populacao
  )  %>%
    group_by(categoria) %>%
    summarise(
      pop.padrao = sum(pop.padrao, na.rm = T)
    ) %>%
    ungroup()


  #arruma as varaveis do db geral
  x$categoria <- apply( x[ , var.categoria ] , 1 , paste , collapse = "-" ) #cria variavel de grupo
  x$area <- apply( x[ , var.area ] , 1 , paste , collapse = "-" ) #cria variavel de grupo

  #gera arquivo com area
  area.aux = x[,c(var.area,'area')] %>%
    filter(!duplicated(area))

  x = x %>% rename(
    evento = var.evento,
    pop.original = var.populacao
  ) %>%
    group_by(area, categoria) %>%
    summarise(
      evento = sum(evento, na.rm = T),
      pop.original = sum(pop.original, na.rm = T)
    ) %>%
    ungroup() %>%
    left_join(p) %>%
    mutate(tx = evento/pop.original,
           evento.padrao = pop.padrao*tx)

  #calculo da taxa
  taxa.padrao = x %>%
    group_by(area) %>%
    summarise(taxa.bruta = escala*sum(evento)/sum(pop.original),
              taxa.padrao = escala*sum(evento.padrao)/sum(pop.padrao),
    ) %>%
    ungroup() %>%
    mutate(nome = nome) %>%
    left_join(area.aux) %>%
    select(var.area, nome, taxa.bruta, taxa.padrao)

  return(taxa.padrao)
}
