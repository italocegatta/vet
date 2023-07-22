#' Calcula o Valor Esperado da Terra
#'
#' Convenient function to save charts in Eps format.
#'
#' @param taxa Numeric. Taxa de desconto na forma decimal
#' @param custo_formacao Numeric. Valor presente do custo de formacao
#' @param preco_madeira Numeric. Preco de venda da madeira em pé
#' @param producao Numeric. Producao de madeira no fim do ciclo
#' @param ciclo Numeric. Idade do ciclo
#'
#' @export
#'
vet <- function(taxa, custo_formacao, preco_madeira, producao, ciclo) {

  vec_receita_vpl <- (preco_madeira * producao) / (1 + taxa)^ciclo

  # valor presente liquido
  vec_vpl <- vec_receita_vpl - custo_formacao

  # valor esperado da terra
  vec_vet <- (vec_vpl * (1 + taxa)^ciclo) / ((1 + taxa)^ciclo - 1)

  vec_vet
}
