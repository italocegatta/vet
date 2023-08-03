#' Calcula o Valor Esperado da Terra
#'
#' Convenient function to save charts in Eps format.
#'
#' @param taxa Numeric. Taxa de desconto na forma decimal
#' @param rotacao Numeric. Idade do ciclo
#' @param vp_custo Numeric. Valor presente do custo de formacao
#' @param producao Numeric. Producao de madeira no fim do ciclo
#' @param preco_madeira Numeric. Preco de venda da madeira em pé
#'
#' @export
#'
vet <- function(taxa, rotacao, vp_custo, producao, preco_madeira) {

  vp_receita <- (preco_madeira * producao) / (1 + taxa)^rotacao
  vpl <- vp_receita - vp_custo
  vet = (vpl * (1 + taxa)^rotacao) / ((1 + taxa)^rotacao - 1)

  vet
}
