library(vet)
library(dplyr)
library(tidyr)
library(stringr)
library(ggridges)
library(ggplot2)
library(patchwork)
library(scales)
library(purrr)
library(copula)

N = 10000
CUSTO = 12000
PRODUCAO = 240
PRECO = 150

lst_param_custo <- list(
  min = CUSTO * 0.5,
  max = CUSTO * 1.5,
  mean = CUSTO,
  sd = CUSTO * 0.2,
  skew = 0.7
)
lst_param_producao <- list(
  min = PRODUCAO * 0.5,
  max = PRODUCAO * 1.5,
  mean = PRODUCAO,
  sd = PRODUCAO * 0.2,
  skew = 0.7
)

vec_vet_unif_unif_indep <- vet(
  taxa = 0.10,
  rotacao = 6,
  vp_custo = runif(N, lst_param_custo$min, lst_param_custo$max),
  producao = runif(N, lst_param_producao$min, lst_param_producao$max),
  preco_madeira = 150
)
copula_normal <- normalCopula(0.7, dim = 2)

copula_mvdc <- mvdc(
  copula = copula_normal,
  margins = c("unif", "unif"),
  paramMargins = list(
    list(min = lst_param_custo$min, max = lst_param_custo$max),
    list(min = lst_param_producao$min, max = lst_param_producao$max)
  )
)

copula_sample <- rMvdc(N, copula_mvdc)

vec_vet_unif_unif_dep <- vet(
      taxa = 0.10,
      rotacao = 6,
      vp_custo = copula_sample[, 1],
      producao = copula_sample[, 2],
      preco_madeira = 150
)

vec_vet_unif_unif_indep <- vet(
  taxa = 0.10,
  rotacao = 6,
  vp_custo = runif(N, lst_param_custo$min, lst_param_custo$max),
  producao = runif(N, lst_param_producao$min, lst_param_producao$max),
  preco_madeira = 150
)

tibble(dep = vec_vet_unif_unif_dep, indep = vec_vet_unif_unif_indep) |>
  gather() |>
  ggplot(aes(value, fill = key)) +
  geom_density(alpha = 0.5) +
  theme_bw()

ks.test(vec_vet_unif_unif_dep, vec_vet_unif_unif_indep)
