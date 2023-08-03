
# pacotes -----------------------------------------------------------------

library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(sn)
library(copula)


# Util ---------------------------------------------------------------------

ajusta_extremos <- function(x, minimo, maximo) {

  valores_fora_limites <- x < minimo | x > maximo

  x[valores_fora_limites] <- runif(sum(valores_fora_limites), minimo, maximo)

  return(x)
}

convert_param_skew <- function(x) {
  as.numeric(x) %>%
    cp2dp("SN") %>%
    as.list()
}


# Setup -------------------------------------------------------------------

set.seed(123)

N = 10000


# Custo de produção -------------------------------------------------------

CUSTO = 12000

lst_param_custo <- list(
  min = CUSTO * 0.5,
  max = CUSTO * 1.5,
  mean = CUSTO,
  sd = CUSTO * 0.2,
  skew = 0.7
)

tab_custo <- tibble(
  cd_var = "custo",
  nm_var =  "Custo de formação",
  cd_distribuicao = c("unif", "norm", "sn_pos", "sn_neg"),
  nm_distribuicao = c("unif", "norm", "sn", "sn"),
  vl_amostra = list(
      runif(N, lst_param_custo$min, lst_param_custo$max),
      rnorm(N, lst_param_custo$mean, lst_param_custo$sd),
      rsn(N, dp = cp2dp(
        c(
          lst_param_custo$mean,
          lst_param_custo$sd,
          lst_param_custo$skew
        ),
        "SN"
      )),
      rsn(N, dp = cp2dp(
        c(
          lst_param_custo$mean,
          lst_param_custo$sd,
          -lst_param_custo$skew
        ),
        "SN"
      ))
    )
  ) |>
  unnest(vl_amostra) |>
  group_by(cd_distribuicao) |>
  mutate(vl_amostra = ajusta_extremos(vl_amostra, lst_param_custo$min, lst_param_custo$max)) |>
  ungroup()


# Producao de madeira ----------------------------------------------------

PRODUCAO = 240

lst_param_producao <- list(
  min = PRODUCAO * 0.5,
  max = PRODUCAO * 1.5,
  mean = PRODUCAO,
  sd = PRODUCAO * 0.2,
  skew = 0.7
)

tab_producao <- tibble(
  cd_var = "producao",
  nm_var =  "Producao de madeira",
  cd_distribuicao = c("unif", "norm", "sn_pos", "sn_neg"),
  nm_distribuicao = c("unif", "norm", "sn", "sn"),
  vl_amostra = list(
      runif(N, lst_param_producao$min, lst_param_producao$max),
      rnorm(N, lst_param_producao$mean, lst_param_producao$sd),
      rsn(N, dp = cp2dp(
        c(
          lst_param_producao$mean,
          lst_param_producao$sd,
          lst_param_producao$skew
        ),
        "SN"
      )),
      rsn(N, dp = cp2dp(
        c(
          lst_param_producao$mean,
          lst_param_producao$sd,
          -lst_param_producao$skew
        ),
        "SN"
      ))
    )
  ) |>
  unnest(vl_amostra) |>
  group_by(cd_distribuicao) |>
  mutate(vl_amostra = ajusta_extremos(vl_amostra, lst_param_producao$min, lst_param_producao$max)) |>
  ungroup()


# Preço da madeira ------------------------------------------------------

PRECO = 150

lst_param_preco <- list(
  min = PRECO * 0.5,
  max = PRECO * 1.5,
  mean = PRECO,
  sd = PRECO * 0.2,
  skew = 0.7
)

tab_preco <- tibble(
  cd_var = "preco",
  nm_var =  "Preço da madeira",
  cd_distribuicao = c("unif", "norm", "sn_pos", "sn_neg"),
  nm_distribuicao = c("unif", "norm", "sn", "sn"),
  vl_amostra = list(
      runif(N, lst_param_preco$min, lst_param_preco$max),
      rnorm(N, lst_param_preco$mean, lst_param_preco$sd),
      rsn(N, dp = cp2dp(
        c(
          lst_param_preco$mean,
          lst_param_preco$sd,
          lst_param_preco$skew
        ),
        "SN"
      )),
      rsn(N, dp = cp2dp(
        c(
          lst_param_preco$mean,
          lst_param_preco$sd,
          -lst_param_preco$skew
        ),
        "SN"
      ))
    )
  ) |>
  unnest(vl_amostra) |>
  group_by(cd_distribuicao) |>
  mutate(vl_amostra = ajusta_extremos(vl_amostra, lst_param_preco$min, lst_param_preco$max)) |>
  ungroup()


# Tabela de amostras independentes -----------------------------------------------------

tab_amostra_independente <- bind_rows(
  tab_custo,
  tab_producao,
  tab_preco
)

saveRDS(tab_amostra_independente, "data-raw/tab_amostra_independente.rds")


# Custo x Produção --------------------------------------------------------

tab_grid_copula <- expand_grid(
  cd_distribuicao1 = c("unif", "norm", "sn_pos", "sn_neg"),
  cd_distribuicao2 = c("unif", "norm", "sn_pos", "sn_neg")
)

lst_distribuicao_param <- list(
  unif = c("min", "max"),
  norm = c("mean", "sd"),
  sn_pos = c("mean", "sd", "skew"),
  sn_neg = c("mean", "sd", "skew")
)

vec_corr <- seq(0.1, 0.9, 0.2)

lst_amostra_dependente <- map(seq_len(nrow(tab_grid_copula)), ~vector("list", length(vec_corr)))

i = 11
ii = 4

for (i in seq_len(nrow(tab_grid_copula))) {

  i_margins <-  c(tab_grid_copula$cd_distribuicao1[i], tab_grid_copula$cd_distribuicao2[i])

  i_is_skew <- str_detect(i_margins, "sn")
  i_is_skew_neg <- str_detect(i_margins, "sn_neg")

  i_custo_param <- lst_param_custo[lst_distribuicao_param[[i_margins[1]]]]
  i_producao_param <- lst_param_producao[lst_distribuicao_param[[i_margins[2]]]]

  i_lst_param_margins <- list(i_custo_param, i_producao_param) |>
    map_if(i_is_skew, \(x) as.numeric(x) |> cp2dp("SN") |> as.list()) |>
    map_if(i_is_skew_neg, \(x) { x$alpha <- x$alpha * -1; x})

  for (ii in seq_along(vec_corr)) {

    ii_copula_normal <- normalCopula(param = vec_corr[ii], dim = 2)

    ii_mult_dist_copula <- mvdc(
      copula = ii_copula_normal,
      margins = str_remove(i_margins, "(_pos)|(_neg)"),
      paramMargins = i_lst_param_margins
    )

    ii_samples <- rMvdc(N, ii_mult_dist_copula)

    ii_custo <- ajusta_extremos(ii_samples[ , 1], lst_param_custo$min, lst_param_custo$max)
    ii_producao <- ajusta_extremos(ii_samples[ , 2], lst_param_producao$min, lst_param_producao$max)

    # plot(ii_samples[ , 1], ii_samples[ , 2])

    lst_amostra_dependente[[i]][[ii]] <- bind_rows(
      tibble(
        cd_var = "custo",
        nm_var = "Custo de formação",
        cd_marginais = str_glue("{i_margins[1]} - {i_margins[2]}"),
        vl_correlacao = vec_corr[ii],
        vl_amostra = ii_custo
      ),
      tibble(
        cd_var = "producao",
        nm_var = "Producao de madeira",
        cd_marginais = str_glue("{i_margins[1]} - {i_margins[2]}"),
        vl_correlacao = vec_corr[ii],
        vl_amostra = ii_producao
      )
    )
  }

}


# Tabela de amostras dependentes -----------------------------------------------------

tab_amostra_dependente <- bind_rows(lst_amostra_dependente)

saveRDS(tab_amostra_dependente, "data-raw/tab_amostra_dependente.rds")


# Validação ---------------------------------------------------------------

# Dados independentes
plt_hist_amostra_dist_var <- tab_amostra_independente |>
  ggplot(aes(vl_amostra)) +
    geom_histogram() +
    facet_grid(cd_distribuicao ~ nm_var, scales = "free_x") +
    theme_bw()

ggsave(
  "data-raw/plt_hist_amostra_dist_var.png",
  plt_hist_amostra_dist_var,
  width = 6, height = 6
)


# Dados dependentes
plt_scatter_custo_producao_correlacao_var <- tab_amostra_dependente |>
  group_by(cd_var, cd_marginais) |>
  mutate(id = row_number()) |>
  ungroup() |>
  select(-nm_var) |>
  spread(cd_var, vl_amostra) |>
  #slice(1:500) |>
  ggplot(aes(custo , producao)) +
  geom_density2d_filled(bins = 5) +
  #geom_point(alpha = 0.05) +
  facet_grid(vl_correlacao~cd_marginais) +
  scale_fill_viridis_d('Percentil', labels = scales::percent(seq(0, 1, 0.2))) +
  theme_bw()

ggsave(
  "data-raw/plt_scatter_custo_producao_correlacao_var.png",
  plt_scatter_custo_producao_correlacao_var,
  width = 16, height = 5
)


# Salva tabela -------------------------------------------------------------

usethis::use_data(tab_amostra_independente, tab_amostra_dependente, overwrite = TRUE)
