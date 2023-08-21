library(vet)
library(dplyr)
library(tidyr)
library(stringr)
library(ggridges)
library(ggplot2)
library(patchwork)
library(scales)
library(purrr)

N = 10000

TAXA = 0.10
ROTACAO = 6
PRODUCAO = 240
PRECO = 150



# marginais ---------------------------------------------------------------

tab_amostra_independente_filtro <- tab_amostra_independente |>
  filter(cd_var %in% c("custo", "producao"), cd_distribuicao %in% c("norm", "unif")) |>
  mutate(tipo = "Independente") |>
  select(tipo, cd_var, cd_distribuicao, vl_amostra)

tab_amostra_dependente_filtro <- tab_amostra_dependente |>
  filter(cd_marginais %in% c("norm - norm", "unif - unif"), vl_correlacao == 0.1) |>
  mutate(tipo = "Dependente", cd_distribuicao = str_extract(cd_marginais, "\\w+")) |>
  select(tipo, cd_var, cd_distribuicao, vl_amostra)

tab_amostra_filtro <- bind_rows(tab_amostra_independente_filtro, tab_amostra_dependente_filtro)

tab_amostra_filtro |>
  ggplot(aes(vl_amostra, fill = tipo)) +
  geom_density(alpha = 0.5) +
  facet_wrap(cd_distribuicao~cd_var, scales = "free") +
  theme_bw()


# vet ---------------------------------------------------------------------



vec_aux_dist <- tab_amostra_independente |>
  distinct(cd_distribuicao) |>
  pull()

tab_grid_independente <- expand_grid(
  custo = vec_aux_dist,
  producao = vec_aux_dist,
)


# Independente ------------------------------------------------------------

lst_vet_independente <- tab_grid_independente %>%
  mutate(vl_vet = NA)

i = 1
for (i in seq_len(nrow(tab_grid_independente))) {

  lst_vet_independente$vl_vet[i] <- vet(
      taxa = TAXA,
      rotacao = ROTACAO,
      vp_custo = tab_amostra_independente |>
        filter(cd_var == "custo", cd_distribuicao == tab_grid_independente$custo[i]) |>
        pull(vl_amostra),
      producao = tab_amostra_independente |>
        filter(cd_var == "producao", cd_distribuicao == tab_grid_independente$producao[i]) |>
        pull(vl_amostra),
      preco_madeira = PRECO
    ) |>
    list()
}

tab_vet_independente <- unnest(lst_vet_independente, vl_vet)


# Dependente --------------------------------------------------------------

vec_aux_corr <- c("0.1", "0.3", "0.5", "0.7", "0.9")
#vec_aux_corr <- c("0.1", "0.9")

tab_grid_dependente <- expand_grid(
    custo = vec_aux_dist,
    producao = vec_aux_dist,
    correlacao = vec_aux_corr
  ) |>
  mutate(cd_marginais = paste(custo, producao, sep = " - "))

lst_vet_dependente <- tab_grid_dependente %>%
  mutate(vl_vet = NA)

i = 1
for (i in seq_len(nrow(tab_grid_dependente))) {

    lst_vet_dependente$vl_vet[i] <- vet(
      taxa = TAXA,
      rotacao = ROTACAO,
      vp_custo = tab_amostra_dependente |>
        filter(
          cd_var == "custo",
          cd_marginais == tab_grid_dependente$cd_marginais[i],
          as.character(vl_correlacao) == tab_grid_dependente$correlacao[i]
        ) |>
        pull(vl_amostra),
      producao = tab_amostra_dependente |>
        filter(
          cd_var == "producao",
          cd_marginais == tab_grid_dependente$cd_marginais[i],
          as.character(vl_correlacao) == tab_grid_dependente$correlacao[i]
        ) |>
        pull(vl_amostra),
      preco_madeira = PRECO
    ) |>
    list()
}

tab_vet_dependente <- unnest(lst_vet_dependente, vl_vet)

tab_vet_compara_dependencia <- tab_grid_dependente |>
  left_join(
    tab_vet_dependente |>
      select(-cd_marginais) |>
      group_nest(custo, producao, correlacao, .key = "data_dependente")
  ) |>
  left_join(
    tab_vet_independente |>
      group_nest(custo, producao, .key = "data_independente")
  ) |>
  mutate(
    ks_test = map2(data_dependente, data_independente, ~ks.test(.x$vl_vet, .y$vl_vet)),
    ks_pvlue = map_dbl(ks_test,~.x$p.value),
    d = map_dbl(ks_test,~.x$statistic, 5)
  )

tab_vet_compara_dependencia |>
  arrange(-ks_pvlue, d) |>
  filter(ks_pvlue >= 0.05) |>
  print(n=80)

tab_vet_compara_dependencia |>
  #arrange(d) |>
  #filter(!ks_pvlue >= 0.05) |>
  slice(5) |>
  select(starts_with("data")) |>
  unnest() |>
  gather() |>
  ggplot(aes(value, fill = key)) +
  geom_density(alpha = 0.5) +
  theme_bw()

tab_vet_compara_dependencia |>
  slice(28) |>
  select(starts_with("data")) |>
  unnest() |>
  gather() |>
  ggplot(aes(value, fill = key)) +
    geom_density(alpha = 0.5) +
    theme_bw()

tab_vet_compara_dependencia$ks_test[[2]]$statistic

