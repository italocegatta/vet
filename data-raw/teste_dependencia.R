library(vet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)
library(patchwork)
library(purrr)

TAXA = 0.10
ROTACAO = 6
PRODUCAO = 240
PRECO = 150

# analise custo e producao

tab_amostra_independente_filtro <- tab_amostra_independente |>
  filter(cd_var %in% c("custo", "producao"), cd_distribuicao %in% c("norm", "unif")) |>
  mutate(tipo = "Independente") |>
  select(tipo, cd_var, cd_distribuicao, vl_amostra)

tab_amostra_dependente_filtro <- tab_amostra_dependente |>
  filter(cd_marginais %in% c("norm - norm", "unif - unif"), vl_correlacao == 0.5) |>
  mutate(tipo = "Dependente", cd_distribuicao = str_extract(cd_marginais, "\\w+")) |>
  select(tipo, cd_var, cd_distribuicao, vl_amostra)

tab_amostra_filtro <- bind_rows(tab_amostra_independente_filtro, tab_amostra_dependente_filtro)

tab_amostra_filtro |>
  ggplot(aes(vl_amostra, fill = tipo)) +
  geom_density(alpha = 0.5) +
  facet_wrap(cd_distribuicao~cd_var, scales = "free") +
  theme_bw()

# vec_aux_corr <- c("0.1", "0.3", "0.5", "0.7", "0.9")
vec_aux_corr <- 1 # não responde ao looping

lst_vet_compara_dependencia <- vector("list", length(vec_aux_corr))

i = 1
for (i in seq_along(vec_aux_corr)) {

  vec_vet_dependente_norm = vet(
    taxa = TAXA,
    rotacao = ROTACAO,
    vp_custo = tab_amostra_filtro |>
      filter(cd_var == "custo", cd_distribuicao == "norm", tipo == "Dependente") |>
      pull(vl_amostra),
    producao = tab_amostra_filtro |>
      filter(cd_var == "producao", cd_distribuicao == "norm", tipo == "Dependente") |>
      pull(vl_amostra),
    preco_madeira = PRECO
  )

  vec_vet_dependente_unif = vet(
    taxa = TAXA,
    rotacao = ROTACAO,
    vp_custo = tab_amostra_filtro |>
      filter(cd_var == "custo", cd_distribuicao == "unif", tipo == "Dependente") |>
      pull(vl_amostra),
    producao = tab_amostra_filtro |>
      filter(cd_var == "producao", cd_distribuicao == "unif", tipo == "Dependente") |>
      pull(vl_amostra),
    preco_madeira = PRECO
  )

  vec_vet_independente_norm = vet(
    taxa = TAXA,
    rotacao = ROTACAO,
    vp_custo = tab_amostra_filtro |>
      filter(cd_var == "custo", cd_distribuicao == "norm", tipo == "Independente") |>
      pull(vl_amostra),
    producao = tab_amostra_filtro |>
      filter(cd_var == "producao", cd_distribuicao == "norm", tipo == "Independente") |>
      pull(vl_amostra),
    preco_madeira = PRECO
  )

  vec_vet_independente_unif = vet(
    taxa = TAXA,
    rotacao = ROTACAO,
    vp_custo = tab_amostra_filtro |>
      filter(cd_var == "custo", cd_distribuicao == "unif", tipo == "Independente") |>
      pull(vl_amostra),
    producao = tab_amostra_filtro |>
      filter(cd_var == "producao", cd_distribuicao == "unif", tipo == "Independente") |>
      pull(vl_amostra),
    preco_madeira = PRECO
  )

  lst_vet_compara_dependencia[[i]] <- tribble(
    ~lg_dependencia, ~cd_marginais, ~vl_vet,
    TRUE, "NORM", vec_vet_dependente_norm,
    TRUE, "UNIF", vec_vet_dependente_unif,
    FALSE, "NORM", vec_vet_independente_norm,
    FALSE, "UNIF", vec_vet_independente_unif,
  ) |>
    unnest(vl_vet) |>
    mutate(vl_correlacao = vec_aux_corr[i])

}

tab_vet_compara_dependencia <- bind_rows(lst_vet_compara_dependencia)

tab_vet_compara_dependencia |>
  ggplot(aes(vl_vet, fill = lg_dependencia)) +
  geom_density(alpha = 0.5) +
  facet_grid(vl_correlacao~cd_marginais) +
  theme_bw()
