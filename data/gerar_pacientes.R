set.seed(1)
pacientes <- fabricatr::fabricate(
  N = 700,
  idade = round(runif(N, 18, 35)),
  dep_baseline = fabricatr::draw_binary(N = N, prob = 0.15),
  gosto_de_psico = fabricatr::draw_ordered(
    x = rnorm(N, mean = -1 + 2 * dep_baseline),
    breaks = c(-1.5, -0.5, 0.5, 1.5),
    break_labels = c("Concordo fortemente", "Concordo", "Neutro",
                     "Discordo", "Discordo fortemente")
  ),
  bipolar_follow = fabricatr::draw_binary(N = N, prob = dplyr::case_when(
    idade <= 23 & dep_baseline == 1 ~ 0.35,
    idade > 23 & dep_baseline == 1 ~ 0.7,
    idade <= 23 & dep_baseline == 0 ~ 0.05,
    idade > 23 & dep_baseline == 0 ~ 0.01,
  ))
) |> 
  tibble::as_tibble() |> 
  dplyr::mutate(gosto_de_psico = as.character(gosto_de_psico))

readr::write_csv(x = pacientes,
                 file = "data/pacientes.csv")
