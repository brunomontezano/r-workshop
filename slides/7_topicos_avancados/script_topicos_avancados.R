# Carregar pacotes para ajustar árvore de decisão e dados do Titanic
library(rpart)
library(rpart.plot)
library(titanic)

# Recodificar algumas variáveis
titanic_treino <- titanic_train |> 
  dplyr::mutate(
    Survived = factor(
      dplyr::case_when(
        Survived == 1 ~ "Sobreviveu",
        Survived == 0 ~ "Faleceu"
      )
    ),
    Sex = dplyr::case_when(
      Sex == "male" ~ "Masculino",
      Sex == "female" ~ "Feminino"
    )
  ) |> 
  dplyr::rename(Sexo = Sex,
                Idade = Age)

# Ajustar a árvore de decisão
titanic_ajuste <- rpart(Survived ~ Sexo + Idade,
                               data = titanic_treino)

# Mostrar gráfico da árvore
rpart.plot(titanic_ajuste,
           extra = 6,
           fallen.leaves = TRUE,
           yes.text = "Sim",
           no.text = "Não",
           main = "Sobrevivência no desastre do Titanic: um exemplo de árvore de decisão")

library(ggplot2)

set.seed(123)
dados_aprendizado <- data.frame(
  X = c(rnorm(50, mean = 0), rnorm(50, mean = 3)),
  Y = c(rnorm(50, mean = 0), rnorm(50, mean = 3)),
  Tipo = rep(c("Grupo A", "Grupo B"), each = 50)
)

p_supervisionado <- ggplot(dados_aprendizado, aes(x = X, y = Y, color = Tipo)) +
  geom_point(alpha = 0.8,
             size = 2.2) +
  labs(title = "Aprendizado Supervisionado") +
  theme_light(16, "Charter") +
  ggsci::scale_color_jama() +
  theme(legend.position = "none") +
  labs(x = latex2exp::TeX("$x_1$"),
       y = latex2exp::TeX("$x_2$"))

p_nao_supervisionado <- ggplot(dados_aprendizado, aes(x = X, y = Y)) +
  geom_point() +
  labs(title = "Aprendizado Não Supervisionado") +
  theme_light(16, "Charter") +
  ggforce::geom_mark_circle(aes(fill = Tipo)) +
  ggsci::scale_fill_jama() +
  labs(x = latex2exp::TeX("$x_1$"),
       y = latex2exp::TeX("$x_2$")) +
  theme(legend.position = "none")

library(patchwork)
p_supervisionado + p_nao_supervisionado

set.seed(123)
dados_regressao <- data.frame(
  X = seq(1, 10, by = 0.5),
  Y = seq(2, 20, by = 1)
)

set.seed(123)
dados_classificacao <- data.frame(
  X = c(rnorm(300, mean = 2, sd = 1.5), rnorm(300, mean = 8, sd = 3)),
  Y = c(rep(0, 300), rep(1, 300))
)

set.seed(123)
p_regressao <- ggplot(dados_regressao, aes(x = X, y = Y)) +
  geom_jitter(alpha = 0.8,
              size = 2.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Problema de Regressão") +
  theme_light(18, "Charter") +
  labs(x = latex2exp::TeX("$x_1"),
       y = latex2exp::TeX("$Y$"))

set.seed(123)
p_classificacao <- ggplot(dados_classificacao, aes(x = X, y = Y)) +
  geom_jitter(aes(color = as.factor(Y)), size = 2.5,
             alpha = 0.4,
             height = 0.002,
             width = 0) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se = FALSE) +
  ggsci::scale_color_npg() +
  labs(title = "Problema de Classificação") +
  theme_light(18, "Charter") +
  theme(legend.position = "none") +
  labs(x = latex2exp::TeX("$x_1"),
       y = latex2exp::TeX("$Y$")) +
  scale_y_continuous(breaks = 0:1)

p_regressao + p_classificacao

library(dados)
library(dplyr)

pinguins |> 
  filter(!is.na(sexo)) |> 
  ggplot(aes(x = profundidade_bico,
             y = comprimento_bico,
             color = sexo,
             size = massa_corporal)) +
  geom_point(alpha = 0.55) +
  facet_wrap(~ especie) +
  ggsci::scale_color_nejm(labels = c("Fêmea", "Macho")) +
  theme_classic(12, "Charter") +
  labs(x = "Profundidade do bico (mm)",
       y = "Comprimento do bico (mm)",
       color = "Sexo",
       size = "Massa corporal (g)")

library(tidymodels)

dados_pinguins <- pinguins |> 
  filter(!is.na(sexo)) |> 
  select(-ano, -ilha)

dados_pinguins

set.seed(1)

divisao_pinguins <- initial_split(dados_pinguins, strata = "sexo", prop = 0.75)
treino_pinguins <- training(divisao_pinguins)
teste_pinguins <- testing(divisao_pinguins)

divisao_pinguins

set.seed(1)

boot_pinguins <- bootstraps(treino_pinguins, times = 25)

boot_pinguins

mod_log <- logistic_reg() |> 
  set_engine("glm")

mod_log

wf_pinguins <- workflow() |> 
  add_formula(sexo ~ .)

wf_pinguins

rs_pinguins <- wf_pinguins |> 
  add_model(mod_log) |> 
  fit_resamples(
    resamples = boot_pinguins,
    control = control_resamples(
      save_pred = TRUE,
      verbose = TRUE
    )
  )

rs_pinguins

collect_metrics(rs_pinguins)

rs_pinguins |> 
  conf_mat_resampled()

rs_pinguins |> 
  collect_predictions() |> 
  group_by(id) |> 
  roc_curve(sexo, .pred_fêmea) |> 
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.2) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  coord_equal() +
  theme_light(12, "Charter") +
  labs(x = "1 - Especificidade", y = "Sensibilidade")

final_pinguins <- wf_pinguins |> 
  add_model(mod_log) |> 
  last_fit(divisao_pinguins)

collect_metrics(final_pinguins)

collect_predictions(final_pinguins) |> 
  conf_mat(sexo, .pred_class)

final_pinguins |> 
  extract_workflow() |> 
  tidy(exponentiate = TRUE) |> 
  arrange(desc(estimate))
