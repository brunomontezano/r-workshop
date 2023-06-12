# Carregar pacotes
library(tidymodels)
library(dados)

# Criar gráfico para análise exploratória
pinguins |> 
  filter(!is.na(sexo)) |> 
  ggplot(aes(x = profundidade_bico,
             y = comprimento_bico,
             color = sexo,
             size = massa_corporal)) +
  geom_point(alpha = 0.55) +
  facet_wrap(~ especie) +
  ggsci::scale_color_nejm(labels = c("Fêmea", "Macho")) +
  theme_classic(15) +
  labs(x = "Profundidade do bico (mm)",
       y = "Comprimento do bico (mm)",
       color = "Sexo",
       size = "Massa corporal (g)")

# Limpar os dados
dados_pinguins <- pinguins |> 
  filter(!is.na(sexo)) |> 
  select(-ano, -ilha)

# Visualizar dados limpos
dados_pinguins

# Dividir dados entre treino e teste
set.seed(1)

divisao_pinguins <- initial_split(dados_pinguins, strata = "sexo", prop = 0.75)
treino_pinguins <- training(divisao_pinguins)
teste_pinguins <- testing(divisao_pinguins)

divisao_pinguins

# Criar reamostragens por bootstrapping
set.seed(1)

boot_pinguins <- bootstraps(treino_pinguins, times = 25)

boot_pinguins

# Especificar o modelo de regressão logística
mod_log <- logistic_reg() |> 
  set_mode("classification") |> 
  set_engine("glm")

mod_log

# Especificar o fluxo de trabalho (workflw) com nossa fórmula
wf_pinguins <- workflow() |> 
  add_formula(sexo ~ .)

wf_pinguins

# Ajustar o modelo nas reamostragens
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

# Coletar métricas de desempenho nas reamostragens
collect_metrics(rs_pinguins)

# Olhar matriz de confusão nas reamostragens
rs_pinguins |> 
  conf_mat_resampled()

# Criar curva ROC das reamostragens
rs_pinguins |> 
  collect_predictions() |> 
  group_by(id) |> 
  roc_curve(sexo, .pred_fêmea) |> 
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.2) +
  geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
  coord_equal() +
  theme_light(18) +
  labs(x = "1 - Especificidade", y = "Sensibilidade")

# Ajustar modelo final na base de treino e testar no conjunto de teste
final_pinguins <- wf_pinguins |> 
  add_model(mod_log) |> 
  last_fit(divisao_pinguins)

# Coletar métricas no modelo final
collect_metrics(final_pinguins)

# Verificar matriz de confusão final
collect_predictions(final_pinguins) |> 
  conf_mat(sexo, .pred_class)

# Olhar os odds ratio da regressão logística
final_pinguins |> 
  extract_workflow() |> 
  tidy(exponentiate = TRUE) |> 
  arrange(desc(estimate))

# Olhar importância de variáveis
final_pinguins |> 
  extract_fit_engine() |> 
  vip::vip(geom = "point")
