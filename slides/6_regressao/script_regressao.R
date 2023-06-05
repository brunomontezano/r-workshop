#### Regressão linear ####

# Carregar pacote `ggplot2` para visualizações
library(ggplot2)

# Carregar pacote `patchwork` usado para juntar gráficos
library(patchwork)

# Carregar pacote `dplyr` para manipulação de dados
library(dplyr)

# Atribuir uma semente
set.seed(1)

# Gerar dados simulados para altura
altura <- round(runif(10, 150, 195), 1)

# Gerar dados simulados para peso
peso <- 0.5 * altura + rnorm(10, mean = 0, sd = 3)

# Criar data frame com altura e peso
medidas <- data.frame(altura, peso)

# Ajustar um modelo de regressão linear simples com peso (Y) explicado pela altura (X)
modelo <- lm(peso ~ altura,
                  data = medidas)

# Adicionar coluna na base com valores preditos pelo modelo linear ajustado acima
medidas$predito <- predict(modelo)

# Adicionar coluna com os resíduos (erro)
medidas$residuos <- medidas$peso - medidas$predito

# Criar gráfico de dispersão com altura e peso
ggplot(medidas,
       aes(x = altura, y = peso)) +
  geom_point(size = 4, color = "slateblue4") +
  scale_x_continuous(labels = \(x) paste0(as.character(x), " cm")) +
  scale_y_continuous(labels = \(x) paste0(as.character(x), " kg")) +
  geom_smooth(
    method = "lm",
    color = "grey20",
    se = FALSE,
    fullrange = TRUE
  ) +
  labs(x = "Altura",
       y = "Peso") +
  theme_minimal(base_size = 16)

#### Qual a utilidade das retas? ####

# Para rodar os códigos a seguir, você vai precisar instalar o pacote `openintro`
# Esse pacote foi criado pelos autores do livro OpenIntro Statistics
# Comando abaixo para instalar (precisa descomentar a linha):
# install.packages("openintro")

# Criar subconjuntos com cada cluster (agrupamento) de dados
neg <- openintro::simulated_scatter |> filter(group == 1)
pos <- openintro::simulated_scatter |> filter(group == 2)
ran <- openintro::simulated_scatter |> filter(group == 3)
bad <- openintro::simulated_scatter |> filter(group == 5)

# Criar gráfico com ajuste ruim de uma reta
bad_reg <- bad |>
  mutate(y = max(y) + min(y) - y) |>
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 2,
             alpha = 0.8,
             color = "slateblue4") +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "gray20") +
  labs(x = NULL,
       y = NULL) +
  theme_minimal(base_size = 16)

# Criar gráfico com relação negativa
p_neg <- ggplot(neg, aes(x = x, y = y)) +
  geom_point(size = 2, alpha = 0.8, color = "slateblue4") +
  geom_smooth(method = "lm", se = FALSE, color = "gray20") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 16)

# Criar gráfico com relação positiva
p_pos <- ggplot(pos, aes(x = x, y = y)) +
  geom_point(size = 2, alpha = 0.8, color = "slateblue4") +
  geom_smooth(method = "lm", se = FALSE, color = "gray20") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 16)

# Criar gráfico com padrões aleatórios (pouca ou nenhuma associação)
p_ran <- ggplot(ran, aes(x = x, y = y)) +
  geom_point(size = 2, alpha = 0.8, color = "slateblue4") +
  geom_smooth(method = "lm", se = FALSE, color = "gray20") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 16)

# Juntar os plots e adicionar rótulos com pacote `patchwork`
p_neg + p_pos + p_ran + bad_reg +
  plot_annotation(title = "Um modelo linear pode ser útil mesmo quando os pontos não passam diretamente pela reta.",
                  subtitle = "Em alguns casos, não será o método mais adequado.",
                  caption = "Adaptado do livro Introduction to Modern Statistics (Çetinkaya-Rundel e Hardin, 2023).",)

#### Como a reta é construída? ####

# Criar gráfico para ilustrar o intercepto (B_0) e coeficiente angular (B_1)
ggplot(medidas,
       aes(x = altura, y = peso)) +
  geom_point(size = 2.5,
             color = "slateblue4",
             alpha = 0.9) +
  scale_x_continuous(labels = \(x) paste0(as.character(x), " cm"),
                     limits = c(0, 195)) +
  scale_y_continuous(labels = \(x) paste0(as.character(x), " kg")) +
  geom_smooth(
    method = "lm",
    color = "grey20",
    se = FALSE,
    fullrange = TRUE
  ) +
  geom_point(
    aes(x = x, y = y),
    data.frame(x = 0, y = -4.47),
    size = 5,
    color = "indianred1"
  ) +
  labs(x = "Altura",
       y = "Peso") +
  geom_segment(
    x = 150,
    xend = 150,
    y = 75,
    yend = 0,
    color = "tan1",
    linewidth = 1.5
  ) +
  geom_segment(
    x = 150,
    xend = 0,
    y = 75,
    yend = 75,
    color = "tan1",
    linewidth = 1.5
  ) +
  geom_point(
    x = 150,
    y = 74.58,
    size = 5,
    color = "maroon1"
  ) +
  theme_minimal(base_size = 16)

#### Observado vs. Esperado ####

# Gráfico para ilustrar o erro, ou resíduos
ggplot(medidas,
       aes(x = altura, y = peso)) +
  geom_segment(
    aes(
      x = altura,
      y = peso,
      xend = altura,
      yend = predito
    ),
    color = "chocolate1",
    linewidth = 1.5,
    alpha = 0.8,
    data = medidas
  ) +
  geom_point(size = 3.5, color = "slateblue4") +
  scale_x_continuous(labels = \(x) paste0(as.character(x), " cm")) +
  scale_y_continuous(labels = \(x) paste0(as.character(x), " kg")) +
  geom_smooth(
    method = "lm",
    color = "grey20",
    se = FALSE,
    fullrange = TRUE
  ) +
  labs(x = "Altura",
       y = "Peso") +
  theme_minimal(base_size = 16)

#### Trabalhar com dados dos gambás ####

# Carregar pacote `readr` para importar dados
library(readr)

# Carregar dados dos gambás
# Dados estão disponíveis no link abaixo:
# https://raw.githubusercontent.com/brunomontezano/r-workshop/main/slides/dados/gambas.csv
# Você deve ler de acordo com o uso da função abaixo:
# gambas <- read_csv("../dados/gambas.csv")
# Revise os caminhos relativos e absolutos, além do diretório de trabalho para
# relembrar como ler um arquivo em csv no R

# Dar uma olhada nos dados dos gambás
glimpse(gambas)

# Ajustar uma regressão linear simples do comprimento da cabeça explicado
# pelo comprimento total
lm(comprimento_cabeca ~ comprimento_total,
   data = gambas)

# Criar gráfico de pontos/dispersão (scatterplot) do comprimento total e
# comprimento da cabeça
ggplot(gambas) +
  geom_point(aes(x = comprimento_total, y = comprimento_cabeca),
             size = 4,
             alpha = 0.7)  +
  labs(x = "Comprimento total (em cm)",
       y = "Comprimento da cabeça (em mm)") +
  theme_minimal(18)


# Exemplo de reta criada na mão
ggplot(gambas, aes(x = comprimento_total, y = comprimento_cabeca)) +
  geom_point(size = 4,
             alpha = 0.7)  +
  geom_abline(
    intercept = 36,
    slope = 0.7,
    color = "dodgerblue4",
    size = 2
  ) +
  labs(x = "Comprimento total (em cm)",
       y = "Comprimento da cabeça (em mm)") +
  theme_minimal(18)

# Gráfico que compara a reta criada na mão e a "melhor reta"
ggplot(gambas, aes(x = comprimento_total, y = comprimento_cabeca)) +
  geom_point(size = 4,
             alpha = 0.7)  +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "firebrick1",
    size = 2
  ) +
  geom_abline(
    intercept = 36,
    slope = 0.7,
    color = "dodgerblue4",
    alpha = 0.6,
    size = 2
  ) +
  labs(x = "Comprimento total (em cm)",
       y = "Comprimento da cabeça (em mm)") +
  theme_minimal(18)

# Ajustar modelo que nos dá a melhor reta
melhor_reta <- lm(comprimento_cabeca ~ comprimento_total, data = gambas)

# Mostrar modelo ajustado
melhor_reta

#### "Melhor reta" baseado em que? ####

# Atribuir uma semente
set.seed(1)

# Criar modelo da melhor reta
melhor_reta <- lm(comprimento_cabeca ~ comprimento_total, data = gambas)

# Caso necessário, instale o pacote `broom`
# install.packages("broom")

# Adicionar valores preditos pela melhor reta e pela reta feita na mão na base
gambas_com_preds <- melhor_reta |> 
  broom::augment() |> 
  rename(pred_melhor_reta = .fitted) |> 
  mutate(
    pred_reta_a_mao = 36 + 0.7 * comprimento_total
  )

# Criar gráfico com os resíduos (erro) da melhor reta
grafico_residuos_melhor_reta <- gambas_com_preds |>
  ggplot(aes(x = comprimento_total, y = comprimento_cabeca)) +
  geom_point(size = 2) +
  geom_abline(
    intercept = melhor_reta$coefficients[1],
    slope = melhor_reta$coefficients[2],
    size = 1,
    colour = "salmon"
  ) +
  geom_segment(
    aes(xend = comprimento_total, yend = pred_melhor_reta),
    colour = "purple",
    size = 0.8
  ) +
  labs(subtitle = "Resíduos da Melhor Reta",
       y = "Comprimento da cabeça (em mm)",
       x = "Comprimento total (em cm)") +
  theme_minimal(15)

# Criar gráfico com os resíduos (erro) da reta feita na mão
grafico_residuos_reta_a_mao <- gambas_com_preds |>
  ggplot(aes(x = comprimento_total, y = comprimento_cabeca)) +
  geom_point(size = 2) +
  geom_abline(
    intercept = 36,
    slope = 0.7,
    size = 1,
    colour = "orange"
  ) +
  geom_segment(
    aes(xend = comprimento_total, yend = pred_reta_a_mao),
    colour = "purple",
    size = 0.8
  ) +
  labs(subtitle = "Resíduos da Reta Escolhida a Mão",
       y = "Comprimento da cabeça (em mm)",
       x = "Comprimento total (em cm)") +
  theme_minimal(15)

# Juntar gráficos com `patchwork` e adicionar rótulos e tema
grafico_residuos_melhor_reta + grafico_residuos_reta_a_mao +
  plot_annotation(title = "Os segmentos roxos são os resíduos (ou o quanto o modelo errou naqueles pontos).",
                  theme = theme_minimal(15))

#### Regressão linear múltipla ####

# Ajustar modelo de regressão linear múltipla no formato
# Comprimento da cabeça = b_0 + b_1comprimento_total + b_2comprimento_cauda
lm(comprimento_cabeca ~ comprimento_total + comprimento_cauda,
   data = gambas)

# Atribuir semente
set.seed(1)

# Caso necessário, instale o pacote `plotly`
# install.packages("plotly")

# Carregar o pacote `plotly` para fazer gráficos interativos
library(plotly)

# Criar variáveis
x <- gambas$comprimento_total
y <- gambas$comprimento_cauda
z <- gambas$comprimento_cabeca

# Ajustar modelo para gráfico 3D
fit <- lm(z ~ x + y)

# Criar número de linhas na grade (grid)
grid.lines <- 26

# Criar objetos das predições para limites dos eixos
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)

# Criar um data frame do x e y (grid)
xy <- expand.grid(x = x.pred, y = y.pred)

# Predizer pontos do desfecho
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)

# Criar gráfico com `plotly`
fig <- plot_ly(data = gambas) |>
  add_trace(
    x = ~ comprimento_total,
    y = ~ comprimento_cauda,
    z = ~ comprimento_cabeca,
    type = "scatter3d",
    mode = "markers",
    opacity = .8
  ) |>
  add_trace(
    z = z.pred,
    x = x.pred,
    y = y.pred,
    type = "surface",
    opacity = .9
  ) |>
  layout(scene = list(
    xaxis = list(title = "Total (cm)"),
    yaxis = list(title = "Cauda (cm)"),
    zaxis = list(title = "Cabeça (mm)")
  )) |>
  hide_legend() |>
  hide_colorbar()

# Mostrar gráfico criado acima
fig

#### Criando modelo linear múltiplo no R ####

# Ajustar modelo linear múltiplo no R
modelo_linear_multiplo <-
  lm(comprimento_cabeca ~ comprimento_total + comprimento_cauda,
     data = gambas)

# Mostrar resumo do modelo
summary(modelo_linear_multiplo)

# Extrair o R^2 (coeficiente de determinação)
summary(modelo_linear_multiplo)$r.squared

# Extrair o R^2 ajustado
# O R^2 ajustado pune pela quantidade de preditores no modelo
summary(modelo_linear_multiplo)$adj.r.squared

#### Preditores categóricos ####

# Criar box plot do comprimento da nadadeira pelo sexo
dados::pinguins |>
  filter(!is.na(sexo)) |>
  mutate(sexo = if_else(sexo == "macho", "Macho", "Fêmea")) |>
  ggplot(aes(y = comprimento_nadadeira, x = sexo, fill = sexo)) +
  geom_boxplot(show.legend = FALSE) +
  labs(y = "Comprimento da nadadeira (mm)",
       x = "Sexo") +
  theme_minimal(20)

# Atribuir semente
set.seed(1)

# Criar gráfico de dispersão do comprimento da nadadeira e sexo
dados::pinguins |>
  filter(!is.na(sexo)) |>
  mutate(sexo_macho = if_else(sexo == "macho", 1, 0)) |>
  ggplot(aes(y = comprimento_nadadeira, x = sexo_macho)) +
  geom_jitter(
    width = 0.05,
    height = 0,
    alpha = 0.7,
    size = 2
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "#0E84B4FF",
    linewidth = 1.5
  ) +
  labs(y = "Comprimento da nadadeira (mm)",
       x = "Sexo (macho)") +
  scale_x_continuous(breaks = 0:1) +
  theme_minimal(20)


# Ajustar o modelo com preditor categórico binário (dicotômico)
# Comprimento da nadadeira explicado por Sexo
lm(comprimento_nadadeira ~ sexo, data = dados::pinguins)

# Preditores com 3 ou mais categorias

# Criar box plot do comprimento da nadadeira com espécie
dados::pinguins |>
  ggplot(aes(y = comprimento_nadadeira, x = especie, fill = especie)) +
  geom_boxplot(show.legend = FALSE) +
  labs(y = "Comprimento da nadadeira (mm)",
       x = "Espécie") +
  theme_minimal(20)

# Ajustar modelo com preditor categórico de três categorias
# Comprimento da nadadeira explicado pela Espécie
lm(comprimento_nadadeira ~ especie, data = dados::pinguins)

#### Heteroscedasticidade ####

# Atribuir uma semente
set.seed(1)

# Criar dados simulados com heteroscedasticidade
hetero_df <- tibble(
  x = runif(50),
  y = rnorm(50, mean = x, sd = x * 0.5)
)

# Caso queira criar os gráficos a seguir, instalar o pacote `latex2exp`
# install.packages("latex2exp")

# Criar gráfico sem transformação, apresentando heteroscedasticidade
cru <- ggplot(hetero_df, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal(20) +
  labs(x = unname(latex2exp::TeX("$x_1$")), y = "Y",
       title = unname(latex2exp::TeX("$Y = \\beta_0 + \\beta_1x_1$")))

# Mostrar gráfico criado acima
cru

# Criar gráfico com desfecho transformado através de raíz quadrada
# Perceba que a variância dos resíduos (erro) ficam mais homogêneos
raiz <- ggplot(hetero_df, aes(x = x, y = sqrt(y))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal(20) +
  labs(x = unname(latex2exp::TeX("$x_1$")),
       y = unname(latex2exp::TeX("$\\sqrt{Y}")),
       title = unname(latex2exp::TeX("$\\sqrt{Y} = \\beta_0 + \\beta_1x_1$")))

# Juntar ambos os gráficos com `patchwork` para comparação
cru + raiz

#### Multicolinearidade ####

# Atribuir semente
set.seed(1)

# Criar dados simulados de área, número de cômodos e preço dos imóveis
area <- rnorm(100, mean = 1500, sd = 500)
comodos <- round(rnorm(100, mean = 3 + 0.5 * area/100, sd = 1), 0)
preco <- rnorm(100, mean = 200000 + 100 * area + 50000 * comodos, sd = 50000)
area <- area / 10

# Juntar os dados em um data frame
dados <- data.frame(area, comodos, preco)

# Criar gráfico que demonstra multicolinearidade entre área e cômodos
ggplot(dados, aes(x = area, y = preco, color = comodos)) +
  geom_point() +
  scale_color_viridis_c(name = "Cômodos") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ",
                                                    big.mark = ".")) +
  scale_x_continuous(labels = \(x) unname(latex2exp::TeX(paste0(
    as.character(x), "$m^2$"
  )))) +
  theme_minimal(20) +
  labs(x = "Área",
       y = "Preço do imóvel")


# Carregar pacote `car` para calcular variance inflation factor (VIF)
library(car)

# Ajustar modelo do preço do imóvel explicado pela área e número de cômodos
regressao_linear <- lm(preco ~ area + comodos,
                       data = dados)

# Calcular VIF dos preditores do modelo
vif(regressao_linear)

#### Outliers ####

# Atribuir semente
set.seed(1)

# Criar dados simulados e juntar em um data frame
x <- 1:10
y <- 2*x + rnorm(10, mean = 0, sd = 1)
df <- data.frame(x, y)

# Criando o gráfico sem outlier
p_sem_outlier <- ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(limits = c(0, 12.5)) +
  scale_y_continuous(limits = c(0, 50)) +
  theme_minimal(18) +
  labs(x = unname(latex2exp::TeX("$x_1$")),
       y = unname(latex2exp::TeX("$Y$")))

# Adicionando um outlier aos dados
df[11, ] <- c(12, 50)

# Criando o gráfico com outlier
p_com_outlier <- ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal(18) +
  scale_x_continuous(limits = c(0, 12.5)) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x = unname(latex2exp::TeX("$x_1$ com \\textit{outlier}")),
       y = NULL)

# Juntar ambos os gráficos com `patchwork`
p_sem_outlier + p_com_outlier

# Criar modelo com dados que possuem outlier
modelo_com_outlier <- lm(y ~ x, data = df)

# Calcular a Distância de Cook para cada observação
cooks.distance(modelo_com_outlier)

# Calcular a medida de 4/n para comparar os valores de Distância de Cook calculados acima
4 / nrow(df)

# Atribuir as Distâncias de Cook em um objeto
distancia_cook <- cooks.distance(modelo_com_outlier)

# Criar data frame com as Distâncias para cada observação
df_cook <- data.frame(distancia_cook, row.names = NULL)

# Criar gráfico que mostra como comparar a Distância de Cook ao ponto de corte (4/n)
ggplot(df_cook, aes(x = as.numeric(row.names(df_cook)), y = distancia_cook)) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:11) +
  geom_hline(
    yintercept = 4 / nrow(df_cook),
    color = "darkred",
    size = 2
  ) +
  labs(x = "Observação", y = "Distância de Cook") +
  theme_minimal(20)