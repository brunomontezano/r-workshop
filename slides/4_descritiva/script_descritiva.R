#### Dados ####

# Carregar o pacote `dados`
library(dados)

# Carregar o pacote `tidyverse`
library(tidyverse)

# Dar uma olhada na base de dados dos `pinguins` do pacote `dados`
glimpse(pinguins)

#### Somatório ####

# Criar um vetor com cinco elementos
x <- c(9, 12, 12, 14, 27)

# Solicitar o somatório do vetor `x` criado acima
sum(x)

#### Medidas de tendências central ####

# Média

# Pedindo a média da massa e do comprimento do bico dos pinguins
# Note que fazemos uso da função `summarise()` do pacote `dplyr`
pinguins |>
  summarise(
    media_massa = mean(massa_corporal, na.rm = TRUE),
    media_comprimento_bico = mean(comprimento_bico, na.rm = TRUE)
  )

# Mediana

# Solicitar mediana da massa e do comprimento do bico
pinguins |> 
  summarise(mediana_massa = median(massa_corporal, na.rm = TRUE),
            mediana_comprimento_bico = median(comprimento_bico, na.rm = TRUE))

# Moda

# Podemos ver a frequência de uma variável com a função count()
# do pacote `dplyr`
pinguins |> 
  count(especie)

# E podemos retornar a moda filtrando esse resultado com filter()
# Mantemos observações onde `n` é igual ao valor máximo de `n`
pinguins |> 
  count(especie) |> 
  filter(n == max(n))

# Com o R base, podemos fazer esta operação de outra forma:

# Criar tabela de frequência com a variável `especie`
table(pinguins$especie)

# Criar uma linha de código que nos retorna a moda de um determinado vetor
table(pinguins$especie)[table(pinguins$especie) == max(table(pinguins$especie))]

#### Valores extremos ####

# Criar um vetor `z`
z <- c(2, 5, 3, 5, 105)

# A média é sensível aos valores extremos
mean(z)

# Já a mediana, não é
median(z)

#### Medidas de dispersão ####

# Variância

# Calcular a variância da massa dos pinguins na mão e através da função `var()`
# Note que os valores retornados são os mesmos
pinguins |> 
  filter(!is.na(massa_corporal)) |> 
  summarise(variancia_na_mao_massa = sum(
    (massa_corporal - mean(massa_corporal))^2 /
      (length(massa_corporal) - 1)),
    variancia_massa = var(massa_corporal)
    )

# Desvio padrão

# Calcular desvio padrão da massa na mão e através da função `sd()`
# `sd()` faz menção a "standard deviation", que é desvio padrão em inglês
pinguins |> 
  filter(!is.na(massa_corporal)) |> 
  summarise(desvio_padrao_na_mao_massa = sqrt(var(massa_corporal)),
    desvio_padrao_massa = sd(massa_corporal)
    )

# Valor mínimo, máximo e amplitude

# Calcular valor mínimo, máximo e amplitude da massa corporal dos pinguins
pinguins |>
  filter(!is.na(massa_corporal)) |>
  summarise(
    massa_max = max(massa_corporal),
    massa_min = min(massa_corporal),
    massa_amplitude = max(massa_corporal) - min(massa_corporal)
  )

# Intervalo interquartil

# Calcular o intervalo interquartil, o primeiro quartil e o terceiro quartil
# da massa corporal dos pinguins
pinguins |>
  filter(!is.na(massa_corporal)) |>
  summarise(
    intervalo_interquartil_massa = IQR(massa_corporal),
    primeiro_quartil = quantile(massa_corporal)["25%"],
    terceiro_quartil = quantile(massa_corporal)["75%"]
  )

#### Distribuição normal ####

# Gerando uma amostra aleatória de uma distribuição normal padrão
set.seed(1)
amostra <- rnorm(1000)

# Calculando a média e o desvio padrão da amostra
media <- mean(amostra)
desvio_padrao <- sd(amostra)

# Criando um data frame com os limites das proporções 68-95-99
# 68% da população está dentro de 1 desvio padrão da média
# 95% da população está dentro de 2 desvios padrão da média
# 99,7% da população está dentro de 3 desvios padrão da média
proporcoes <- data.frame(
  limite_inf = c(media - desvio_padrao, media - 2*desvio_padrao, media - 3*desvio_padrao),
  limite_sup = c(media + desvio_padrao, media + 2*desvio_padrao, media + 3*desvio_padrao),
  proporcao = c(0.68, 0.95, 0.99)
)

# Criando o histograma com linhas indicativas
ggplot(data.frame(x = amostra), aes(x)) +
  geom_histogram(bins = 20,
                 color = "black",
                 fill = "lightblue") +
  geom_vline(
    xintercept = c(media, proporcoes$limite_inf, proporcoes$limite_sup),
    color = c(
      "#00468bff",
      "#925e9fff",
      "#42b540ff",
      "#ed0000ff",
      "#925e9fff",
      "#42b540ff",
      "#ed0000ff"
    ),
    linetype = c(
      "solid",
      "dashed",
      "dashed",
      "dashed",
      "dashed",
      "dashed",
      "dashed"
    ),
    size = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
    alpha = c(1, 1, 1, 1, 1, 1, 1)
  ) +
  theme_bw(base_size = 16) +
  labs(x = "Valores da amostra", y = "Frequência")

# Testar normalidade através do teste de Shapiro-Wilk
# Se o valor p for menor 0,05, há evidência suficiente para dizer que a
# amostra não vem de uma população normalmente distribuída
shapiro.test(amostra)

# Nesse caso, parece que a amostra simulada segue uma distribuição normal

# Testar a normalidade da distribuição da massa corporal dos pinguins
pinguins |> 
  pull(massa_corporal) |> 
  shapiro.test()

# Parece que a massa dos pinguins não segue uma distribuição normal

#### Assimetria ####

# Atribuir uma semente
# Isso é feito para conseguirmos replicar os resultados quando usamos
# algum processo de geração de números aleatórios, como é o caso
set.seed(1)

# Criar um gráfico que exemplifica uma distribuição assimétrica
tibble(x = rchisq(1000, 10)) |> 
  ggplot() +
  aes(x = x) +
  geom_histogram() +
  labs(x = "Valores", y = "Frequência") +
  ggthemes::theme_clean(base_size = 16)

#### Curtose ####

# Atribuir semente novamente
set.seed(1)

# Criar dados simulados de uma distribuição com alta curtose
alta_curtose <- tibble(valores = rnorm(1000, mean = 0, sd = 1) * 5,
                       curtose = "Alta")

# Criar dados simulados de uma distribuição com baixa curtose
baixa_curtose <- tibble(valores = rnorm(1000, mean = 0, sd = 2),
                       curtose = "Baixa")
                       
# Juntar os dados de alta e baixa curtose com a função `bind_rows()` do pacote `dplyr`
dados_curtose <- bind_rows(alta_curtose, baixa_curtose)

# Criar gráfico que compara uma distribuição com alta curtose e uma com baixa curtose
dados_curtose |> 
ggplot() +
  geom_histogram(aes(x = valores, fill = curtose), alpha = 0.5) +
  labs(title = "Comparação de alta e baixa curtose", x = "Valores",
       y = "Frequência", fill = "Curtose") +
  theme_bw(base_size = 16)

#### Medidas de assimetria e curtose ####

# Caso não tenha o pacote `moments` instalado, descomente a linha abaixo e instale
# install.packages("moments")

# Carregar o pacote `moments`
library(moments)

# Calcular a assimetria da distribuição da massa corporal dos pinguins
pinguins |>
  pull(massa_corporal) |>
  skewness(na.rm = TRUE)

# Agora, calcular a curtose da distribuição da massa corporal dos pinguins
pinguins |>
  pull(massa_corporal) |>
  kurtosis(na.rm = TRUE)

# Histograma da distribuição da massa corporal dos pinguins
pinguins |>
  filter(!is.na(massa_corporal)) |>
  ggplot(aes(x = massa_corporal)) +
  geom_histogram(fill = "#ad002aff") +
  labs(x = "Massa corporal (em gramas)", y = "Frequência") +
  theme_classic(base_size = 16)

#### Transformação de dados ####

# Caso queira instalar o pacote `patchwork`, descomente a linha abaixo
# install.packages("patchwork")

# Carregar pacote `patchwork`
library(patchwork)

# Atribuir uma semente
set.seed(123)

# Criar amostra simulada não-normal
amostra <- rexp(10000)

# Criar gráfico da distribuição original
original <- ggplot(data.frame(x = amostra), aes(x)) +
  geom_histogram(bins = 20, color = "black", fill = "lightblue") +
  labs(title = "Distribuição Original", y = "", x = "") +
  theme_minimal(base_size = 14)

# Transformar a amostra através do log10()
amostra_log <- log10(amostra)

# Criar gráfico da distribuição transformada através de log10()
logaritmo <- ggplot(data.frame(x = amostra_log), aes(x)) +
  geom_histogram(bins = 20, color = "black", fill = "lightgreen") +
  labs(title = "Transformação logarítmica", x = "", y = "") +
  theme_minimal(base_size = 14)

# Transformar amostra através da raíz quadrada
amostra_sqrt <- sqrt(amostra)

# Criar gráfico da distribuição transformada através de raíz quadrada
raiz_quadrada <- ggplot(data.frame(x = amostra_sqrt), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 20, color = "black", fill = "darksalmon") +
  labs(title = "Transformação raíz quadrada", x = "", y = "") +
  theme_minimal(base_size = 14)

# Transformar a partir de raíz cúbica
amostra_raiz_cubica <- amostra^(1/3)

# Gráfico da distribuição transformada através de raíz cúbica
raiz_cubica <- ggplot(data.frame(x = amostra_raiz_cubica), aes(x)) +
  geom_histogram(aes(y = ..density..), bins = 20, color = "black", fill = "aquamarine") +
  labs(title = "Transformação raíz cúbica", x = "", y = "") +
  theme_minimal(base_size = 14)

# Juntar os gráficos em um único através do pacote `patchwork`
((original + logaritmo) / (raiz_quadrada + raiz_cubica)) +
  plot_annotation(title = "Dados simulados de 10.000 observações",
                  theme = theme(text = element_text(size = 16)))

#### Transformando a massa corporal dos pinguins ####

# Plotar a massa corporal dos pinguins em um histograma
pinguins |>
  ggplot(aes(x = massa_corporal)) +
  geom_histogram(fill = "coral2") +
  labs(x = "Massa corporal (em gramas)",
       y = "Frequência") +
  theme_bw(base_size = 16)

# Plotar a massa dos pinguins após transformar através de log10()
pinguins |>
  mutate(massa_log = log10(massa_corporal)) |>
  ggplot(aes(x = massa_log)) +
  geom_histogram(fill = "deepskyblue4") +
  labs(x = "Log da massa corporal",
       y = "Frequência") +
  theme_bw(base_size = 16)

#### Tabelas de frequência ####

# Caso queira instalar o pacote `janitor`, descomente a linha abaixo
# install.packages("janitor")

# Carregar pacote janitor
library(janitor)

# Solicitar tabela de frequência das espécies dos pinguins
pinguins |> 
  tabyl(especie)

# Solicitar tabulação cruzada das espécies e ilhas dos pinguins
pinguins |> 
  tabyl(especie, ilha)


# Adicionar totais e porcentagens na tabela
pinguins |>
  tabyl(especie, ilha) |>
  adorn_totals(where = "row") |>
  adorn_percentages(denominator = "col") |>
  adorn_pct_formatting(digits = 1)   

# Deixar mais completa e agradável visualmente
pinguins |> 
  tabyl(especie, ilha) |> 
  adorn_totals(where = "row") |> 
  adorn_percentages(denominator = "col") |> 
  adorn_pct_formatting() |> 
  adorn_ns(position = "front") |> 
  adorn_title(
    row_name = "Espécie",
    col_name = "Ilha")

# Caso queira instalar o pacote `flextable` para exportar tabelas para
# formato Word, Excel, PowerPoint, etc, descomente a linha abaixo
# install.packages("flextable")

# O código abaixo vai criar um arquivo em docx no seu diretório de trabalho
# atual com o arquivo da tabela em Word
pinguins |>
  tabyl(especie, ilha) |>
  adorn_totals(where = "row") |>
  adorn_percentages(denominator = "col") |>
  adorn_pct_formatting() |>
  adorn_ns(position = "front") |>
  adorn_title(row_name = "Espécie",
              col_name = "Ilha",
              placement = "combined") |>
  flextable::flextable() |>
  flextable::save_as_docx(path = "tabela_especie_ilha.docx")
