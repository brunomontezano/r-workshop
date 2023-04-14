# Carregar os pacotes `dados`, `dplyr` e `ggplot2`
library(dados)
library(dplyr)
library(ggplot2)

# Mostrar a base de dados dos `dados_starwars` do pacote `dados`
dados_starwars

#### Exemplos iniciais de `ggplot2` ####

# Gráfico de dispersão/pontos (scatterplot) da massa e altura dos personagens de Star Wars
ggplot(data = dados_starwars,
       aes(x = massa, y = altura)) +
  geom_point(color = "forestgreen", size = 3) +
  xlab("Massa (kg)") +
  ylab("Altura (cm)") +
  ggtitle("Altura e massa dos personagens") +
  theme_bw(base_size = 18)

# Mesmo gráfico acima mas com os pontos coloridos a partir do gênero do personagem
ggplot(data = dados_starwars |> 
         filter(!is.na(genero)),
       aes(x = massa, y = altura, color = genero)) +
  geom_point(size = 4, alpha = 0.7) +
  xlab("Massa (kg)") +
  ylab("Altura (cm)") +
  ggtitle("Altura e massa dos personagens",
          "Estratificado por gênero") +
  theme_minimal(base_size = 20) +
  scale_color_discrete(name = "Gênero")

#### Camadas de um gráfico do `ggplot2` ####

# Criar o canvas (tela) onde o gráfico será criado
dados_starwars |> 
  ggplot()

# Mapear os eixos x e y para massa e altura, respectivamente
# Note que os eixos já se adaptam de acordo com os dados
dados_starwars |> 
  ggplot() +
  aes(x = massa, y = altura)

# Por fim, adicionar os pontos para cada observação ser representada por um ponto
dados_starwars |> 
  ggplot() +
  aes(x = massa, y = altura) +
  geom_point()

#### Gráfico de pontos ####

dados_starwars |> # Usar os `dados_starwars`
  filter(massa < 1000) |> # Manter apenas observações com menos de 1.000kg de massa
  ggplot() + # Iniciar o canvas
  aes(x = massa, y = altura) + # Mapear massa no eixo x e altura no eixo y
  geom_point() # Usar a geometria de pontos para criar um gráfico de dispersão

#### Gráfico de linhas ####

# Mostrar a base `dados_gapminder` do pacote `dados`
dados_gapminder

# Criar gráfico de linha com o `geom_line()`
dados_gapminder |> 
  filter(pais == "Brasil") |> 
  ggplot() +
  aes(x = ano, y = pib_per_capita) +
  geom_line()

# Modificando as escalas do eixo x e y através das funções da família `scale`
dados_gapminder |> 
  filter(pais == "Brasil") |> 
  ggplot() +
  aes(x = ano, y = pib_per_capita) +
  geom_line() +
  scale_x_continuous(breaks = seq(1952, 2007, 5)) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ",
                                             decimal.mark = ",",
                                             big.mark = "."))


# Adicionar rótulos e modificar o tema do gráfico
# através das funções `labs()` e `theme_minimal()`
dados_gapminder |> 
  filter(pais == "Brasil") |> 
  ggplot() +
  aes(x = ano, y = pib_per_capita) +
  geom_line(linewidth = 2, color = "darkslateblue") +
  scale_x_continuous(breaks = seq(1952, 2007, 5)) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ",
                                             decimal.mark = ",",
                                             big.mark = ".")) +
  labs(x = "Ano", y = "PIB per capita",
       title = "PIB per capita do Brasil ao passar dos anos",
       subtitle = "De 1952 a 2007",
       caption = "Fonte: Gapminder") +
  theme_minimal(base_size = 16)

#### Gráfico de barras ####

# Mostrar dados `pixar_avalicao_publico` do pacote `dados`
pixar_avalicao_publico

# A função count() serve para contar os valores únicos de uma variável
pixar_avalicao_publico |> 
  count(nota_cinema_score)

# Combinar a função `count()` com a geometria `geom_col()` para criar um gráfico de barras
pixar_avalicao_publico |> 
  count(nota_cinema_score) |> 
  ggplot() +
  aes(x = nota_cinema_score, y = n) |> 
  geom_col()


# Código mais complexo para destacar a coluna da nota A no gráfico de barras
# Note que usamos `mutate()` para modificar e criar variáveis
# A função `scale_fill_manual()` foi usada para indicar as cores para cada nível
# da variável `cor`
pixar_avalicao_publico |>
  count(nota_cinema_score) |>
  mutate(
    nota_cinema_score = forcats::fct_na_value_to_level(nota_cinema_score, "Sem escore"),
    nota_cinema_score = factor(nota_cinema_score,
                               levels = c("A-", "A",
                                          "A+",
                                          "Sem escore")),
    cor = dplyr::case_when(nota_cinema_score == "A" ~ "Colorido",
                           .default = "Cinza")
  ) |>
  ggplot() +
  aes(x = nota_cinema_score, y = n, fill = cor) |>
  geom_col(width = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("gray25", "darkorange")) +
  labs(title = "A maioria dos filmes (n = 13) receberam nota A.",
       x = "Nota segundo CinemaScore",
       y = "Número de filmes") +
  theme_classic(base_size = 18)

#### Histograma ####

# Criando histograma básico com a nota da plataforma Rotten Tomatoes
# Note que para o histograma, precisamos mapear apenas o eixo x
pixar_avalicao_publico |> 
  ggplot() +
  aes(x = nota_rotten_tomatoes) +
  geom_histogram()

# Personalizar cores, rótulos (labels) e tema do gráfico
pixar_avalicao_publico |> 
  ggplot() +
  aes(x = nota_rotten_tomatoes) +
  geom_histogram(fill = "cyan4") +
  labs(x = "Nota segundo críticos do Rotten Tomatoes",
       y = "Contagem",
       title = "Como se distribuem as notas segundo os críticos do Rotten Tomatoes?",
       subtitle = "Dados de 24 filmes da Pixar",
       caption = "Fonte: Wikipedia") +
  theme_light(base_size = 16)

#### Box plot ####

# Box plot da expectativa de vida para cada continente
# Note que filtramos a base apenas com as observações referentes ao ano 1952
# A filtragem foi realizada com a função `filter()` do pacote `dplyr`
dados_gapminder |> 
  filter(ano == 1952) |> 
  ggplot() +
  aes(x = continente, y = expectativa_de_vida) +
  geom_boxplot()


# Personalizar cores, tema, rótulos e anotar um texto para acompanhar o gráfico
dados_gapminder |> 
  filter(ano == 1952) |> 
  ggplot() +
  aes(x = continente, y = expectativa_de_vida, color = continente) +
  geom_boxplot(show.legend = FALSE) +
  theme_light(base_size = 16) +
  labs(x = "Continente", y = "Expectativa de vida (em anos)",
       title = "Como se distribuía a expectativa de vida em cada continente em 1952?",
       caption = "Fonte: Gapminder") +
  annotate("text", x = 1, y = 65,
           label = "Em 1952,\na África tinha\numa expectativa\nde vida\nde 38,8 anos.",
           size = 4)

#### Brincadeira do calendário das aulas do mês de abril ####

# Caso queira rodar esse código do calendário, instale o pacote `calendR`
# Descomente a linha abaixo para rodar o comando de instalação
# install.packages("calendR")

# Calendário com as aulas do curso de Introdução ao R em abril de 2023
calendR::calendR(year = 2023, month = 04,
        special.days = c(17, 24),
        special.col = "#bfe2f2",
        low.col = "white",
        title = "Aulas de R no mês de abril de 2023",
        subtitle = "Exclusivas para o grupo de pesquisa Alliance",
        weeknames = c("Segunda", "Terça",
                      "Quarta", "Quinta", "Sexta", "Sábado", "Domingo"),
        lunar = FALSE)
