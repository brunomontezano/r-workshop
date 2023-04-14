#### Tidyverse ####

# Caso você não tenha o pacote `tidyverse`,
# ele pode ser instalado com o comando abaixo
# Retire o `#` para rodar o comando, caso necessário

# install.packages("tidyverse")

#### Diretório de trabalho ####

# Função para encontrar o diretório de trabalho atual
getwd()

#### Pipes ####

# Criar vetor `x`
x <- c(2, 7, 40, 11, 21)

# Somar valores do vetor `x` e tirar a raíz quadrada do resultado da soma
sqrt(sum(x))

# Mesmo cálculo, agora com pipe!
x |>
  sum() |>
  sqrt()

#### Atribuindo valores ao usar pipe ####

raiz_quadrada_da_soma <- x |>
  sum() |>
  sqrt()

raiz_quadrada_da_soma

#### Carregando pacote `dados`####

# install.packages("dados")
library(dados)

questionario

#### Valores ausentes ####

# Criar vetor com valores ausentes
vetor_com_missing <- c(25, NA, 13, 44, 12, NA)

# Solicitar média aritmética do vetor com valores ausentes
mean(vetor_com_missing)

# Agora, solicitando a mesma média, mas agora usando o argumento `na.rm = TRUE`
mean(vetor_com_missing, na.rm = TRUE)

# Esta sintaxe abaixo realiza a mesma tarefa do código acima com
# o uso do argumento `na.rm = TRUE`
mean(vetor_com_missing[!is.na(vetor_com_missing)])

#### Fatores ####

# Os fatores são uma classe de dados para lidar com variáveis categóricas.

# Criar um vetor de fator
sexo <- factor(c("Masculino", "Feminino", "Feminino", "Masculino", "Masculino"))

# Mostrar o fator
# Note que abaixo da saída no Console, existe uma linha indicando os níveis (Levels)
# da variável, que seriam os valores possíveis desse fator
sexo

# Observe na base de dados `questionario` que existem fatores presentes
questionario

#### Filtrando linhas com `filter()` ####

# Carregando pacote `dplyr`para auxiliar nas rotinas de manipulação de dados
library(dplyr)

# Criar subconjunto apenas com os sujeitos divorciados
divorciados <- questionario |> 
  filter(estado_civil == "Divorciado(a)")

# Mostrar subconjunto criado acima
divorciados

# Remover observações (linhas) que apresentam valor ausente na variável `horas_tv`
divorciados |> 
  filter(!is.na(horas_tv))

#### Ordenando linhas com `arrange()` ####

# Ordenar a base `questionario` baseado no ano (de forma ascendente) e na idade
# dos sujeitos (de forma descendente)
questionario |> 
  arrange(ano, desc(idade))

#### Selecionando colunas com `select()` ####

# Selecionar apenas as variáveis `ano`, `idade` e `horas_tv`
divorciados |> 
  select(ano, idade, horas_tv)

#### Removendo colunas com `select()` ####

# Removendo variáveis `estado_civil`, `renda` e `denominacao` da base `divorciados`
divorciados |> 
  select(-estado_civil, -renda, -denominacao)

#### Funções auxiliares para `select()` ####

# Selecionar colunas que o nome termina com "ao"
questionario |>
  select(ends_with("ao"))

# Selecionar variáveis da variável `ano` até a variável `raca`
questionario |> 
  select(ano:raca)

#### Criando e modificando colunas com `mutate()` ####

# Filtrar base `divorciados` apenas com sujeitos de raça "Branca",
# então selecionar apenas as colunas `ano`, `idade` e `horas_tv`,
# então criar uma variável da idade dos sujeitos em décadas e
# criar variável das horas assistidas de TV por dia em minutos
divorciados |> 
  filter(raca == "Branca") |> 
  select(ano, idade, horas_tv) |> 
  mutate(idade_em_decadas = idade / 10,
         horas_tv_em_minutos = horas_tv * 60)

# Filtrar a base `divorciados` com os sujeitos protestantes,
# então selecionar as colunas `ano`, `idade` e `horas_tv`,
# então modificar a variável `horas_tv` para `horas_tv` multiplicada por 60
divorciados |> 
  filter(religiao == "Protestante") |> 
  select(ano, idade, horas_tv) |> 
  mutate(horas_tv = horas_tv * 60)

#### Agregando dados com `summarise()` ####

# Para os divorciados do ano de 2000,
# calcular o número de observações,
# a média de idade,
# a mediana das horas assistidas de TV,
# e amplitude de horas de TV assistidas por dia
divorciados |> 
  filter(ano == 2000) |> 
  summarise(n_observacoes = n(),
            media_idade = mean(idade, na.rm = TRUE),
            mediana_horas_tv = median(horas_tv, na.rm = TRUE),
            amplitude_horas_tv = max(horas_tv, na.rm = TRUE) - min(horas_tv, na.rm = TRUE))

# Podemos usar a função `across()` para sumarizar determinadas medidas em mais
# de uma coluna (variável) ao mesmo tempo
divorciados |> 
  filter(ano == 2014) |> 
  summarise(across(c(idade, horas_tv),
                   list(media = \(x) mean(x, na.rm = TRUE),
                        desvio_padrao = \(x) sd(x, na.rm = TRUE))))

#### Agrupando dados com `group_by()` ####

# Número de religiões reportadas pelos entrevistados, o tamanho da amostra,
# e a média de horas diárias assistidas de TV em cada ano (agrupado por ano)
questionario |> 
  group_by(ano) |> 
  summarise(numero_de_religioes = n_distinct(religiao),
            n_observacoes = n(),
            media_horas_tv = mean(horas_tv, na.rm = TRUE))

