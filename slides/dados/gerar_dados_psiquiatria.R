# dados_psiquiatria: Dados de saúde mental simulados
# Autor: Bruno Braga Montezano
# Data: 25/05/2023

#### Definir tamanho amostral ####
tamanho_amostral <- 850

#### Criar dados simulados ####

# Definindo uma seed aleatória para reprodução dos resultados
set.seed(123)

# Criar tibble para facilitar rotinas tidy
dados_psiquiatria <- tibble(
  # Criar variável sexo com 40% homens e 60% mulheres
  sexo = sample(
    x = c("M", "F"),
    size = tamanho_amostral,
    replace = TRUE,
    prob = c(0.4, 0.6)
  ),
  # Criar variável de idade de 18 a 70 anos
  idade = round(rnorm(
    n = tamanho_amostral, mean = 35, sd = 5
  )),
  # Criar variável com escore de sintomas depressivos a partir do sexo
  # Mulheres apresentam escores maiores de depressão
  depressao = ifelse(sexo == "F", round(rnorm(
    n = tamanho_amostral, mean = 20, sd = 5
  )), round(rnorm(
    n = tamanho_amostral, mean = 15, sd = 5
  ))),
  # Criar variável de sintomas de ansiedade com média 10 e desvio padrão de 3
  ansiedade = round(rnorm(
    n = tamanho_amostral, mean = 10, sd = 3
  )),
  # Criar variável da escolaridade de forma aleatória
  escolaridade = sample(
    x = c("Ensino fundamental", "Ensino médio", "Ensino superior"),
    size = tamanho_amostral,
    replace = TRUE
  ),
  # Criar variável da cor de pele
  # 80% brancos e 20% não-brancos
  cor = ifelse(runif(tamanho_amostral) < 0.8, "Branco", "Não-branco"),
  # Criar variável de satisfação com a vida
  satisfacao = ifelse(
    # Escores menores em sujeitos com ensino fundamental
    escolaridade == "Ensino fundamental",
    round(rnorm(tamanho_amostral, mean = 20, sd = 3)),
    # Escores intermediários em sujeitos com ensino médio
    ifelse(escolaridade == "Ensino médio", round(rnorm(
      n = tamanho_amostral, mean = 25, sd = 3
    )),
    # Escores maiores de satisfação em sujeitos com ensino superior
    round(rnorm(
      n = tamanho_amostral, mean = 30, sd = 3
    )))
  )
)

#### Definindo as correlações ####

# Correlação positiva entre depressão e ansiedade
cor_escore_depressao_ansiedade <- 0.65
# Correlação negativa entre depressão e satisfação com a vida
cor_escore_depressao_satisfacao <- -0.4
# Correlação negativa entre ansiedade e satisfação com a vida
cor_escore_ansiedade_satisfacao <- -0.35

#### Gerando os dados correlacionados ####

dados_psiquiatria <- dados_psiquiatria |> mutate(
  # Ansiedade e depressão
  ansiedade = round(cor_escore_depressao_ansiedade * depressao + sqrt(1 - cor_escore_depressao_ansiedade ^
                                                                  2) * ansiedade),
  # Satisfação com a vida e depressão
  satisfacao = cor_escore_depressao_satisfacao * depressao + sqrt(1 - cor_escore_depressao_satisfacao ^
                                                                    2) * satisfacao,
  # Satisfação com a vida e ansiedade
  satisfacao = abs(round(
    cor_escore_ansiedade_satisfacao * ansiedade + sqrt(1 - cor_escore_ansiedade_satisfacao ^
                                                         2) * satisfacao
  ))
) |> 
  dplyr::relocate(sexo, idade, cor, escolaridade)

#### Exportação dos dados simulados ####

# Exportar os dados em sav para treinar a importação com os alunos
haven::write_sav(
  data = dados_psiquiatria,
  path = "slides/dados/dados_psiquiatria.sav"
)