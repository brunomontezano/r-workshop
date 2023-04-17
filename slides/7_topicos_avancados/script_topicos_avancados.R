
# Carregar pacote `rpart` para ajustar modelos de árvore de decisão
library(rpart)

# Carregar pacote `rpart.plot` para plotar (criar gráficos) das árvores de decisão
library(rpart.plot)

# Carregar pacote `titanic` para acessar os dados do Titanic
library(titanic)

# Recodificar algumas variáveis para a língua portuguesa
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

# Ajustar a árvore de decisão para predizer sobrevivência do sujeito baseado
# no sexo e na idade
titanic_ajuste <- rpart(Survived ~ Sexo + Idade,
                               data = titanic_treino)

# Mostrar gráfico da árvore de decisão
rpart.plot(titanic_ajuste,
           extra = 6,
           fallen.leaves = TRUE,
           yes.text = "Sim",
           no.text = "Não",
           main = "Sobrevivência no desastre do Titanic")