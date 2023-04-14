#### Carregamento de pacotes ####

# Carregar o pacote `dados` para usar os dados dos pinguins
library(dados)

# Carregar o pacote `janitor` para tabulações
library(janitor)

# Carregar o pacote `ggplot2` para visualização de dados
library(ggplot2)

# Carregar pacote `dplyr` para manipulação de dados
library(dplyr)

#### Teste qui-quadrado ####

# Criar uma tabulação cruzada entre espécie e ilha
pinguins |> 
  tabyl(especie, ilha)

# Rodar um teste qui-quadrado de independência
chisq.test(pinguins$especie, pinguins$ilha)

# Criar gráfico de barras com a proporção de pinguins morando em cada ilha
# separado por espécie
ggplot(pinguins,
       aes(x = especie, fill = ilha)) +
  geom_bar(position = "fill") +
  labs(x = "Espécie",
       y = "Proporção",
       fill = "Ilha") +
  ghibli::scale_fill_ghibli_d("PonyoMedium") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 21,
                base_family = "Charter")

# Calcular o V de Cramer, medida de efeito para teste qui-quadrado

# Se você não possui o pacote `effectsize`, pode instalar descomentando a linha abaixo
# install.packages("effectsize")

effectsize::cramers_v(pinguins$especie,
                      pinguins$ilha,
                      alternative = "two.sided",
                      ci = 0.95)

#### Teste t de Student ####

# Criar histograma que mostra distribuição da massa corporal para pinguins
# fêmea e pinguins macho
pinguins |>
  filter(!is.na(sexo)) |>
  group_by(sexo) |>
  mutate(
    sexo = if_else(sexo == "macho", "Macho", "Fêmea"),
    media_massa = mean(massa_corporal)
  ) |>
  ggplot(aes(x = massa_corporal,
             fill = sexo)) +
  geom_histogram(alpha = 0.65) +
  labs(x = "Massa corporal",
       y = "# de pinguins",
       fill = "Sexo") +
  theme_minimal(base_size = 16) +
  geom_vline(aes(xintercept = media_massa, color = sexo),
             size = 2,
             show.legend = FALSE)

# Sumarizar a média e desvio padrão da massa corporal agrupado por sexo
pinguins |>
  filter(!is.na(sexo)) |>
  group_by(sexo) |>
  summarise(
    media_massa = mean(massa_corporal, na.rm = TRUE),
    dp_massa = sd(massa_corporal, na.rm = TRUE)
  )

# Rodar um teste t de Student para verificar a diferença nas médias da massa
# corporal de machos e fêmeas
t.test(formula = massa_corporal ~ sexo,
       data = pinguins,
       paired = FALSE,
       conf.level = 0.95)

# Calcular o tamanho de efeito através do d de Cohen com a função `cohens_d()`
# do pacote `effectsize`
effectsize::cohens_d(massa_corporal ~ sexo,
                     data = pinguins,
                     pooled_sd = FALSE,
                     paired = FALSE,
                     ci = 0.95)

#### ANOVA de uma via ####

# Box plot que demonstra a massa corporal dos pinguins em cada ilha
pinguins |>
  ggplot(aes(x = ilha,
             y = massa_corporal,
             fill = ilha)) +
  geom_boxplot() +
  labs(y = "Massa corporal",
       x = "Ilha") +
  scale_y_continuous(labels = \(x) paste0(as.character(x), "g")) +
  theme_minimal(base_size = 18) +
  theme(legend.position = "none")

# Ajustar uma ANOVA de uma via para verificar se há diferença entre massa
# corporal dos pinguins de acordo com a ilha
anova_massa_ilha <- aov(massa_corporal ~ ilha,
                        data = pinguins)

# Mostrar resumo da ANOVA ajustada acima
summary(anova_massa_ilha)

# Rodar teste de Tukey como post-hoc para verificar diferença entre cada ilha
TukeyHSD(anova_massa_ilha)

#### Correlação ####

# Criar gráfico de dispersão/pontos (scatterplot) da massa corporal e do
# comprimento do bico
pinguins |>
  ggplot(aes(x = massa_corporal,
             y = comprimento_bico)) +
  geom_point(color = "#00468b99", alpha = 0.8) +
  labs(x = "Massa corporal (em gramas)",
       y = "Comprimento do bico (em mm)") +
  theme_minimal(base_size = 16)

# Rodar um teste de correlação de Pearson
cor.test(pinguins$massa_corporal,
         pinguins$comprimento_bico,
         method = "pearson",
         conf.level = 0.95)

# Comentado abaixo, segue o código modificado para rodar um teste de correlação
# de Spearman
#cor.test(pinguins$massa_corporal,
#         pinguins$comprimento_bico,
#         method = "spearman",
#         conf.level = 0.95)