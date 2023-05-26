# dados_antropometricos: Dados antropométricos puxados da internet
# Autor: Bruno Braga Montezano
# Data: 26/05/2023

#### Armazenar URL dos dados ####

url <- "http://socr.ucla.edu/docs/resources/SOCR_Data/SOCR_Data_Dinov_020108_HeightsWeights.html"

#### Fazer a requisição da página e extrair o conteúdo ####

pagina <- httr::GET(url) |> 
  # Puxar conteúdo com encoding UTF-8
  httr::content(encoding = "UTF-8") |> 
  # Extrair tabela em html de dentro da página
  rvest::html_table(header = TRUE)

#### Limpeza e transformação de dados ####

dados_antropometricos <- pagina |> 
  # Remover a tibble de dentro da lista
  tibble::deframe() |> 
  # Limpar nome de variáveis para snake_case
  janitor::clean_names() |> 
  # Renomear as variáveis da tibble
  dplyr::rename(
    id = index,
    altura = height_inches,
    peso = weight_pounds
  ) |> 
  dplyr::mutate(
    # Transformar libras para quilogramas
    peso = peso * 0.45,
    # Transformar polegadas para centímetros
    altura = altura * 2.54
  )

#### Exportar dados para formato xlsx (arquivo do Excel) ####

dados_antropometricos |> 
  writexl::write_xlsx(path = "slides/dados/dados_antropometricos.xlsx")

