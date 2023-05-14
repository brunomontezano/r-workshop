# Código: Gerar PDFs dos certificados para todos alunos do arquivo csv
# Autor: Bruno Braga Montezano

# Carregar função para gerar os certificados
source("certificados/gerar_certificado.R")

# Carregar arquivo com informações dos alunos
alunos <- readr::read_csv("certificados/alunos.csv") |>
  # Criar variável do caminho do arquivo em PDF para o certificado
  dplyr::mutate(
    arquivo_pdf = stringr::str_c(
      "../PDF/",
      dplyr::row_number(),
      "_",
      stringr::str_replace_all(nome, stringr::fixed(" "), "_"),
      ".pdf"
    ),
    caminho_pdf = stringr::str_c(
      "certificados/PDF/",
      dplyr::row_number(),
      "_",
      stringr::str_replace_all(nome, stringr::fixed(" "), "_"),
      ".pdf"
    )
  )

# Loop para gerar certificado com nome de todos os alunos
for (i in seq_len(nrow(alunos))) {
  with(
    alunos,
    gerar_certificado(
      modelo = "certificados/modelo_certificado.Rmd",
      nome_aluno = nome[i],
      saida_pdf = arquivo_pdf[i],
      caminho_pdf = caminho_pdf[i],
      diretorio_rmd = "certificados/Rmd"
    )
  )
}
