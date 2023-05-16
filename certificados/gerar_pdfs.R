# Código: Gerar PDFs dos certificados para todos alunos do arquivo csv
# Autor: Bruno Braga Montezano

# Para rodar localmente, os diretórios "Rmd" e "PDF" devem ser criados.
# O diretório "Rmd" é o diretório onde ficam os arquivos .Rmd temporários.
# O diretório "PDF" é onde os certificados compilados serão gerados.

# Carregar função para gerar os certificados
source("certificados/gerar_certificado.R")

# Carregar arquivo no formato .csv com nome dos alunos
alunos <- readr::read_csv("certificados/alunos.csv") |>
  dplyr::mutate(
  # Criar variável do caminho do arquivo em PDF para o certificado
    arquivo_pdf = stringr::str_c(
      "../PDF/",
      dplyr::row_number(),
      "_",
      stringr::str_replace_all(nome, stringr::fixed(" "), "_"),
      ".pdf"
    ),
  # Criar variável do caminho dos PDFs possivelmente já existentes
  # para colocar como entrada na função file.exists() para fluxo do if
  # na função gerar_certificado()
    caminho_pdf = stringr::str_c(
      "certificados/PDF/",
      dplyr::row_number(),
      "_",
      stringr::str_replace_all(nome, stringr::fixed(" "), "_"),
      ".pdf"
    )
  )

# Loop para gerar certificado com nome de todos os alunos
# Para cada aluno, rodar a função gerar_certificado()
for (i in seq_len(nrow(alunos))) {
  with(
    alunos,
    gerar_certificado(
      modelo = "certificados/_modelo_certificado.Rmd",
      nome_aluno = nome[i],
      saida_pdf = arquivo_pdf[i],
      caminho_pdf = caminho_pdf[i],
      diretorio_rmd = "certificados/Rmd"
    )
  )
}
