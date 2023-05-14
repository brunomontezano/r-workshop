#' Gerar certificado para um aluno do curso
#'
#' @param modelo Modelo de certificado de participação.
#' @param nome_aluno Nome do aluno a ser inserido no certificado.
#' @param nome_evento Nome do evento que o aluno participou.
#' @param data_evento Data de finalização do evento (curso).
#' @param local_evento Local (cidade e país) onde aconteceu o evento.
#' @param saida_pdf Caminho de saída do PDF do certificado.
#' @param diretorio_rmd Diretório para os arquivos .Rmd temporários.
#'
#' @return
#' @export
gerar_certificado <-
  function(modelo,
           nome_aluno,
           nome_evento,
           data_evento,
           local_evento,
           saida_pdf,
           diretorio_rmd) {
    cat("\n Iniciando a compilação:", saida_pdf, "\n")
    
    # Criar um arquivo qmd temporário com aluno e informações do evento
    modelo_certificado <- readr::read_file(modelo)
    
    # Modificar NOMEALUNO pelo nome do aluno para o modelo
    # temporário de cada iteração
    modelo_tmp <- modelo_certificado |> 
      stringr::str_replace("NOMEALUNO", nome_aluno)
    
    # O diretorio_rmd tem que ser definido para o
    # rmarkdown::render() saber onde salvar o arquivo temporário
    arquivo_rmd <- tempfile(tmpdir = diretorio_rmd, fileext = ".Rmd")
    readr::write_file(modelo_tmp, arquivo_rmd)
    
    # Criar os certificados usando R Markdown
    rmarkdown::render(arquivo_rmd,
                      output_file = saida_pdf,
                      quiet = TRUE)
    
    # Arquivo .Rmd temporário pode ser excluído após cada iteração
    file.remove(arquivo_rmd)
    cat("\n Finalizado:", saida_pdf, "\n")
  }
