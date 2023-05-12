#' Gerar certificado para um aluno do curso
#'
#' @param modelo Modelo de certificado de participação.
#' @param nome_aluno Nome do aluno a ser inserido no certificado.
#' @param nome_evento Nome do evento que o aluno participou.
#' @param data_evento Data de finalização do evento (curso).
#' @param local_evento Local (cidade e país) onde aconteceu o evento.
#' @param saida_pdf Caminho de saída do PDF do certificado.
#' @param diretorio_qmd Diretório para os arquivos .qmd temporários.
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
           diretorio_qmd) {
    cat("\n Iniciando:", saida_pdf, "\n")
    
    # Criar um arquivo qmd temporário com aluno e informações do evento
    modelo_certificado <- readr::read_file(modelo)
    
    qmd_temporario <- modelo_certificado |> 
      stringr::str_replace("NOMEALUNO", nome_aluno) |> 
      stringr::str_replace("NOMEEVENTO", nome_evento) |> 
      stringr::str_replace("DATAEVENTO", data_evento) |> 
      stringr::str_replace("LOCALEVENTO", local_evento)
    
    # O diretorio_knit tem que ser definido para o
    # quarto::quarto_render() funcionar
    arquivo_qmd <- tempfile(tmpdir = diretorio_qmd, fileext = ".qmd")
    readr::write_file(qmd_temporario, arquivo_qmd)
    
    # Criar os certificados usando R Markdown/Quarto
    rmarkdown::render(arquivo_qmd,
                      output_file = saida_pdf,
                      quiet = TRUE)
    
    # Arquivo .qmd temporário pode ser excluído
    file.remove(arquivo_qmd)
    cat("\n Finalizado:", saida_pdf, "\n")
  }