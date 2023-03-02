box::use(
  shiny[tags, tagList],
  shinyalert[shinyalert, closeAlert],
  glue[glue],
  shinyjs[runjs, refresh],
)

#' @export
modal_loader <- function(mensagem, sub_mensagem = '') {

  tagList(
    shinyalert::shinyalert(
      html = TRUE,
      title = '<img src="static/loader_laranja.svg" height="80" alpha=0.2>',
      text = glue('<div class = "display_flex"><span style = "font-size: 25px;font-weight: bold;">{mensagem}</span><span class = "loader"></span></div>
                        <h4 class = "mensagem_loader">{sub_mensagem}</h4>'),
      showCancelButton = FALSE,
      showConfirmButton = FALSE,
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE,
      size = 's')
  )

}

#' @export
modal_error_w_reload <- function(mensagem, sub_mensagem = '') {

    tagList(
      shinyalert::shinyalert(
        html = TRUE,
        title = '<img src="static/error_pig.svg" height="80" alpha=0.2>',
        text = glue('<div class = "display_flex"><span style = "font-size: 25px;font-weight: bold;">{mensagem}</span><span class = "loader"></span></div>
                        <h4 class = "mensagem_loader">{sub_mensagem}</h4>'),
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        confirmButtonText = "Atualizar",
        callbackR = function(value) {if (value) {refresh()}}
        )
    )

  }


#' @export
alterar_mensagem_modal_loader <- function(sub_mensagem) {

  runjs(glue(.open = "<<", .close = ">>", "
              $('h4.mensagem_loader')[0].innerText = '<<sub_mensagem>>';
             "))

}


#' @export
close_modal_loader <- function() {
  closeAlert()
}
