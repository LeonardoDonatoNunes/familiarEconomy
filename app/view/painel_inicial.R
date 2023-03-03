box::use(
  shiny,
  shinyWidgets[radioGroupButtons, pickerInput],
  echarts4r[echarts4rOutput, renderEcharts4r],
  reactable[reactableOutput, renderReactable],
)

box::use(
  app/logic/dados,
  app/logic/vis_graficos,
  app/logic/vis_tabelas,
  app/logic/aux_geral,
)

#' @export
ui <- function(id, db_pool) {
  ns <- shiny$NS(id)

  shiny$tagList(

    shiny$tags$div(class = 'display_flex_row',
                   pickerInput(
                     inputId = ns('ano_sel'),
                     label = NULL,
                     width = '80px',
                     choices = aux_geral$vct_anos(),
                     selected = aux_geral$ano_ultimo_mes(),
                     options = list(
                       style = "btn-warning")
                   ),
                   radioGroupButtons(
                     inputId = ns('mes_sel'),
                     label = NULL,
                     choices = aux_geral$vct_meses(),
                     selected = aux_geral$ultimo_mes(),
                     status = 'warning'
                   )
    ),

    shiny$column(8, reactableOutput(ns('tbl'))),
    shiny$column(4, echarts4rOutput(ns('plt_receita_despesa')))

  )

}

#' @export
server <- function(id, db_pool) {

  shiny$moduleServer(id, function(input, output, session) {

    reac <- shiny$reactiveValues()

    shiny$observeEvent(c(input$ano_sel, input$mes_sel), {
      reac$pagamento <- dados$get_pagamento(db_pool = db_pool, ano_ref = input$ano_sel, mes_ref = input$mes_sel)
    })

    output$plt_receita_despesa <- renderEcharts4r({
      if (!is.null(reac$pagamento)) {
        vis_graficos$receita_despesa(db_pool, reac$pagamento)
      }
    })

    output$tbl <- renderReactable({
      if (!is.null(reac$pagamento)) {
        vis_tabelas$tbl_pagamentos(db_pool, reac$pagamento)
      }
    })

  })

}
