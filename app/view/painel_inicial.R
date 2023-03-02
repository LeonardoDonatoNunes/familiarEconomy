box::use(
  shiny,
  echarts4r[echarts4rOutput, renderEcharts4r],
)

box::use(
  app/logic/vis_graficos
)

#' @export
ui <- function(id, db_pool) {
  ns <- shiny$NS(id)

  shiny$tagList(

    echarts4rOutput(ns('plt_receita_despesa'))

  )

}

#' @export
server <- function(id, db_pool) {

  shiny$moduleServer(id, function(input, output, session) {

    output$plt_receita_despesa <- renderEcharts4r({
      vis_graficos$receita_despesa(db_pool)
    })

  })

}
