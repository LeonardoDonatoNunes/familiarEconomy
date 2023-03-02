box::use(
  shiny,
  reactable[reactableOutput, renderReactable],
  dplyr[slice, pull, filter],
  shinyjs[show, hide, hidden, reset],
  shinyWidgets,
)

box::use(

  app/logic/dados,
  app/logic/aux,
  app/logic/vis_tabelas,

)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(

    shiny$column(
      3,
      shiny$textInput(ns('nome'), "Nome da receita"),
      shinyWidgets$currencyInput(ns('valor'), "Valor", 0, format = "Brazilian"),
      shiny$checkboxInput(ns('mensal'), "Receita mensal", value = TRUE),
      shiny$selectInput(ns('ano_ref'), "Ano referência", choices = aux$vct_anos(), selected = aux$ano_ultimo_mes()),
      shiny$selectInput(ns('mes_ref'), "Mês referência", choices = aux$vct_meses(), selected = aux$ultimo_mes()),
      shiny$actionButton(ns('salvar'), "Salvar", icon = shiny$icon("cloud-arrow-up")),
      hidden(shiny$actionButton(ns('deletar'), "Deletar", icon = shiny$icon("trash")))
    ),

    shiny$column(
      9,
      shiny$tags$div(
        class = 'display_flex_row flex_start',
        shiny$checkboxInput(ns('filtrar_periodo'), "Filtrar período selecionado", value = TRUE, width = 'auto'),
        shiny$checkboxInput(ns('filtrar_usuario'), "Filtrar usuário selecionado", value = TRUE, width = 'auto')
      ),
      reactableOutput(ns('tbl'))
    )


  )

}

#' @export
server <- function(id, reac_geral, db_pool){

  shiny$moduleServer(id, function(input, output, session) {

    reac <- NULL
    reac <- shiny$reactiveValues(receita = dados$get_receita(db_pool), receita_sel = NULL)

    shiny$observeEvent(input$salvar, {
      dados$upsert_receita(db_pool, id = reac$receita_sel$id, nome = input$nome, usuario_id = reac_geral$usuario_sel, valor = input$valor, mensal = input$mensal, ano_ref = input$ano_ref, mes_ref = input$mes_ref)
      reac$receita <- shiny$isolate(dados$get_receita(db_pool))
      reset('nome')
    })

    shiny$observeEvent(input$tbl__reactable__selected, ignoreNULL = FALSE, {

      if (!is.null(input$tbl__reactable__selected)) {

        reac$receita_sel <-
          reac$receita |>
          slice(input$tbl__reactable__selected)

        shiny$updateTextInput(inputId = 'nome', value = reac$receita_sel$nome)
        shinyWidgets$updateCurrencyInput(inputId = 'valor', value = reac$receita_sel$valor)
        shiny$updateSelectInput(inputId = 'ano_sel', selected = reac$receita_sel$ano_sel)
        shiny$updateSelectInput(inputId = 'mes_sel', selected = reac$receita_sel$mes_sel)

        show('deletar')

      } else {

        reac$receita_sel <- NULL
        hide('deletar')
        reset('nome')

      }


    })

    shiny$observeEvent(input$deletar, {
      dados$delete_from_tabela(db_pool, 'receita', id = reac$receita_sel$id)
      reac$receita <- dados$get_receita(db_pool)
      reset('nome')
    })

    shiny$observe({

      if (all(!is.null(input$ano_ref), !is.null(input$mes_ref))) {

        reac$receita_vis <- reac$receita

        if (!is.null(input$filtrar_periodo)) {

          if (input$filtrar_periodo) {
            reac$receita_vis <- shiny$isolate(
              reac$receita_vis |>
                filter(
                  ano_ref == input$ano_ref,
                  mes_ref == input$mes_ref
                )
            )
          }

        }

        if (!is.null(input$filtrar_usuario)) {
          if (input$filtrar_usuario) {

            if (!is.null(reac_geral$usuario_sel)) {

              reac$receita_vis <- shiny$isolate(
                reac$receita_vis |>
                  filter(
                    usuario_id == reac_geral$usuario_sel
                  )
              )
            }
          }
        }
      }
    })


    output$tbl <- renderReactable({
      vis_tabelas$tbl_receita(db_pool, df = reac$receita_vis)
    })

  })
}
