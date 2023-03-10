box::use(
  shiny,
  reactable[reactableOutput, renderReactable],
  dplyr[slice, pull, filter, add_row, select, mutate],
  shinyjs[show, hide, hidden, reset, disable, enable],
  shinyWidgets,
  shinyalert[shinyalert],
)

box::use(

  app/logic/dados,
  app/logic/aux_geral,
  app/logic/vis_tabelas,
  app/view/shiny_aux,

)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(

    shiny$column(
      3,
      shiny$tags$fieldset(
        shiny$tags$legend(
          shiny$tags$span(
            'Cadastro de receita',
            style = 'float:left'
          )),
        shiny$textInput(ns('nome'), "Nome da receita"),
        shiny$checkboxInput(ns('mensal'), "Renda mensal", value = TRUE),
        shinyWidgets$currencyInput(ns('valor'), "Valor", 0, format = "Brazilian"),
        shiny$dateInput(ns('data_ini'), "Dada inicial"),
        shiny$dateInput(ns('data_fim'), "Dada final"),
        shiny$checkboxInput(ns('receita_atual'), "Receita atual", value = TRUE),
        shiny$actionButton(ns('adicionar'), "Adicionar", icon = shiny$icon("circle-plus")),
        hidden(shiny$actionButton(ns('arquivar'), "Arquivar", icon = shiny$icon("box"))),
        hidden(shiny$actionButton(ns('deletar'), "Deletar", icon = shiny$icon("trash")))
      ),
      shiny$actionButton(ns('desfazer'), "Desfazer alterações", icon = shiny$icon("trash-arrow-up")),
      shiny$actionButton(ns('salvar'), "Salvar", icon = shiny$icon("cloud-arrow-up"))

    ),

    shiny$column(
      9,
      reactableOutput(ns('tbl'))
    )


  )

}

#' @export
server <- function(id, reac_geral, db_pool = NULL){

  shiny$moduleServer(id, function(input, output, session) {

    ns <- shiny$NS(id)
    reac <- shiny$reactiveValues(receita_sel = NULL)

    aux_geral$ativar_delete_key(ns('deletar'))

    shiny$observe({
      if (!is.null(db_pool)) {
        reac_geral$receita <- dados$get_receita(db_pool)
      }
    })


    shiny$observeEvent(input$adicionar, {

      id_temp <- reac$receita_sel$id
      novo_id <- FALSE

      if (is.null(id_temp)) {

        novo_id <- TRUE

        if (nrow(reac_geral$receita) > 0) {
          id_temp <-   reac_geral$receita$id |> max() + 1
        } else {
          id_temp <- 1
        }
      }

      usuario <- reac_geral$usuario |> filter(id == reac_geral$usuario_sel) |> pull(nome)
      data_fim <- input$data_fim
      if (input$receita_atual) {
        data_fim <- NA
      }



      nova_linha = data.frame(
        id = id_temp,
        nome = input$nome,
        usuario_id = reac_geral$usuario_sel,
        mensal = input$mensal,
        valor = input$valor,
        data_ini = input$data_ini,
        data_fim = data_fim,
        usuario = usuario,
        add = TRUE,
        new = novo_id
      )

      reac_geral$receita <- shiny$isolate(
        reac_geral$receita |>
          filter(id != nova_linha$id) |>
          add_row(nova_linha)
      )

      reset('nome')
      reset('descricao')
    })

    shiny$observeEvent(input$salvar, {

      shiny_aux$modal_loader("Aguarde", "Salvando categorias")

      df_aux <-
        reac_geral$receita |>
        filter(add) |>
        mutate(id = ifelse(new, NA, id)) |>
        select(-usuario, -add, -new)

      resp <- dados$upsert_df(db_pool, 'receita', df_aux)

      if (resp$status) {

        reac_geral$receita <- dados$get_receita(db_pool)
        shiny_aux$close_modal_loader()
        shinyalert("Receita salva", type = 'success')

      } else {

        shiny_aux$close_modal_loader()
        shinyalert("Erro", text = resp$error, type = 'error')

      }

    })

    shiny$observeEvent(input$receita_atual, {

      if (input$receita_atual){
        disable(id = 'data_fim')
      } else {
        enable(id = 'data_fim')
      }

    })

    shiny$observeEvent(input$tbl__reactable__selected, ignoreNULL = FALSE, {

      if (!is.null(input$tbl__reactable__selected)) {

        reac$receita_sel <-
          reac_geral$receita |>
          slice(input$tbl__reactable__selected)

        shiny$updateTextInput(inputId = 'nome', value = reac$receita_sel$nome)
        shiny$updateCheckboxInput(inputId = 'mensal', value = reac$receita_sel$mensal)
        shinyWidgets$updateCurrencyInput(inputId = 'valor', value = reac$receita_sel$valor)
        shiny$updateDateInput(inputId = 'data_ini', value = reac$receita_sel$data_ini)
        shiny$updateDateInput(inputId = 'data_fim', value = reac$receita_sel$data_fim)
        shiny$updateActionButton(inputId = 'adicionar', label = "Atualizar", icon = shiny$icon("pen-to-square"))
        show('arquivar')
        show('deletar')

      } else {

        reac$receita_sel <- NULL
        hide('arquivar')
        hide('deletar')
        reset('nome')
        reset('data_ini')
        reset('data_fim')
        shiny$updateActionButton(inputId = 'adicionar', label = "Adicionar", icon = shiny$icon("circle-plus"))

      }


    })

    shiny$observeEvent(input$deletar, {

      df_aux <-
        reac_geral$receita |>
        filter(id == reac$receita_sel$id)

      if (df_aux$new | df_aux$add) {

        reac_geral$receita <- reac_geral$receita |> filter(id != reac$receita_sel$id)
        reset('nome')

      } else {

        dados$delete_from_tabela(db_pool, 'receita', id = reac$receita_sel$id)
        reac_geral$receita <- reac_geral$receita |> filter(id != reac$receita_sel$id)
        reset('nome')

      }

    })

    shiny$observeEvent(input$desfazer, {

      reac_geral$receita <-
        reac_geral$receita |>
        filter(!new | !add)
    })

    output$tbl <- renderReactable({

      vis_tabelas$tbl_receita(db_pool, df = reac_geral$receita)

    })

  })
}
