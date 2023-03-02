box::use(
  shiny,
  reactable[reactableOutput, renderReactable],
  dplyr[slice, pull, add_row, mutate, bind_rows, filter, select],
  shinyjs[show, hide, hidden, reset],
  shinyalert[shinyalert]

)

box::use(

  app/logic/dados,
  app/logic/vis_tabelas,
  app/view/shiny_aux,

)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(

    shiny$column(
      4,
      shiny$tags$fieldset(
        shiny$tags$legend(
          shiny$tags$span(
            'Cadastro de categoria (Geral)',
            style = 'float:left'
          )),
        shiny$textInput(ns('nome'), "Nome do usuário"),
        shiny$actionButton(ns('adicionar'), "Adicionar", icon = shiny$icon("circle-plus")),
        hidden(shiny$actionButton(ns('deletar'), "Deletar", icon = shiny$icon("trash")))
      ),
      shiny$actionButton(ns('salvar'), "Salvar alterações", icon = shiny$icon("cloud-arrow-up"))
    ),
    shiny$column(
      8,
      reactableOutput(ns('tbl'))
    )


  )

}

#' @export
server <- function(id, db_pool, reac_geral) {

  shiny$moduleServer(id, function(input, output, session) {

    reac <- shiny$reactiveValues(usuario_sel = NULL)

    shiny$observeEvent(input$adicionar, {

      if (is.null(reac_geral$usuario_sel)) {

        if (length(reac_geral$usuario$id) > 0) {
          novo_id <- max(reac_geral$usuario$id) + 1
        } else {
          novo_id <- 1
        }



        reac_geral$usuario <-
          shiny$isolate(
            reac_geral$usuario |>
              add_row(id = novo_id, nome = input$nome, add = TRUE, new = TRUE)
          )

      } else {

        reac_geral$usuario <-
          shiny$isolate(
            reac_geral$usuario |>
              mutate(
                nome = ifelse(id == reac_geral$usuario_sel$id, input$nome, nome),
                add = ifelse(id == reac_geral$usuario_sel$id, TRUE, add),
                new = ifelse(id == reac_geral$usuario_sel$id, FALSE, add)
              )
          )

      }

      reset('nome')

    })

    shiny$observeEvent(input$salvar, {

      shiny_aux$modal_loader("Aguarde", "Salvando usuários")

      df_aux <-
        reac_geral$usuario |>
        filter(add) |>
        mutate(id = ifelse(new, NA, id)) |>
        select(-add, -new)

      resp <- dados$upsert_df(db_pool, 'usuario', df_aux)

      if (resp$status) {

        shiny_aux$close_modal_loader()
        shinyalert("Usuários salvos", type = 'success')

      } else {

        shiny_aux$close_modal_loader()
        shinyalert("Usuários salvos", text = resp$error, type = 'error')

      }

      reac_geral$usuario <- reac_geral$usuario |> mutate(add = FALSE, new = FALSE)

    })

    output$tbl <- renderReactable({
      vis_tabelas$tbl_usuario(db_pool, df = reac_geral$usuario)
    })

    shiny$observeEvent(input$tbl__reactable__selected, ignoreNULL = FALSE, {

      if (!is.null(input$tbl__reactable__selected)) {

        reac_geral$usuario_sel <-
          reac_geral$usuario |>
          slice(input$tbl__reactable__selected)

        shiny$updateTextInput(inputId = 'nome', value = reac_geral$usuario_sel$nome)
        shiny$updateActionButton(inputId = 'adicionar', label = "Atualizar", icon = shiny$icon("pen-to-square"))
        show('deletar')

      } else {

        shiny$updateActionButton(inputId = 'adicionar', label = "Adicionar", icon = shiny$icon("circle-plus"))
        reac_geral$usuario_sel <- NULL
        hide('deletar')
        reset('nome')

      }


    })

    shiny$observeEvent(input$deletar, {

      dados$delete_from_tabela(db_pool, 'usuario', id = reac_geral$usuario_sel$id)
      reac_geral$usuario <- reac_geral$usuario |> filter(id != reac_geral$usuario_sel$id)
      reset('nome')

    })

  })

}
