box::use(
  shiny,
  reactable[reactableOutput, renderReactable],
  dplyr[slice, pull, filter, mutate, select, left_join, add_row],
  shinyjs[show, hide, hidden, reset],
  shinyWidgets,
  shinyalert[shinyalert]
)

box::use(

  app/logic/dados,
  app/logic/aux,
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
            'Cadastro de categoria (Específica)',
            style = 'float:left'
          )),
        shiny$textInput(ns('nome'), "Nome da categoria"),
        shiny$selectInput(ns('cat_geral_id'), "Categoria geral", choices = NULL),
        shiny$textAreaInput(ns('descricao'), "Descrição"),
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

    reac <- shiny$reactiveValues(cat_espec_sel = NULL)

    shiny$observe({
      if (!is.null(db_pool)) {
        shiny$isolate({
          reac_geral$cat_espec <- dados$get_cat_espec(db_pool)
          shiny$updateSelectInput(inputId = 'cat_geral_id', choices = dados$vct_cat_geral(db_pool, reac_geral$cat_geral))
        })
      }
    })


    shiny$observeEvent(input$adicionar, {

      id_temp <- reac$cat_espec_sel$id
      novo_id <- FALSE

      if (is.null(id_temp)) {

        novo_id <- TRUE

        if (nrow(reac_geral$cat_espec) > 0) {
          id_temp <-   reac_geral$cat_espec$id |> max() + 1
        } else {
          id_temp <- 1
        }
      }

      nova_linha = data.frame(
        id = id_temp,
        nome = input$nome,
        cat_geral_id = as.numeric(input$cat_geral_id),
        usuario_id = as.numeric(reac_geral$usuario_sel),
        descricao = input$descricao,
        add = TRUE,
        new = novo_id
      ) |>
        left_join(reac_geral$cat_geral |> select(cat_geral_id = id, nome_cat_geral = nome, dividido, mensal), by = 'cat_geral_id') |>
        left_join(reac_geral$usuario |> select(usuario_id = id, usuario = nome), by = 'usuario_id')

      reac_geral$cat_espec <- shiny$isolate(
        reac_geral$cat_espec |>
          filter(id != nova_linha$id) |>
          add_row(nova_linha)
      )

      reset('nome')
      reset('descricao')

      aux$focus_input(ns('nome'))


    })

    shiny$observeEvent(input$salvar, {

      shiny_aux$modal_loader("Aguarde", "Salvando categorias")

      df_aux <-
        reac_geral$cat_espec |>
        filter(add) |>
        mutate(id = ifelse(new, NA, id)) |>
        select(id, nome, cat_geral_id, usuario_id, descricao)

      resp <- dados$upsert_df(db_pool, 'categoria_despesa_especifica', df_aux)

      if (resp$status) {

        reac_geral$cat_espec <- dados$get_cat_espec(db_pool)
        shiny_aux$close_modal_loader()
        shinyalert("Categorias específicas salvas", type = 'success')

      } else {

        shiny_aux$close_modal_loader()
        shinyalert("Erro", text = resp$error, type = 'error')

      }

    })

    shiny$observeEvent(input$tbl__reactable__selected, ignoreNULL = FALSE, {

      if (!is.null(input$tbl__reactable__selected)) {

        reac$cat_espec_sel <-
          reac_geral$cat_espec |>
          slice(input$tbl__reactable__selected)

        shiny$updateTextInput(inputId = 'nome', value = reac$cat_espec_sel$nome)
        shiny$updateSelectInput(inputId = 'cat_geral_id', selected = reac$cat_espec_sel$cat_espec_id)
        shiny$updateTextAreaInput(inputId = 'descricao', value = reac$cat_espec_sel$descricao)
        shiny$updateCheckboxInput(inputId = 'mensal', value = reac$cat_espec_sel$mensal)
        shiny$updateCheckboxInput(inputId = 'dividido', value = reac$cat_espec_sel$dividido)
        shiny$updateTextAreaInput(inputId = 'descricao', value = reac$cat_espec_sel$descricao)
        shiny$updateActionButton(inputId = 'adicionar', label = "Atualizar", icon = shiny$icon("pen-to-square"))
        show('arquivar')
        show('deletar')

      } else {

        reac$cat_espec_sel <- NULL
        hide('arquivar')
        hide('deletar')
        reset('nome')
        reset('descricao')
        shiny$updateActionButton(inputId = 'adicionar', label = "Adicionar", icon = shiny$icon("circle-plus"))

      }


    })

    shiny$observeEvent(input$deletar, {

      df_aux <-
        reac_geral$cat_espec |>
        filter(id == reac$cat_espec_sel$id)

      if (df_aux$new | df_aux$add) {

        reac_geral$cat_espec <- reac_geral$cat_espec |> filter(id != reac$cat_espec_sel$id)
        reset('nome')
        reset('descricao')

      } else {

        dados$delete_from_tabela(db_pool, 'categoria_despesa_especifica', id = reac$cat_espec_sel$id)
        reac_geral$cat_espec <- reac_geral$cat_espec |> filter(id != reac$cat_espec_sel$id)
        reset('nome')
        reset('descricao')
      }

    })

    shiny$observeEvent(input$desfazer, {
      reac_geral$cat_espec <-
        reac_geral$cat_espec |>
        filter(!new | !add)
    })

    output$tbl <- renderReactable({
      vis_tabelas$tbl_cat_espec(db_pool, df = reac_geral$cat_espec)
    })

  })
}
