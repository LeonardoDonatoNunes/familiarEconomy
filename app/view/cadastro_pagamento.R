box::use(
  shiny,
  reactable[reactableOutput, renderReactable],
  dplyr[slice, pull, filter, mutate, select, add_row, left_join],
  shinyjs[show, hide, hidden, reset],
  shinyWidgets,
  shinyalert[shinyalert],
)

box::use(

  app/logic/dados,
  app/logic/aux_geral,
  app/logic/vis_tabelas,
  app/view/shiny_aux

)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$fluidPage(

    shiny$column(
      3,

      shiny$tags$fieldset(
        shiny$tags$legend("Cadastro de pagamentos"),
        shiny$selectInput(ns('cat_geral_id'), "Categoria geral", choices = NULL),
        shiny$selectInput(ns('cat_espec_id'), "Categoria específica", choices = NULL),
        shiny$checkboxInput(ns('mostrar_todas_categorias'), "Mostrar todas as categorias específicas", value = FALSE),
        shiny$checkboxInput(ns('somente_categorias_usuario'), "Mostrar somente categorias do usuário", value = FALSE),
        shiny$selectInput(ns('ano_ref'), "Ano referência", choices = aux_geral$vct_anos(), selected = aux_geral$ano_ultimo_mes()),
        shiny$selectInput(ns('mes_ref'), "Mês referência", choices = aux_geral$vct_meses(), selected = aux_geral$ultimo_mes()),
        shinyWidgets$currencyInput(ns('valor'), "Valor", 0, format = "Brazilian"),
        shiny$numericInput(ns('numero_parcelas'), "Número de parcelas", value = 1, min = 1),
        shiny$numericInput(ns('parcela_numero'), "Parcela", value = 1, min = 1),
        shiny$actionButton(ns('adicionar'), "Adicionar", icon = shiny$icon("circle-plus")),
        hidden(shiny$actionButton(ns('arquivar'), "Arquivar", icon = shiny$icon("box"))),
        hidden(shiny$actionButton(ns('deletar'), "Deletar", icon = shiny$icon("trash")))
      ),

      shiny$actionButton(ns('desfazer'), "Desfazer alterações", icon = shiny$icon("trash-arrow-up")),
      shiny$actionButton(ns('salvar'), "Salvar", icon = shiny$icon("cloud-arrow-up"))

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
server <- function(id, reac_geral, db_pool = NULL){

  shiny$moduleServer(id, function(input, output, session) {

    ns <- shiny$NS(id)
    reac <- shiny$reactiveValues(pagamento_sel = NULL)

    shiny$observe({
      if (!is.null(db_pool)) {

        shiny$isolate({
          reac_geral$cat_geral <- dados$get_cat_geral(db_pool)
          reac_geral$cat_espec <- dados$get_cat_espec(db_pool)
          shiny$updateSelectInput(inputId = 'cat_geral_id', choices = dados$vct_cat_geral(db_pool, reac_geral$cat_geral))
        })
      }
    })

    buscar_pagamentos <- shiny$reactive({

      if (!is.null(input$filtrar_periodo) & !is.null(input$filtrar_usuario) & !is.null(reac_geral$usuario_sel)) {

        input$salvar

        ano_ref <- NULL
        mes_ref <- NULL
        if (input$filtrar_periodo) {
          ano_ref <- input$ano_ref
          mes_ref <- input$mes_ref
        }

        usuario_id <- NULL
        if (input$filtrar_usuario) {
          usuario_id <- reac_geral$usuario_sel
        }

        reac_geral$pagamento <- dados$get_pagamento(db_pool, ano_ref = ano_ref, mes_ref = mes_ref, usuario_id = usuario_id)


      }

    })


    shiny$observe({
      if (!is.null(db_pool)) {
        buscar_pagamentos()
      }
    })

    shiny$observe({

      if (!is.null(reac_geral$cat_espec)) {

        if (all(!is.null(input$cat_geral_id), !is.null(input$mostrar_todas_categorias), !is.null(input$somente_categorias_usuario))) {

          aux_df <- reac_geral$cat_espec

          if (!input$mostrar_todas_categorias) {

            aux_df <- aux_df |> filter(cat_geral_id == input$cat_geral_id)

          }

          if (input$somente_categorias_usuario) {

            aux_df <- aux_df |> filter(usuario_id == reac_geral$usuario_sel)

          }


          vct_choices <- dados$vct_cat_espec(db_pool, df = aux_df)
          shiny$updateSelectInput(inputId = 'cat_espec_id', choices = vct_choices)

        }
      }

    })


    shiny$observeEvent(input$adicionar, {

      id_temp <- reac$pagamento_sel$id
      novo_id <- FALSE

      if (is.null(id_temp)) {

        novo_id <- TRUE

        if (nrow(reac_geral$pagamento) > 0) {
          id_temp <-   reac_geral$pagamento$id |> max() + 1
        } else {
          id_temp <- 1
        }
      }

      nova_linha = data.frame(
        id = id_temp,
        cat_espec_id = as.numeric(input$cat_espec_id),
        ano_ref = as.numeric(input$ano_ref),
        mes_ref = as.numeric(input$mes_ref),
        numero_parcelas = input$numero_parcelas,
        parcela_numero = input$parcela_numero,
        valor = input$valor,
        usuario_id = as.numeric(reac_geral$usuario_sel),
        add = TRUE,
        new = novo_id
      ) |>
        left_join(reac_geral$cat_espec |> select(cat_espec_id = id, especifica = nome, dividido, mensal, cat_geral_id, geral = nome_cat_geral), by = 'cat_espec_id') |>
        left_join(reac_geral$usuario |> select(usuario_id = id, usuario = nome), by = 'usuario_id')


      reac_geral$pagamento <- shiny$isolate(
        reac_geral$pagamento |>
          filter(id != nova_linha$id) |>
          add_row(nova_linha)
      )

      reset('nome')
      reset('descricao')

      aux_geral$focus_input(ns('cat_espec_id'))


    })

    shiny$observeEvent(input$salvar, {

      shiny_aux$modal_loader("Aguarde", "Salvando pagamentos")

      df_aux <-
        reac_geral$pagamento |>
        filter(add) |>
        mutate(id = ifelse(new, NA, id)) |>
        select(id, usuario_id, cat_espec_id, ano_ref, mes_ref, valor, numero_parcelas, parcela_numero)

      resp <- dados$upsert_df(db_pool, 'pagamento', df_aux)

      if (resp$status) {

        reac_geral$pagamento <- buscar_pagamentos()
        shiny_aux$close_modal_loader()
        shinyalert("Pagamentos salvos", type = 'success')

      } else {

        shiny_aux$close_modal_loader()
        shinyalert("Erro", text = resp$error, type = 'error')

      }

    })

    shiny$observeEvent(input$tbl__reactable__selected, ignoreNULL = FALSE, {

      if (!is.null(input$tbl__reactable__selected)) {

        reac$pagamento_sel <-
          reac_geral$pagamento |>
          slice(input$tbl__reactable__selected)

        shiny$updateSelectInput(inputId = 'cat_geral_id', selected = reac$pagamento_sel$cat_geral_id)
        shiny$updateSelectInput(inputId = 'cat_espec_id', selected = reac$pagamento_sel$cat_espec_id)
        shiny$updateSelectInput(inputId = 'ano_ref', selected = reac$pagamento_sel$ano_ref)
        shiny$updateSelectInput(inputId = 'mes_ref', selected = reac$pagamento_sel$mes_ref)
        shiny$updateNumericInput(inputId = 'valor', value = reac$pagamento_sel$valor)
        shiny$updateNumericInput(inputId = 'numero_parcelas', value = reac$pagamento_sel$numero_parcelas)
        shiny$updateNumericInput(inputId = 'parcela_numero', value = reac$pagamento_sel$parcela_numero)
        shiny$updateActionButton(inputId = 'adicionar', label = "Atualizar", icon = shiny$icon("pen-to-square"))
        show('arquivar')
        show('deletar')

      } else {

        reac$pagamento_sel <- NULL
        hide('arquivar')
        hide('deletar')
        reset('valor')
        reset('numero_parcelas')
        reset('parcela_numero')
        shiny$updateActionButton(inputId = 'adicionar', label = "Adicionar", icon = shiny$icon("circle-plus"))

      }


    })

    shiny$observeEvent(input$deletar, {

      df_aux <-
        reac_geral$pagamento |>
        filter(id == reac$pagamento_sel$id)

      if (df_aux$new | df_aux$add) {

        reac_geral$pagamento <- reac_geral$pagamento |> filter(id != reac$pagamento_sel$id)
        reset('nome')
        reset('descricao')

      } else {

        dados$delete_from_tabela(db_pool, 'pagamento', id = reac$pagamento_sel$id)
        reac_geral$pagamento <- reac_geral$pagamento |> filter(id != reac$pagamento_sel$id)
        reset('nome')
        reset('descricao')
      }

    })

    shiny$observeEvent(input$desfazer, {
      reac_geral$pagamento <-
        reac_geral$pagamento |>
        filter(!new | !add)
    })

    output$tbl <- renderReactable({
      if (!is.null(reac_geral$pagamento)) {
        vis_tabelas$tbl_pagamentos(db_pool, df = reac_geral$pagamento)
      }
    })


  })
}
