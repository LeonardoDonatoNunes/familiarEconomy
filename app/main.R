box::use(
  shiny[bootstrapPage, moduleServer, observe, onStop, NS,reactive, renderText,actionButton, tags, textOutput, icon, renderUI, selectInput, tagList, reactiveValues, observeEvent, verbatimTextOutput, renderPrint],
  shinyjs[useShinyjs, refresh, runjs],
  shinydashboard,
  stats[setNames],
  pool[poolClose],
  pingr[is_online],
  shinyalert[shinyalert],
  jsonlite[fromJSON],
  glue[glue],
)

box::use(
  app/logic/db_connect,
  app/logic/dados,
  app/view/cadastro_usuario,
  # app/view/cadastro_receita,
  app/view/cadastro_pagamento,
  app/view/cadastro_cat_geral,
  app/view/cadastro_cat_espec,
  app/view/painel_inicial,
  app/view/shiny_aux,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  bootstrapPage(
    tags$div(
      class = "components-container",

      shinydashboard$dashboardPage(
        skin = "yellow",

        shinydashboard$dashboardHeader(
          title = tags$span(tags$img(src="static/logo.png", height="90%")),
          shinydashboard$dropdownMenuOutput(ns("teste"))

        ),

        shinydashboard$dashboardSidebar(

          shinydashboard$sidebarMenu(
            shinydashboard$menuItem("Painel", tabName = 'painel_inicial', icon = icon("chart-line")),
            shinydashboard$menuItem("Cadastrar Pagamentos", tabName = 'cadastro_pagamento', icon = icon("money-bill-transfer")),
            shinydashboard$menuItem("Cadastrar usuário", tabName = 'cadastro_usuario', icon = icon("user")),
            # shinydashboard$menuItem("Cadastrar receita", tabName = 'cadastro_receita', icon = icon("hand-holding-dollar")),
            shinydashboard$menuItem("Cadastrar Categoria geral", tabName = 'cadastro_cat_geral', icon = icon("sitemap")),
            shinydashboard$menuItem("Cadastrar Categoria específica", tabName = 'cadastro_cat_espec', icon = icon("list-ul"))
          )

        ),

        shinydashboard$dashboardBody(
          useShinyjs(),

          shinydashboard$tabItems(
            shinydashboard$tabItem('painel_inicial', painel_inicial$ui(ns('painel_inicial'))),
            shinydashboard$tabItem('cadastro_pagamento', cadastro_pagamento$ui(ns('cadastro_pagamento'))),
            shinydashboard$tabItem('cadastro_usuario', cadastro_usuario$ui(ns('cadastro_usuario'))),
            # shinydashboard$tabItem('cadastro_receita', cadastro_receita$ui(ns('cadastro_receita'))),
            shinydashboard$tabItem('cadastro_cat_geral', cadastro_cat_geral$ui(ns('cadastro_cat_geral'))),
            shinydashboard$tabItem('cadastro_cat_espec', cadastro_cat_espec$ui(ns('cadastro_cat_espec')))
          )

        )

      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- NS(id)

    localStorageAux <- ns('localStorage')

    runjs(glue("Shiny.setInputValue('<<localStorageAux>>', localStorage);", .open = "<<", .close = ">>"))


    shiny_aux$modal_loader("Aguarde", "Criando conexão com o banco de dados")

    db_pool <- NULL
    if (is_online()) {

      tryCatch({
        db_pool <- db_connect$create_db_pool()
      }, error = function(e) {

        shiny_aux$close_modal_loader()

        shiny_aux$modal_error_w_reload(
          mensagem = "Erro",
          sub_mensagem = paste0("Falha na conexao com o banco. ERROR: ", e)
        )
      })


    } else {

      shiny_aux$close_modal_loader()
      shiny_aux$modal_error_w_reload(
        mensagem = "Sem conexão",
        sub_mensagem = "Sem coneção com a internet. Confira a conexão e atualize a página."
      )

    }

    if (!is.null(db_pool)) {

      reac_geral <- reactiveValues(usuario = dados$get_usuario(db_pool))

      onStop(function() {

        cat("Closing Database Connections")

        poolClose(db_pool)

      })


      # Seleção e controle de usuario -------------------------------
      usuario_selected <- reactive({

        if (!is.null(input$localStorage$usuario)) {
          aux <- input$localStorage$usuario |> fromJSON()
          aux$id
        } else {
          NULL
        }

      })

      output$teste <- renderUI({

        if (!is.null(input$localStorage)) {

          shiny_aux$alterar_mensagem_modal_loader("Buscando os dados...")
          vct_usuario <- reac_geral$usuario$id |> setNames(reac_geral$usuario$nome)

          dropdownClass <- paste0("dropdown ", 'notifications', "-menu")

          tagList(
            tags$li(
              class = dropdownClass,
              tags$div(class = 'seletor_usuario',
                       selectInput(
                         ns('usuario_sel'),
                         label = NULL,
                         choices = vct_usuario,
                         width = 'auto',
                         selected = usuario_selected()
                       )
              )
            )
          )

        }
      })


      observe({

        teste <-
          all(
            !is.null(reac_geral$usuario),
            !is.null(reac_geral$cat_geral),
            !is.null(reac_geral$cat_espec),
            !is.null(reac_geral$pagamento)
          )

        if (teste) {

          shiny_aux$close_modal_loader()

        }


      })


      observeEvent(input$usuario_sel, {

        if (input$usuario_sel != '') {
          reac_geral$usuario_sel <- input$usuario_sel
        }

        runjs(
          glue("localStorage.setItem(\"usuario\", '{\"id\": <<input$usuario_sel>>}');", .open = "<<", .close = ">>")
        )

      })


      painel_inicial$server('painel_inicial', db_pool)
      cadastro_usuario$server('cadastro_usuario', db_pool, reac_geral)
      # cadastro_receita$server('cadastro_receita', reac_geral, db_pool)
      cadastro_pagamento$server('cadastro_pagamento', reac_geral, db_pool)
      cadastro_cat_geral$server('cadastro_cat_geral', reac_geral, db_pool)
      cadastro_cat_espec$server('cadastro_cat_espec', reac_geral, db_pool)


    }

  })
}
