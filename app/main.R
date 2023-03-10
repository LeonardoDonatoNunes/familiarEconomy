box::use(
  shiny[bootstrapPage, updateQueryString, getQueryString, textAreaInput, actionLink, checkboxInput, modalButton, freezeReactiveValue, moduleServer, observe, onStop, NS,reactive, renderText,actionButton, tags, textOutput, icon, renderUI, selectInput, tagList, reactiveValues, observeEvent, verbatimTextOutput, renderPrint, showModal, modalDialog],
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
  app/logic/aux_geral,
  app/logic/dados,
  app/view/cadastro_usuario,
  app/view/cadastro_receita,
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
            id = ns('sidebarID'),
            shinydashboard$menuItem("Painel", tabName = 'painel_inicial', icon = icon("chart-line")),
            shinydashboard$menuItem("Cadastrar Pagamentos", tabName = 'cadastro_pagamento', icon = icon("money-bill-transfer")),
            shinydashboard$menuItem("Cadastrar usuário", tabName = 'cadastro_usuario', icon = icon("user")),
            shinydashboard$menuItem("Cadastrar receita", tabName = 'cadastro_receita', icon = icon("hand-holding-dollar")),
            shinydashboard$menuItem("Cadastrar Categoria geral", tabName = 'cadastro_cat_geral', icon = icon("sitemap")),
            shinydashboard$menuItem("Cadastrar Categoria específica", tabName = 'cadastro_cat_espec', icon = icon("list-ul"))
          ),
          actionLink(ns('abrir_terminal'), "Terminal ", icon = icon("r-project"))

        ),

        shinydashboard$dashboardBody(
          useShinyjs(),

          shinydashboard$tabItems(
            shinydashboard$tabItem('painel_inicial', painel_inicial$ui(ns('painel_inicial'))),
            shinydashboard$tabItem('cadastro_pagamento', cadastro_pagamento$ui(ns('cadastro_pagamento'))),
            shinydashboard$tabItem('cadastro_usuario', cadastro_usuario$ui(ns('cadastro_usuario'))),
            shinydashboard$tabItem('cadastro_receita', cadastro_receita$ui(ns('cadastro_receita'))),
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
    reac <- reactiveValues()

    observeEvent(getQueryString(session)$tab, {
      currentQueryString <- getQueryString(session)$tab # alternative: parseQueryString(session$clientData$url_search)$tab
      if(is.null(input$sidebarID) || !is.null(currentQueryString) && currentQueryString != input$sidebarID){
        freezeReactiveValue(input, "sidebarID")
        shinydashboard$updateTabItems(session, "sidebarID", selected = currentQueryString)
      }
    }, priority = 1)

    observeEvent(input$sidebarID, {
      currentQueryString <- getQueryString(session)$tab # alternative: parseQueryString(session$clientData$url_search)$tab
      pushQueryString <- paste0("?tab=", input$sidebarID)
      if(is.null(currentQueryString) || currentQueryString != input$sidebarID){
        freezeReactiveValue(input, "sidebarID")
        updateQueryString(pushQueryString, mode = "push", session)
      }
    }, priority = 0)



    localStorageAux <- ns('localStorage')

    runjs(glue("Shiny.setInputValue('<<localStorageAux>>', localStorage);", .open = "<<", .close = ">>"))


    shiny_aux$modal_loader("Aguarde", "Criando conexão com o banco de dados")

    db_pool <- NULL
    if (is_online()) {

      tryCatch({
        db_pool <- db_connect$pool_db()
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
            !is.null(reac_geral$receita),
            !is.null(reac_geral$cat_geral),
            !is.null(reac_geral$cat_espec),
            !is.null(reac_geral$pagamento)
          )

        if (teste) {

          shiny_aux$close_modal_loader()

        }


      })


      observeEvent(input$usuario_sel, {

        reac_geral$usuario_sel <- as.numeric(input$usuario_sel)

        runjs(
          glue("localStorage.setItem(\"usuario\", '{\"id\": <<input$usuario_sel>>}');", .open = "<<", .close = ">>")
        )

      })

      observeEvent(input$abrir_terminal, {
        modal_terminal()
      })


      painel_inicial$server('painel_inicial', db_pool)
      cadastro_usuario$server('cadastro_usuario', db_pool, reac_geral)
      cadastro_receita$server('cadastro_receita', reac_geral, db_pool)
      cadastro_pagamento$server('cadastro_pagamento', reac_geral, db_pool)
      cadastro_cat_geral$server('cadastro_cat_geral', reac_geral, db_pool)
      cadastro_cat_espec$server('cadastro_cat_espec', reac_geral, db_pool)




      observeEvent(input$executar, {

        tryCatch({

          resultado_r <- eval(parse(text = input$terminal_r))

          if (input$formatar_real) {

            if (is.numeric(resultado_r)) {
              reac$resultado_r <- aux_geral$real_fmt(resultado_r)
            } else {
              reac$resultado_r <- resultado_r
            }

          } else {
            reac$resultado_r <- resultado_r
          }

        }, error = function(e) {

          reac$resultado_r <- e

        })

      })

      output$resultado_r <- renderText({reac$resultado_r})




      modal_terminal <- function() {

        showModal(
          modalDialog(

            title = NULL,
            text = NULL,
            footer = list(modalButton(label = "Encerrar")),

            tagList(
              tags$fieldset(
                class = 'terminal_r',
                tags$legend("Rascunho R"),
                textAreaInput(
                  ns('terminal_r'),
                  label = NULL,
                  height = '150px',
                  width = '80%',
                  placeholder = "Terminal R para executar contas e funções simples do R base"),
                actionButton(ns('executar'), "Executar", icon = icon('r-project')),
                checkboxInput(ns('formatar_real'), "Formatar como Real", value = TRUE),
                tags$h3(class = 'console_r', textOutput(ns('resultado_r')))
              )
            )

          )
        )
      }




    }

  })
}
