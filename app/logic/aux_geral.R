box::use(
  glue[glue, glue_collapse],
  purrr[map_chr],
  reactable[reactable, reactableTheme, colDef, colFormat],
  lubridate[month, year],
  stringr[str_to_title],
  stats[setNames],
  scales[number_format],
  shiny[tags],
  dplyr[mutate],
  tippy[tippy],
  shinyjs[runjs],
  shiny[tags],
)


#' @export
vct_meses <- function() {

  1:12 |> setNames(
    month(
      x = 1:12,
      label = TRUE,
      abbr = FALSE) |>
      str_to_title()
  )

}

#' @export
vct_anos <- function(anos_p_tras = 3) {

  ano <- year(Sys.Date())
  seq(ano - anos_p_tras, ano, 1)

}


#' @export
ano_ultimo_mes <- function() {

  mes_atual <- lubridate::floor_date(Sys.Date(), unit = 'month')
  year(mes_atual - months(1))

}

#' @export
ultimo_mes <- function() {

  month(Sys.Date() - months(1)) |>
    setNames(
      month(
        Sys.Date() - months(1),
        label = TRUE,
        abbr = FALSE) |>
        str_to_title())

}




#' @export
add_equal_to <- function(col_name, x) {

  if (length(x) > 1) {

    eq_t <- glue::glue(' in ({paste0(x, collapse = \',\')})')

  } else {

    eq_t <- glue::glue(" = '{x}'")


  }

  glue::glue(col_name, eq_t)
}

#' @export
create_where <- function(colunas) {

  where <- ''

  if (length(colunas) > 0) {
    where <-
      glue(" where ",
           seq_along(colunas) |>
             map_chr(~add_equal_to(names(colunas[.x]), colunas[.x])) |>
             glue_collapse(' and '))

  }

  return(where)

}


#' @export
reactable_padrao <- function(df,...) {

  colDefs <- list(...)

  df |>
    reactable(
      onClick = 'select',
      selection = "single",
      theme = reactableTheme(
        rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d"),
        searchInputStyle = list(`align-self` = 'flex-start')
      ),
      searchable = TRUE,
      pagination = F,
      highlight = TRUE,
      outlined  =  TRUE,
      rowStyle = list(cursor = "pointer"),
      wrap = FALSE,
      defaultColDef = colDef(align = 'center', footerStyle = list(fontWeight = "bold"), headerStyle = list(fontWeight = "bold")),
      columns = colDefs
    )

}

#' @export
create_check_col <- function(value) {

  ifelse(value, '\U0002713', '\U0002717')

}

#' @export
lbl_mes <- function(value) {

  value |>
    month(label = TRUE, abbr = FALSE) |>
    str_to_title()
}

#' @export
real_fmt <- number_format(accuracy = 0.01, prefix = "R$ ", big.mark = ".", decimal.mark = ",")

#' @export
lgl_style <- function(value) {

  if (value) {
    color <- '#00ff00'
  } else {
    color <- '#ff0000'
  }

  list(color = color, fontWeight = "bold", fontSize = '20px')

}

#' @export
with_tooltip <- function(value, tooltip, ...) {

  tags$div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
           tippy(value, tooltip, ...))
}

#' @export
with_tooltip_by_size <- function(value) {

  if (nchar(value) > 50) {

    tooltip <- value
    texto <- paste0(substr(value, 1,50), "...")

    tags$div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
             tippy(texto, tooltip))

  } else {
    value
  }

}


#' @export
focus_input <- function(id) {
  runjs(glue("document.getElementById('app-<<id>>').focus();", .open = "<<", .close = ">>"))
}

#' @export
parcela_style <- function(value) {

  if (value == "1/1") {
    backgroundColor <- '#D4DAF7'
      color <- '#2A366F'
  } else if (eval(parse(text = value)) < 1) {
    backgroundColor <- '#F7D4DA'
      color <- '#6F2A36'
  } else {
    backgroundColor <- '#D8F5D6'
      color <- '#2F532D'
  }


  list(
    display = "inline-block", padding = "2px 12px", borderRadius = "15px",
    color = color, fontWeight = "600", fontSize = '12px', backgroundColor = backgroundColor)

}

#' @export
set_class_parcela <- function(value) {


  if (!is.na(value)) {
    status <- if (value == "1/1") {"pending"} else if (eval(parse(text = value)) < 1) {"danger"} else {"success"}
    class <- paste0("reactable-tag reactable-status-", tolower(status))
    label <- if (value == "1/1") {"M"} else {value}
    tags$div(class = class, label)
  } else {
    tags$div(value)
  }

}
