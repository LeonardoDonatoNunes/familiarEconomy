box::use(
  echarts4r,
  dplyr[filter, pull, mutate, select, add_row, group_by, summarise, ungroup],
  glue[glue],
)

box::use(
  app/logic/db_connect,
  app/logic/dados,
  app/logic/aux_geral,
)

conectar_db <- FALSE

if (conectar_db) {

  db_pool <- db_connect$create_db_pool()

}

#' @export
receita_despesa <- function(db_pool, df = NULL, df_receita = NULL, ano_ref = aux_geral$ano_ultimo_mes(), mes_ref = aux_geral$ultimo_mes()) {

  if (is.null(df)) {

    df <- dados$get_pagamento(db_pool, ano_ref = ano_ref, mes_ref = as.numeric(mes_ref))

  }

  if (is.null(df_receita)) {
    df_receita <- dados$get_receita(db_pool)
  }

  if (nrow(df) > 1) {

    df_aux <-
      df |>
      filter(parcela_numero >= numero_parcelas) |>
      group_by(ano_ref, mes_ref) |>
      summarise(valor = sum(valor)) |>
      ungroup() |>
      mutate(data = as.Date(glue("{ano_ref}-{mes_ref}-01")))

    datas <- seq(df_aux$data, df_aux$data + months(6), by = "month")

    df_aux <-
      data.frame(
        data = datas,
        valor = df_aux$valor,
        receitas = sum(df_receita$valor, na.rm = TRUE)
      )

    df_parcelas <- df |>
      filter(!mensal, parcela_numero < numero_parcelas) |>
      mutate(parcelas = numero_parcelas - (1-parcela_numero)) |>
      select(parcelas, valor)

    for (i in 1:nrow(df_parcelas)) {

      npar <- min(df_parcelas[i,]$parcelas, 7)
      valor_parcela <- df_parcelas[i,]$valor
      zeros <- nrow(df_aux) - npar
      vetor <- c(rep(valor_parcela, npar), rep(0, zeros))

      df_aux <-
        df_aux |>
        mutate(valor = valor + vetor)

    }

    df_gastos_mes <- df |>
      filter(!mensal, parcela_numero == numero_parcelas)

    df_aux[2:7,]$valor <- df_aux[2:7,]$valor - sum(df_gastos_mes$valor)


    df_aux |>
      echarts4r$e_charts(data) |> # initialise and set x
      echarts4r$e_line(valor, smooth = TRUE, name = "Gasto total") |>
      echarts4r$e_labels() |>
      echarts4r$e_axis_labels(x = "Data", y = "Valor (R$)") |>
      echarts4r$e_area(receitas) |>  # add area
      echarts4r$e_title("Despesas Próximos 6 meses") |>  # Add title & subtitle
      echarts4r$e_theme("infographic") |>  # theme
      echarts4r$e_legend(right = 0) |>  # move legend to the bottom
      echarts4r$e_tooltip(trigger = "axis") # tooltip
  }
}






