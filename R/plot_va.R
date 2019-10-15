#' Plot variability attribution
#'
#' @param result A va_result as produced by compute_va
#' @param coloring Color specifications
#'
#' @export
plot_va <- function(result, coloring = coloring_default, smooth = FALSE){

  va_table <- result$table
  if(smooth){
      if(!rlang::is_empty(get_facet_cols(result)))
        va_table <- dplyr::group_by_at(va_table, get_facet_cols(result))
      va_table <- dplyr::group_modify(va_table,
                                      function(df, ...)
                                        dplyr::mutate_at(df, get_variability_cols(result), ~smooth.spline(.data[[get_idv_col(result)]],log(.+1E-16))$y %>% exp))
  }
  plot_tab <- tidyr::gather(va_table, "source", "value", get_variability_cols(result)) %>%
    dplyr::mutate(source = factor(source, levels = get_variability_cols(result)))
  if(is.function(coloring)) colors <- coloring(result)
  if(is.null(coloring)){
    ui_inform("No colors were provided, the plot will use default colors instead.")
    colors <- color_like_hadley(result)
  }
  p <- ggplot2::ggplot(plot_tab, ggplot2::aes_string(get_idv_col(result), "value", fill = "source"))+
    ggplot2::geom_area(position = ggplot2::position_fill(reverse = T))+
    ggplot2::scale_fill_manual("Source", values = colors)+
    ggplot2::scale_y_continuous("Percent of total variability", labels = percent_labels)+
    ggplot2::theme(legend.position = "bottom")

  facet_vars <- get_facet_cols(result)
  if(!rlang::is_empty(facet_vars)){
    p <- p + ggplot2::facet_wrap(facet_vars, labeller = ggplot2::label_both)
  }
  return(p)
}

plot_va_compare <- function(..., coloring = coloring_default, smooth = FALSE){
  result <- combine_results(...)
  va_table <- result$table

  plot_tab <- tidyr::gather(va_table, "source", "value", get_variability_cols(result)) %>%
    dplyr::mutate(source = factor(source, levels = get_variability_cols(result)))
  if(is.function(coloring)) colors <- coloring(result)
  if(is.null(coloring)){
    ui_inform("No colors were provided, the plot will use default colors instead.")
    colors <- color_like_hadley(result)
  }
  p <- ggplot2::ggplot(plot_tab, ggplot2::aes_string(get_idv_col(result), "value", fill = "source"))+
    ggplot2::geom_area(position = ggplot2::position_fill(reverse = T))+
    ggplot2::scale_fill_manual("Source", values = colors)+
    ggplot2::scale_y_continuous("Percent of total variability", labels = percent_labels)+
    ggplot2::theme(legend.position = "bottom")

  facet_vars <- get_facet_cols(result)
  if(length(facet_vars)==1){
    p <- p + ggplot2::facet_wrap(".result", labeller = ggplot2::label_value)
  }else{
    facet_vars <- facet_vars[facet_vars!=".result"]
    p <- p + ggplot2::facet_grid(rows = ggplot2::vars(!!!rlang::syms(facet_vars)),
                                 cols = ggplot2::vars(.result),
                                 labeller = ggplot2::labeller(.result = ggplot2::label_value,
                                                              .default = ggplot2::label_both))
  }
  return(p)
}

percent_labels <- function(x){
  paste0(format(x*100, digits = 0), "%")
}
