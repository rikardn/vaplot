#' Plot variability attribution
#'
#' @param va_results A va_result as produced by compute_va
#'
#' @export
plot_va <- function(va_results, colors = NULL){

  va_table <- va_results$table
  idv_var <- names(which(va_results$column_types == "idv"))
  facet_vars <- names(which(va_results$column_types == "facet_var"))
  variability_vars <- names(which(va_results$column_types == "variability"))

  plot_tab <- tidyr::gather(va_table, "source", "value", variability_vars) %>%
    dplyr::mutate(source = factor(source, levels = variability_vars))
  if(is.null(colors)){
    ui_inform("No colors were provided, the plot will use default colors instead.")
    colors <- color_like_hadley(va_results)
  }
  p <- ggplot2::ggplot(plot_tab, ggplot2::aes_string(idv_var, "value", fill = "source"))+
    ggplot2::geom_area(position = ggplot2::position_fill(reverse = T))+
    ggplot2::scale_fill_manual("Source", values = colors)+
    ggplot2::scale_y_continuous("Percent of total variability", labels = percent_labels)+
    ggplot2::theme(legend.position = "bottom")

  if(!rlang::is_empty(facet_vars)){
    p <- p + ggplot2::facet_wrap(facet_vars, labeller = ggplot2::label_both)
  }
  return(p)
}

percent_labels <- function(x){
  paste0(format(x*100, digits = 0), "%")
}
