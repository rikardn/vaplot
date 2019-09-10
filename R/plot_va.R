#' Plot variability attribution
#'
#' @param va_results A va_result as produced by compute_va
#'
#' @export
plot_va <- function(va_results){

  va_table <- va_results$table
  idv_var <- names(which(va_results$column_types == "idv"))
  facet_vars <- names(which(va_results$column_types == "facet_var"))
  variability_vars <- names(which(va_results$column_types == "variability"))

  plot_tab <- tidyr::gather(va_table, "source", "value", variability_vars)
  p <- ggplot2::ggplot(plot_tab, ggplot2::aes_string(idv_var, "value", fill = "source"))+
    ggplot2::geom_area(position = position_fill(reverse = T))+
   # scale_fill_manual("Source", values = fct_colors)+
    ggplot2::scale_y_continuous("Percent of total variability", labels = scales::percent)+
    ggplot2::theme(legend.position = "bottom")

  if(!rlang::is_empty(facet_vars)){
    p <- p + ggplot2::facet_wrap(facet_vars, labeller = ggplot2::label_both)
  }
  return(p)
}
