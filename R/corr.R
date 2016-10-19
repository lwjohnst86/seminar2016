# Functions to run the correlation analysis
#
# Analyze -----------------------------------------------------------------

#' Correlation analysis.
#'
#' @param data Project data
#' @param x The covariates and outcomes
#' @param y The fatty acids.
#'
#' @export
analyze_corr <-
    function(data = project_data,
             x = tg_pct) {

        name_order <- substr(x, nchar(x), nchar(x))
        name_order <- as.integer(name_order)
        name_order <- x[order(name_order)]

    data %>%
        dplyr::filter(VN == 0) %>%
        dplyr::select_(.dots = x) %>%
        mason::design('cor') %>%
        mason::add_settings(method = 'pearson', use = 'complete.obs', hclust.order = TRUE) %>%
        mason::add_variables('xvars', x) %>%
        mason::construct() %>%
        mason::scrub() %>%
        mason::polish_renaming(renaming_fats, 'Vars2') %>%
        mason::polish_renaming(renaming_fats, 'Vars1') %>%
        dplyr::mutate(Vars2 = factor(Vars2, unique(Vars2)),
                      Vars1 = factor(Vars1, unique(Vars1)))
    }

# Plotting ----------------------------------------------------------------

#' Correlation heatmap plot.
#'
#' @param results Correlation results
#'
#' @export
plot_heatmap <- function(results) {
     results %>%
        seer::view_heatmap(values.text = FALSE,
            ylab = 'Triacylglycerol fatty acids (mol%)',
            number.colours = 5) +
        graph_theme(ticks = FALSE, legend.pos = 'right') +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
}
