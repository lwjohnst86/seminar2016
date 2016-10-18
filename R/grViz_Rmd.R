

#' Create a diagram from GraphViz commands.
#'
#' @param gr The GraphViz commands, as a single character string.
#' @param width Width dimension of the graph.
#' @param height Height dimension of the graph.
#'
grViz_Rmd <- function(gr, width = NULL, height = NULL) {
    if (interactive()) {
        DiagrammeR::grViz(gr, width = width, height = height)
    } else {
        chunk <- knitr::opts_current$get()
        path <- chunk$fig.path
        gr.path <- paste0(path, chunk$label, '.pdf')
        dir.create(path, showWarnings = FALSE, recursive = TRUE)
        capture.output(
            DiagrammeR::grViz(gr, width = width, height = height) %>%
                DiagrammeRsvg::export_svg() %>%
                charToRaw() %>%
                rsvg::rsvg_pdf(gr.path),
            file = '/dev/null'
        )
        knitr::include_graphics(gr.path)
    }
}
