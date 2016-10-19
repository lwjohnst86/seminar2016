# Functions for the GEE analysis
#
# Grab or combine data ----------------------------------------------------

#' Prepare the project data for analysis through GEE.
#'
#' @param data project data
#' @export
prep_gee_data <- function(data) {
    no_fattyacids <- data %>%
        dplyr::select(-dplyr::matches('pct_tg\\d+|^tg\\d+'),
                      -TotalNE,-TotalTG, -BaseTAG, -lBaseTAG)

    scaled_variables <- data %>%
        dplyr::filter(VN == 0) %>%
        dplyr::select(SID, TotalNE, TotalTG, BaseTAG, lBaseTAG,
                      dplyr::matches('pct_tg\\d+|^tg\\d+')) %>%
        dplyr::mutate_each(dplyr::funs(as.numeric(scale(.))), -SID)

    dplyr::full_join(
            no_fattyacids,
            scaled_variables,
            by = 'SID'
        ) %>%
        dplyr::group_by(VN) %>%
        dplyr::mutate(
            Waist = as.numeric(scale(Waist)),
            ALT = as.numeric(scale(ALT)),
            BaseAge = as.numeric(scale(BaseAge)),
            MET = as.numeric(scale(MET))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(SID, VN)
}

# Analyze -----------------------------------------------------------------

#' Run GEE models on the prepared project data.
#'
#' @param data The project data
#' @param y outcomes (IS, BCF)
#' @param x predictors (TAGFA)
#' @param covariates to adjust for
#' @param intvar interaction variable
#' @param rename_x Function to rename x variables
#' @param rename_y Function to rename y variables
#' @export
analyze_gee <- function(data = project_data,
                        y = outcomes,
                        x = list(
                            tg_conc = tg_conc,
                            tg_pct = tg_pct,
                            tg_total = tg_totals
                        ),
                        covars = covariates,
                        intvar = NULL,
                        rename_x = renaming_fats,
                        rename_y = renaming_outcomes) {

    int <- !is.null(intvar)
    if (int) {
        extract_term <- ':'
    } else {
        extract_term <- 'Xterm$'
    }
    data %>%
        prep_gee_data() %>%
        mason::design('gee') %>%
        mason::add_settings(family = stats::gaussian(),
                            corstr = 'ar1', cluster.id = 'SID') %>%
        mason::add_variables('yvars', y) %>%
        mason::add_variables('xvars', x[['tg_pct']]) %>%
        mason::add_variables('covariates', covars) %>% {
            if (int) {
                mason::add_variables(., 'interaction', intvar)
            } else {
                .
            }
        } %>%
        mason::construct() %>%
        mason::add_variables('xvars', x[['tg_conc']]) %>%
        mason::construct() %>%
        mason::add_variables('xvars', x[['tg_total']]) %>%
        mason::construct() %>%
        mason::scrub() %>%
        mason::polish_filter(extract_term, 'term') %>%
        dplyr::mutate(unit = ifelse(grepl('pct', Xterms), 'mol%',
                                    ifelse(grepl('^tg\\d', Xterms), 'nmol/mL',
                                           'Totals'))) %>%
        mason::polish_transform_estimates(function(x) (exp(x) - 1) * 100) %>%
        mason::polish_renaming(rename_x, 'Xterms') %>%
        mason::polish_renaming(rename_y, 'Yterms') %>%
        dplyr::mutate(
            order1 = substr(Xterms, nchar(Xterms), nchar(Xterms)),
            order1 = ifelse(order1 == 0, 10, order1),
            order1 = ifelse(order1 == 'l', 20, order1),
            order1 = ifelse(order1 == 'G', 30, order1),
            order1 = as.integer(order1)
        ) %>%
        mason::polish_adjust_pvalue(method = 'BH') %>%
        dplyr::rename(unadj.p.value = p.value, p.value = adj.p.value) %>%
        dplyr::arrange(desc(order1)) %>%
        dplyr::mutate(Yterms = factor(
            Yterms,
            levels = c('log(1/HOMA-IR)', 'log(ISI)',
                       'log(IGI/IR)', 'log(ISSI-2)'),
            labels = c('log(1/HOMA-IR)', 'log(ISI)',
                       'log(IGI/IR)', 'log(ISSI-2)'),
            ordered = TRUE),
            Xterms = factor(Xterms, unique(Xterms))) %>%
        dplyr::select(-order1)

}

# Plotting ----------------------------------------------------------------

#' Plot the GEE results in a Forest plot style.
#'
#' @param results Results data frame from the GEE analysis
#'
#' @export
plot_gee_main <- function(results) {
    results %>%
        dplyr::mutate(p.value = ifelse(p.value > 0.05, 1, 0.04)) %>%
        seer::view_main_effect(
            graph.options = 'dot.size',
            groups = 'unit~Yterms',
            legend.title = 'FDR\nadjusted\np-values',
            xlab = 'Percent difference with 95% CI in the outcomes\nfor each SD increase in fatty acid',
            ylab = 'Triacylglycerol fatty acids'
            ) +
        graph_theme(ticks = FALSE, legend.pos = 'right') +
        ggplot2::theme(legend.margin = grid::unit(0, 'cm'))
}
