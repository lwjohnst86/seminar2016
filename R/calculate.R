# Calculate or extract for inline -----------------------------------------

#' Calculate the percent change over time for the outcome variables.
#'
#' @param data Project data.
#'
#' @export
calc_outcome_changes <- function(data = project_data) {
    prep.data <- data %>%
        dplyr::select(f.VN, HOMA, ISI, IGIIR, ISSI2) %>%
        tidyr::gather(Measure, Value,-f.VN) %>%
        stats::na.omit() %>%
        dplyr::group_by(Measure, f.VN) %>%
        dplyr::summarise(med = median(Value),
                         n = n()) %>%
        dplyr::ungroup()

    sample_size <- prep.data$n %>%
        {paste0(min(.), '-', max(.))}

    change_over_time <- prep.data %>%
        dplyr::select(-n) %>%
        tidyr::spread(f.VN, med) %>%
        dplyr::mutate(pctChg = ((yr6 - yr0) / yr0) * 100) %>%
        dplyr::select(pctChg) %>%
        abs() %>%
        round(0) %>%
        {paste0(min(.), '% to ', max(.), '%')}

    pval <- mason::design(data, 'gee') %>%
        mason::add_settings(family = stats::gaussian(), corstr = 'ar1', cluster.id = 'SID') %>%
        mason::add_variables('yvars', c('linvHOMA', 'lISI', 'lIGIIR', 'lISSI2')) %>%
        mason::add_variables('xvars', 'VN') %>%
        mason::construct() %>%
        mason::scrub() %>%
        mason::polish_filter('Xterm$', 'term') %>%
        dplyr::summarise(p.value = mean(p.value)) %>%
        dplyr::mutate(p.value = format_p(p.value))

    change_outcomes <- list(n = sample_size, chg = change_over_time, p = pval)

    return(change_outcomes)
}
