# Renaming ----------------------------------------------------------------

renaming_outcomes <- function(x) {
    x %>%
        gsub('linvHOMA', 'log(1/HOMA-IR)', .) %>%
        gsub('lISI', 'log(ISI)', .) %>%
        gsub('lIGIIR', 'log(IGI/IR)', .) %>%
        gsub('lISSI2', 'log(ISSI-2)', .) %>%
        gsub('^invHOMA$', '1/HOMA-IR', .) %>%
        gsub('^ISSI2$', 'ISSI-2', .) %>%
        gsub('^IGIIR$', 'IGI/IR', .)
}

renaming_fats <- function(x) {
    x %>%
        gsub('.*(\\d\\d)(\\d)', '\\1:\\2', .) %>%
        gsub('n(\\d)$', 'n-\\1', .) %>%
        gsub('D(\\d\\d)$', 'D-\\1', .) %>%
        gsub('^pct_', '', .) %>%
        gsub('TotalTG', 'Total', .) %>%
        gsub('^TAG$', 'Clinical TAG', .)
}

renaming_fraction <- function(x) {
    x %>%
        gsub('ne', 'Non-esterified', .)
}
