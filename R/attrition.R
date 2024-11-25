fetchAttritionCounts <- function(andromeda, table = "treatmentHistory") {
  n <- andromeda[[table]] %>%
    dplyr::group_by(.data$personId) %>% 
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::pull()
  list(
    nSubjects = length(n),
    nRecords = sum(n)
  )
}

initAttrition <- function(andromeda) {
  andromeda$attrition <- data.frame(
    number_records = numeric(0),
    number_subject = numeric(0),
    reason_id = numeric(0),
    reason = character(0)
  )
}

appendAttrition <- function(toAdd, andromeda) {
  if (is.null(andromeda$attrition)) initAttrition(andromeda)

  andromeda$attrition <- dplyr::rows_append(
    x = andromeda$attrition,
    y = toAdd,
    copy = TRUE
  )

  message(sprintf("%s: %s records; %subjects", toAdd$reason, toAdd$nRecords, toAdd$nSubjects))
}
