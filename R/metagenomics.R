parse_krakenuniq_report_as_graph <- function(path_report, path_taxonomy){

}

#' Parse a KrakenUniq report
#'
#' Reads a KrakenUniq report and standardises key column names so output includes the following:
#' `taxid`, `fullname`, `reads_covered_by_clade`, `reads_assigned_directly`.
#'
#' @param path Path to a tabular KrakenUniq report file.
#'
#' @return A `data.table` with standardised column names.
#' @export
#'
#' @examples
#' path = system.file("reports/krakenuniq.report.tsv")
#' parse_krakenuniq_report(path)
parse_krakenuniq_report <- function(path){

  ## Parse report
  dt <- data.table::fread(file = path, header = TRUE)

  # Standardise column names
  data.table::setnames(
    dt,
    old = c("taxID", "taxName", "taxReads", "reads"),
    new = c("taxid", "fullname", "reads_assigned_directly", "reads_covered_by_clade"),
    skip_absent = TRUE
  )

  # Reorder cols so standardised cols come first
  first_cols <- c("taxid", "fullname", "reads_covered_by_clade", "reads_assigned_directly")
  data.table::setcolorder(dt, c(first_cols, setdiff(colnames(dt), first_cols)))

  # Return data.table
  dt[]
}

#' Parse a Kraken2 report
#'
#' Reads a Kraken2 report and standardises key column names so output includes the following:
#' `taxid`, `fullname`, `reads_covered_by_clade`, `reads_assigned_directly`.
#'
#' @param path Path to a tabular KrakenUniq report file.
#'
#' @return A `data.table` with standardised column names.
#' @export
#'
#' @examples
#' path = system.file("reports/kraken2.report.tsv")
#' parse_kraken_report(path)
parse_kraken_report <- function(path){

  # Read Report
  dt <- data.table::fread(file = path, header = FALSE, col.names = c("percent", "reads_covered_by_clade", "reads_assigned_directly", "rank", "taxid", "fullname"), strip.white = TRUE, quote = "")

  # Reorder cols so standardised cols come first
  first_cols <- c("taxid", "fullname", "reads_covered_by_clade", "reads_assigned_directly")
  data.table::setcolorder(dt, c(first_cols, setdiff(colnames(dt), first_cols)))

  # Return data.table
  dt[]
}
