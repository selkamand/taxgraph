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

# Converting Reports to Graphs --------------------------------------------

#' Convert a Kraken report to an annotated taxonomy graph
#'
#' Filters a taxonomy to the taxids present in a Kraken report and annotates
#' graph vertices with read counts (`reads_covered_by_clade`,
#' `reads_assigned_directly`).
#'
#' @param report A `data.table` with at least `taxid` and `reads_covered_by_clade`.
#' @param taxonomy An `igraph` taxonomy graph with vertex attribute `name` (taxid).
#' @param drop_missing Logical; if `TRUE`, restrict the taxonomy to taxids present
#'   in the report.
#'
#' @return An `igraph` object with vertex-level read count attributes.
#'
#' @export
report_to_graph <- function(report, taxonomy, drop_missing = TRUE){
  assertions::assert_names_include(report, "taxid", "reads_covered_by_clade")

  # Filter report for taxids with at least 1 supporting read
  taxids_to_keep <- report$taxid[report$reads_covered_by_clade > 0]
  taxids_to_keep <- as.character(taxids_to_keep)

  # Return filtered taxonomy (unless `drop_missing = FALSE`, in which case we just annotate the full taxonomy with report results)
  if(drop_missing){
    subgraph <- taxonomy_filter_for_taxids(taxonomy, report$taxid)
  }
  else
    subgraph <- taxonomy

  # Annotate with report counts

  subgraph_annotated <- taxonomy_annotate_with_report_counts(subgraph, report)

  # Return result
  return(subgraph_annotated)
}

taxonomy_annotate_with_report_counts <- function(taxonomy, report){
  report_subset <- report[,c("taxid", "reads_covered_by_clade", "reads_assigned_directly")]
  names(report_subset)[1] <- "name"

  # align rows to graph vertex order
  idx <- match(igraph::V(taxonomy)$name, report_subset$name)

  # Add Attributes
  igraph::V(taxonomy)$reads_covered_by_clade <- na_to_zero(report_subset$reads_covered_by_clade[idx])
  igraph::V(taxonomy)$reads_assigned_directly <- na_to_zero(report_subset$reads_assigned_directly[idx])

  return(taxonomy)
}


# Utils -------------------------------------------------------------------

na_to_zero <- function(vec){
  vec[is.na(vec)] <- 0
  return(vec)
}

