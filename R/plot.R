#' Plot igraph tree
#'
#' Plot a quick taxonomy tree using base R.
#'
#' @param taxonomy taxonomy object created from [parse_taxonomy()] or [report_to_graph()]
#' @param circular make tree radial (flag)
#'
#' @returns run for its side effects
#' @export
#'
#' @examples
#' path_to_taxdb = system.file(package="taxgraph", "taxonomies/taxDB")
#' taxonomy = parse_taxonomy(path_to_taxdb, type = "taxdb")
#' plot_igraph_tree(taxonomy)
plot_igraph_tree <- function(taxonomy, circular = FALSE){
  plot(taxonomy, layout = igraph::layout_as_tree(graph = taxonomy, circular = circular))
}

plot_ggraph_tree <- function(taxonomy){

}

#' Plot a sunburst chart from a taxonomy graph
#'
#' @param taxonomy An igraph object with a `reads_covered_by_clade` vertex attribute, e.g. produced by [report_to_graph()].
#' @param force Logical; set to TRUE to allow plotting large graphs.
#'
#' @return A plotly sunburst plot.
#' @export
#'
#' @examples
#' path_to_taxdb = system.file(package="taxgraph", "taxonomies/taxDB")
#' path_to_report = system.file(package= "taxgraph", "reports/krakenuniq.report.tsv")
#'
#' taxonomy = parse_taxonomy(path_to_taxdb, type = "taxdb")
#' report_dataframe = parse_krakenuniq_report(path_to_report)
#'
#' report_graph = report_to_graph(report_dataframe, taxonomy)
#' plot_sunburst_plotly(report_graph)
plot_sunburst_plotly <- function(taxonomy, force = FALSE){

  # assert size parameter is valid
  size_based_on = "reads_covered_by_clade"
  assertions::assert_one_of(size_based_on, y = igraph::vertex_attr_names(taxonomy), msg = paste0("Missing required annotation: [", size_based_on,"]"))

  # Warn if plot is going to take ages to build
  MAX_NODES_BEFORE_WARNING = 200
  nodes_in_network <- igraph::gsize(taxonomy)
  if(nodes_in_network > MAX_NODES_BEFORE_WARNING){
    stop(
      "Making sunburst plots with over ", MAX_NODES_BEFORE_WARNING, " nodes can take a while.",
      "Your graph has ",nodes_in_network, ". If you're sure you want to proceed, set `force = TRUE`"
    )
  }

  # Convert igraph to annotated long data.frame
  df <- igraph::as_long_data_frame(taxonomy)

  plotly::plot_ly(
    labels = df$to_fullname,
    parents = df$from_fullname,
    values = df[[paste0("to_", size_based_on)]],
    type = 'sunburst',
    branchvalues = 'total'
  )
}


taxonomy_to_long_data_frame <- function(taxonomy){
  igraph::as_long_data_frame(taxonomy)
}

# taxonomy_to_annotated_edge_data_frame <- function(taxonomy){
#
#   # 2 column df with 'from' and 'to'
#   edges = igraph::as_data_frame(taxonomy, what = "edges")
#
#   vertices = igraph::as_data_frame(taxonomy, what = "vertices")
#
#   data.table::
#
# }

