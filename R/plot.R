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
