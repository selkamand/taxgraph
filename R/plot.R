#' Plot a quick igraph tree
#'
#'
#' @param taxonomy taxonomy object created from [parse_taxonomy()] or
#' @param circular
#'
#' @returns
#' @export
#'
#' @examples
plot_igraph_tree <- function(taxonomy, circular = FALSE){
  plot(taxonomy, layout = igraph::layout_as_tree(graph = taxonomy, circular = circular))
}

plot_ggraph_tree <- function(taxonomy){

}
