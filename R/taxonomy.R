
#' Parse taxonomy
#'
#' @param path path to a taxonomy file
#' @param type type of taxonomy file described by path (taxDB, ktaxonomy, ncbi)
#'
#' @returns Returns an igraph structure with the following properties
#' - Directed,
#' - Up to 1 parent per node, multiple children per node allowed)
#' - Nodes with no parent allowed (we do not force a single 'root' node)
#' - `name` node attribute represents unique character identifier (usually taxid)
#' - `fullname` node attribute represents scientific name
#' - `rank` node attribute represents level of taxid in tree (e.g. family/genus/species). Exact terms may change depending on taxonomy type.
#'
#' @export
#'
#' @examples
#' path_to_taxdb = system.file(package="taxgraph", "taxonomies/taxDB")
#' parse_taxonomy(path_to_taxdb, type = "taxdb")
#'
parse_taxonomy <- function(path, type = c("taxdb", "ktaxonomy", "ncbi")){
  type <- rlang::arg_match(type)

  if(type == "taxdb"){
    return(parse_krakenuniq_taxonomy(path))
  }

  stop("No parser for taxonomy type: ", type, " has been implemented yet. Please create a github issue with this error message")
}

parse_taxdb_as_dataframe <- function(path){
  assertions::assert_file_exists(path)
  # read.csv(file = path, header = FALSE, col.names =  c("taxid", "parent_taxid", "fullname", "rank"), sep = "\t", quote = "")
  data.table::fread(file = path, header = FALSE, col.names = c("taxid", "parent_taxid", "fullname", "rank"), quote = "", sep = "\t")
}


parse_krakenuniq_taxonomy <- function(path){

  # Get taxonomy data.frame
  taxonomy_table <- parse_taxdb_as_dataframe(path)

  # Drop any cases where parent taxonomy = taxonomy ID since that technically creates a loop
  edges <- taxonomy_table[taxonomy_table$taxid != taxonomy_table$parent_taxid, c("parent_taxid", "taxid"), drop=FALSE]
  colnames(edges) <- c("from", "to")
  nodes <- taxonomy_table[,c("taxid", "rank", "fullname")]
  colnames(nodes)[1] <- "name"

  # Create Igraph Structure
  igraph::graph_from_data_frame(
    d = edges,
    vertices = nodes,
    directed = TRUE
  )
}


# Taxonomy Operations -----------------------------------------------------

#' Filter for specific taxids
#'
#' Filters a taxonomy for specific taxids while preserving connectivity where possible.
#' For example if you have graph \strong{A -> B -> C -> D} and filter for \strong{B} & \strong{D}
#' all other vertices are removed by vertex suppression, producing the graph:
#' \strong{B -> D}
#'
#' @param taxonomy taxonomy (igraph) object produced by [parse_taxonomy()]
#' @param taxids a vector of taxids to filter for.
#'
#' @returns A subgraph (igraph) of taxonomy including only specified `taxids`.
#' @export
#'
#' @examples
#' path_to_taxdb = system.file(package="taxgraph", "taxonomies/taxDB")
#' taxonomy <- parse_taxonomy(path_to_taxdb, type = "taxdb")
#' taxonomy_filter_for_taxids(taxonomy, c("10376"))
taxonomy_filter_for_taxids <- function(taxonomy, taxids){

  # Standardise taxid format
  taxids <- unique(as.character(taxids))

  ## Create Subgraph including only specified taxids
  g_filtered = igraph::subgraph(taxonomy, igraph::V(taxonomy)[[name %in% taxids]])

  ## Add reconnect nodes using information from the original taxonomy
  g_filtered_reconnected <- taxonomy_complete_connections(g_filtered, taxonomy)

  return(g_filtered_reconnected)
}

#' Filter for specific ranks
#'
#' Filters a taxonomy for nodes with rank in `ranks`, while preserving
#' connectivity where possible.
#'
#' @param taxonomy taxonomy (igraph) object produced by [parse_taxonomy()]
#' @param ranks a vector of ranks to filter for.
#'
#' @returns A subgraph (igraph) of taxonomy including only specified `ranks`.
#' @export
#'
#' @examples
#' path_to_taxdb = system.file(package="taxgraph", "taxonomies/taxDB")
#' taxonomy = parse_taxonomy(path_to_taxdb, type = "taxdb")
#' taxonomy_filter_for_ranks(taxonomy, ranks = c("family", "genus", "species"))
taxonomy_filter_for_ranks <- function(taxonomy, ranks){

  # Identify Taxids to keep
  taxids_to_keep <- igraph::V(taxonomy)[rank %in% ranks]$name

  # Filter for them (keeping lineage relationships intact)
  taxonomy_filter_for_taxids(taxonomy, taxids_to_keep)
}

#' Delete a taxid and all descendants
#'
#' Deletes a taxid and all of its children from a taxonomy.
#'
#' @param taxonomy taxonomy (igraph) object produced by [parse_taxonomy()]
#' @param taxid a single taxid to delete.
#'
#' @returns A taxonomy (igraph) with `taxid` and all descendants removed.
#' @export
#'
#' @examples
#' path_to_taxdb = system.file(package="taxgraph", "taxonomies/taxDB")
#' taxonomy = parse_taxonomy(path_to_taxdb, type = "taxdb")
#' taxonomy_delete_taxid_and_children(taxonomy, taxid = "10376")
taxonomy_delete_taxid_and_children <- function(taxonomy, taxid){

  # Assertions
  assertions::assert_length(taxid, length = 1)

  # Ensure taxid is character (so we match on name not vertex id)
  taxid <- as.character(taxid)

  # If taxid is not in taxonomy, return taxonomy as-is
  if(taxonomy_includes_taxids(taxid, taxonomy = taxonomy)){
    return(taxonomy)
  }

  # Find all children of taxid
  lineage_to_delete <- igraph::subcomponent(taxonomy, v = taxid, mode = "out")

  # Delete the vertices and return graph
  igraph::delete_vertices(taxonomy, v = lineage_to_delete)
}

#' Filter for a taxid and its descendants
#'
#' Returns a subgraph containing a taxid and all of its descendants.
#'
#' @param taxonomy taxonomy (igraph) object produced by [parse_taxonomy()]
#' @param taxid a single taxid to retain along with its descendants.
#'
#' @returns A subgraph (igraph) containing `taxid` and all descendant nodes.
#' @export
#'
#' @examples
#' path_to_taxdb = system.file(package="taxgraph", "taxonomies/taxDB")
#' taxonomy = parse_taxonomy(path_to_taxdb, type = "taxdb")
#' taxonomy_filter_for_descendants(taxonomy, taxid = "10376")
taxonomy_filter_for_descendants <- function(taxonomy, taxid){

  # Assertions
  assertions::assert_length(taxid, length = 1)

  # Ensure taxid is character (so we match on name not vertex id)
  taxid <- as.character(taxid)

  # If taxid is not in taxonomy - Return emtpy taxonomy (TODO: FIGURE OUT HOW TO DO THIS)

  # Find all children of taxid
  lineage_to_keep <- igraph::subcomponent(taxonomy, v = taxid, mode = "out")

  # Create a subgraph on only a specific lineage.
  igraph::subgraph(taxonomy, vids = as.numeric(lineage_to_keep))
}


#' Remove taxids with low read support
#'
#' Filters a taxonomy to retain only nodes with `reads_covered_by_clade`
#' greater than or equal to `min_reads_covered_by_clade`, while preserving
#' connectivity where possible.
#'
#' @param taxonomy taxonomy (igraph) object produced by [parse_taxonomy()]
#'   and annotated with `reads_covered_by_clade`.
#' @param min_reads_covered_by_clade minimum reads required to retain a taxid.
#'
#' @returns A subgraph (igraph) of taxonomy including only supported taxids.
#' @export
#'
#' @examples
#' path_to_taxdb = system.file(package="taxgraph", "taxonomies/taxDB")
#' taxonomy = parse_taxonomy(path_to_taxdb, type = "taxdb")
#' report_dataframe <- parse_krakenuniq_report(
#'   system.file(package = "taxgraph", "reports/krakenuniq.report.tsv")
#' )
#' report_graph = report_to_graph(report_dataframe, taxonomy, drop_missing = FALSE)
#' # assuming taxonomy has been annotated with read counts
#' taxonomy_remove_taxids_with_low_read_support(report_graph, min_reads_covered_by_clade = 10)
taxonomy_remove_taxids_with_low_read_support <- function(taxonomy, min_reads_covered_by_clade = 1){

  # Grab vertex annotations
  vertex_attributes <- igraph::vertex_attr(taxonomy)

  # Check taxonomy is annotated with read support info
  assertions::assert_names_include(
    vertex_attributes,
    names = "reads_covered_by_clade",
    msg = "Cannot find `reads_covered_by_clade` vertex attribute in taxonomy. Please ensure taxonomy has been generated from metagenomics report or if from parsed taxonomy has been annotated with reads_covered_by_clade"
  )

  # Identify taxids to keep
  taxids_to_keep = vertex_attributes$name[vertex_attributes$reads_covered_by_clade >= min_reads_covered_by_clade]

  # Create subgraph
  taxonomy_filter_for_taxids(taxonomy = taxonomy, taxids = taxids_to_keep)
}

# Helpers -----------------------------------------------------------------
taxonomy_includes_taxids <- function(taxids, taxonomy){
  all(taxids %in% igraph::V(taxonomy)$name)
}

is_valid_attribute <- function(taxonomy, attribute_name){
  attrs <- igraph::vertex_attr_names(taxonomy)
}

# For all nodes with no parent in g_incomplete,
# find whether an ancestral node is present in g_incomplete based on g_complete
taxonomy_complete_connections <- function(g_incomplete, g_complete){

  # Grab all taxids in subgraph
  taxids_in_subgraph = igraph::V(g_incomplete)$name

  edges_to_add <- c()

  ## Iterate through each node in the subgraph (g_incomplete)
  for (vertex_id in igraph::V(g_incomplete)){
    taxid = igraph::V(g_incomplete)[vertex_id]$name

    # Identify the parent node in subgraph (NA if no parent)
    parent = igraph_get_parent_taxid(g_incomplete, vertex_id)

    # If no parents in subgraph, look in the complete graph
    if(is.na(parent)){

      # Find whether any taxids upstream of our taxid-of-interest in complete graph are also in
      # Our subgraph - if so grab the closest parent
      parent_taxid_from_g_complete <- search_graph_for_parent_taxid(g_complete, taxid, taxids_in_subgraph)

      # If parent still couldn't be found, do nothing (maybe replace by adding a link to no-parent
      if(is.na(parent_taxid_from_g_complete)) next

      # Otherwise grab parent vertex index from parent in subgraph
      parent_id <- as.numeric(igraph::V(g_incomplete)[name == parent_taxid_from_g_complete])

      # Expand out vector of edges to add
      edges_to_add <- c(edges_to_add, c(parent_id, vertex_id))
    }

    # if parent is in subgraph, we don't need to do anything
  }

  # Add the missing edges to the subgraph
  igraph::add_edges(graph = g_incomplete, edges = edges_to_add)
}


## Find vertex of parent
igraph_get_parent_index <- function(graph, v){
  i <- as.numeric(igraph::neighbors(graph = graph, v = as.numeric(v), mode = "in"))
  if(length(i) == 0) return(NaN)
  else return(i)
}
igraph_get_parent_taxid <- function(graph, vertex_id){
  i <- igraph::neighbors(graph = graph, v = vertex_id, mode = "in")
  if(length(i) == 0) return(NA_character_)
  else return(i$name)
}

search_graph_for_parent_taxid <- function(g_complete, child_taxid, valid_taxids){
  # Fetch lineage of child_taxid
  lineage_v <- igraph::subcomponent(g_complete, v=igraph::V(g_complete)$name == child_taxid, mode = "in")

  # Create subgraph of just this lineage
  g_lineage <- igraph::subgraph(g_complete, lineage_v)

  # Compute distances
  d_to_child <- igraph::distances(graph = g_lineage, v = child_taxid)[1, ,drop=TRUE]


  # Sort
  d_to_child <- sort(d_to_child, decreasing = FALSE)

  # Drop child_taxid column (will = 0)
  d_to_child <- d_to_child[names(d_to_child) != child_taxid]

  # If ANY of the lineage are valid taxids
  # (usually all those in incomplete subgraph defined in calling scope )

  if(any(names(d_to_child) %in% valid_taxids)){
    closest_valid_parent = names(d_to_child)[names(d_to_child) %in% valid_taxids][1]
    return(closest_valid_parent)
  }
  else{
    return(NA_character_)
  }
}

