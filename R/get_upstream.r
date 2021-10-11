#' @export
get_upstream <- function(go_id){
    # Figure out which ontology the id is from
    aspect <- suppressMessages(select(GO.db, go_id, "ONTOLOGY")$ONTOLOGY)

    # Find ancestors
    ancestor_ids <- eval(parse(text=paste("GO", aspect, "ANCESTOR[[\"", go_id, "\"]]", sep="")))

    return(c(go_id, ancestor_ids))
}
