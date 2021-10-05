get_downstream <- function(go_id){
    # Figure out which ontology the id is from
    aspect <- select(GO.db, go_id, "ONTOLOGY")$ONTOLOGY

    # Find offspring
    offspring_ids <- eval(parse(text=paste("GO", aspect, "OFFSPRING[[\"", go_id, "\"]]", sep="")))

    return(c(go_id, ancestor_ids))
}
