#' @export
go_id_count <- function(go_id, in_list, variants=TRUE, relationships="all", evidence="any"){
    # The out_list is everything not in the in_list
    out_list <- all_agi[!all_agi %in% in_list]

    # For each go_id to test determine all children - if a gene has a child go_id then it inherits the parent and should count towards the positive count
    downstream_ids <- get_downstream(go_id)

    # Get a list of AGI associated with the go_id
    true_list <- go2agi(downstream_ids, variants=variants, relationships=relationships, evidence=evidence)
    
    # Count the different categories
    in_count <- in_list%in%true_list
    out_count <- out_list%in%true_list

    return(list(in_true=sum(in_count), in_false=sum(!in_count), out_true=sum(out_count), out_false=sum(!out_count)))
}
