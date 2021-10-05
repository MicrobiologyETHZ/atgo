go_id_count <- function(go_id, in_go_list, out_go_list){
        # For each go_id to test determine all children - if a gene has a child go_id then it inherits the parent and should count towards the positive count
        downstream_ids <- get_downstream(go_id)

    # If an agi has the go_id or any of its offspring it counts positive
    in_count <- lapply(in_go_list, function(x) any(x%in%downstream_ids))
        out_count <- lapply(out_go_list, function(x) any(x%in%downstream_ids))

        return(list(in_true=sum(in_count), in_false=sum(!in_count), out_true=sum(out_count), out_false=sum(!out_count)))
}
