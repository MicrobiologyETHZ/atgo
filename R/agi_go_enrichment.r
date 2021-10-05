agi_go_enrichment <- function(agi_list, variants=TRUE, relationships="all", aspect="all", evidence="any", method="binom", p.adjust_method="BH"){
    # Check if method is valid
    if(!method%in%c("binom", "fisher", "chisq")){
        stop(paste(method, "is not a valid method. Supported methods are binom, fisher and chisq."))
    }

    # Find all associated go_ids for the inlist
    in_go_list <- lapply(agi_list, function(x) agi2go(x, variants=variants, relationships=relationships, aspect=aspect, evidence=evidence))
    in_go_ids <- unique(unlist(go_list))

    # Find all associated go_ids for the outlist
    out_list <- all_agi[!all_agi%in%agi_list]
    out_go_list <- lapply(out_list, function(x) agi2go(x, variants=variants, relationships=relationships, aspect=aspect, evidence=evidence))
    out_go_ids <- unique(unlist(out_go_list))

    # Test each go_id
    p_values <- unlist(sapply(go_ids, function(x) go_id_enrichment(x, in_go_list, out_go_list, method=method)))
    names(p_values) <- in_go_ids
    p_adj <- p.adjust(p_values, method=p.adjust_method)

    return(p_adj)
}
