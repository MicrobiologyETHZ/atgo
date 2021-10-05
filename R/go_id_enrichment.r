go_id_enrichment <- function(go_id, in_go_list, out_go_list, method="binom"){
    # Check if method is valid
    if(!method%in%c("binom", "fisher", "chisq")){
        stop(paste(method, "is not a valid method. Supported methods are binom, fisher and chisq."))
    }

    # Count in and out lists for go_id
    go_id_count <- go_id_count(go_id, in_go_list, out_go_list)

    # Perform statistical test
    if(method=="binom"){
        pout <- out_true/(out_true + out_false)
        p <- binom.test(in_true, in_true + out_true, pout)$p.value
    }else if(method=="fisher"){
        m <- matrix(c(in_true, in_false, out_true, out_false))
        p <- fisher.test(m)$p.value
    }else if(method=="chisq"){
        m <- matrix(c(in_true, in_false, out_true, out_false))
        p <- chisq.test(m)$p.value
    }
    return(p)
}
