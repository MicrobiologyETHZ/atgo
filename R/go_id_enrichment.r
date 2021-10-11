#' @export
go_id_enrichment <- function(go_id, in_list, variants=TRUE, relationships="all", evidence="any", method="binom", report_counts=FALSE){
    # Check if method is valid
    if(!method%in%c("binom", "fisher", "chisq")){
        stop(paste(method, "is not a valid method. Supported methods are binom, fisher and chisq."))
    }

    # Count in and out lists for go_id
    count <- go_id_count(go_id, in_list, variants=variants, relationships=relationships, evidence=evidence)

    # Perform statistical test
    if(method=="binom"){
        pout <- count$out_true/(count$out_true + count$out_false)
        p <- binom.test(count$in_true, count$in_true + count$in_false, pout)$p.value
    }else if(method=="fisher"){
        m <- matrix(c(count$in_true, count$in_false, count$out_true, count$out_false), nrow=2)
        p <- fisher.test(m)$p.value
    }else if(method=="chisq"){
        m <- matrix(c(count$in_true, count$in_false, count$out_true, count$out_false), nrow=2)
        p <- chisq.test(m)$p.value
    }

    if(report_counts){
        result <- c(p, count$in_true, count$in_false, count$out_true, count$out_false)
        names(result) <- c("p.value", "in_group_true", "in_group_false", "out_group_true", "out_group_false")
        return(result)
    }else{
        return(p)
    }
}
