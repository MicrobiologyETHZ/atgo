#' Function to look for GO term enrichment in a list of A. thaliana genes
#'
#' @param agi_list          A vector of AGI codes for A. thaliana genes of interest
#' @param variants          A logical indicating whether or not isoform variants are included as part of their locus. Default: TRUE
#' @param relationships     A vector of GO relationships to consider, see http://www.ontobee.org/ontology/RO .  Default: all
#' @param aspect            A vector of GO aspects to consider. F: molecular function; C: cellular component; P: biological process. Default: all
#' @param evidence          A vector of GO evidence codes to consider, see http://geneontology.org/docs/guide-go-evidence-codes/ . Default: any
#' @param method            The method to use for statistical testing; binom, fisher or chisq. Default: binom
#' @param p.adjust_method   The method for p-value adjustment, see p.adjust. Default: BH
#' @param cutoff            If given, only results with p-value less than or equal to this parameter will be reported. Default: report all results
#' @param test_upstream     A logical to determine if GO terms upstream of those given in ATH_GOSLIM will also be tested for significance. Default: TRUE
#' @param report_counts     A logical, if set the results table will include counts of in- and out-group genes for each GO term. Default: FALSE
#' @details
#' GO terms are found for all genes in the agi_list. For each term, associated genes are counted in the agi_list and in the rest of A. thaliana. A gene is considered associated if either it has a direct association according to ath_goslim or if it is associated with any of the offspring terms. A statistical test is then carried out to determine the likelihood of the term being over- or under-represented. P-values are then adjusted for multiple testing.
#' @return A labelled vector of adjusted p-values.
#' @export
#' @author Chris Field <fieldc@@ethz.ch>
#' @examples
#' None
agi_go_enrichment <- function(agi_list, variants=TRUE, relationships="all", aspects="all", evidence="any", method="binom", p.adjust_method="BH", cutoff=1, test_upstream=TRUE, report_counts=FALSE){
    # Check if method is valid
    if(!method%in%c("binom", "fisher", "chisq")){
        stop(paste(method, "is not a valid method. Supported methods are binom, fisher and chisq."))
    }

    # Find all associated go_ids for the inlist
    in_go_ids <- agi2go(agi_list, variants=variants, relationships=relationships, aspects=aspects, evidence=evidence)

    # In utter madness, check to see if they are valid keys in GO.db
    if(sum(!in_go_ids%in%keys(GO.db))>0){
        warning("Some GO IDs associated with these AGI are not listed in GO.db for reasons beyond comprehension. They will be ignored.")
    }
    in_go_ids <- in_go_ids[in_go_ids%in%keys(GO.db)]

    # If test_upstream then add all upstream go_ids for testing, but remove "all"
    if(test_upstream){
        upstream_ids <- unlist(lapply(in_go_ids, function(x) get_upstream(x)))
        in_go_ids <- unique(c(in_go_ids, upstream_ids))
        in_go_ids <- in_go_ids[in_go_ids!="all"]
    }

    # Fetch readable terms
    in_terms <- suppressMessages(select(GO.db, in_go_ids, "TERM"))
    in_terms <- in_terms$TERM

    # Test each go_id
    cat(paste("Testing", length(in_go_ids), "GO ids\n"))
    results <- lapply(in_go_ids, function(x) go_id_enrichment(x, agi_list, variants=variants, relationships=relationships, evidence=evidence, method=method, report_counts=report_counts))
    results <- do.call(rbind, results)
    colnames(results)[1] <- "p.value"

    output_tab <- data.frame(go_id=in_go_ids, term=in_terms)
    output_tab <- cbind(output_tab, results)
    
    output_tab <- output_tab[order(output_tab$p.value),]

    # Reduce table to only significant hits if cutoff is set
    output_tab <- output_tab[output_tab$p.value<=cutoff,]

    return(output_tab)
}
