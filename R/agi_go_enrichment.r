#' Function to look for GO term enrichment in a list of A. thaliana genes
#'
#' @param agi_list          A vector of AGI codes for A. thaliana genes of interest
#' @param variants          A logical indicating whether or not isoform variants are included as part of their locus. Default: TRUE
#' @param relationships     A vector of GO relationships to consider, see http://www.ontobee.org/ontology/RO .  Default: all
#' @param aspect            A vector of GO aspects to consider. F: molecular function; C: cellular component; P: biological process. Default: all
#' @param evidence          A vector of GO evidence codes to consider, see http://geneontology.org/docs/guide-go-evidence-codes/ . Default: any
#' @param method            The method to use for statistical testing; binom, fisher or chisq. Default: binom
#' @param p.adjust_method   The method for p-value adjustment, see p.adjust. Default: BH
#' @details
#' GO terms are found for all genes in the agi_list. For each term, associated genes are counted in the agi_list and in the rest of A. thaliana. A gene is considered associated if either it has a direct association according to ath_goslim or if it is associated with any of the offspring terms. A statistical test is then carried out to determine the likelihood of the term being over- or under-represented. P-values are then adjusted for multiple testing.
#' @return A labelled vector of adjusted p-values.
#' @export
#' @author Chris Field <fieldc@@ethz.ch>
#' @examples
#' None
agi_go_enrichment <- function(agi_list, variants=TRUE, relationships="all", aspects="all", evidence="any", method="binom", p.adjust_method="BH"){
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
    
    # Test each go_id
    cat(paste("Testing", length(in_go_ids), "GO ids\n"))
    p_values <- unlist(sapply(in_go_ids, function(x) go_id_enrichment(x, agi_list, variants=variants, relationships=relationships, evidence=evidence, method=method)))
    names(p_values) <- in_go_ids
    p_adj <- p.adjust(p_values, method=p.adjust_method)
    p_adj <- sort(p_adj)

    return(p_adj)
}
