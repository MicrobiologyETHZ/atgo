#' Function to compare GO term enrichment in multiple lists of A. thaliana genes
#'
#' @param agi_lists         A list of named vectors, each containing AGI codes of interest, for instance differentially expressed genes in different conditions.
#' @param variants          A logical indicating whether or not isoform variants are included as part of their locus. Default: TRUE
#' @param relationships     A vector of GO relationships to consider, see http://www.ontobee.org/ontology/RO .  Default: all
#' @param aspect            A vector of GO aspects to consider. F: molecular function; C: cellular component; P: biological process. Default: all
#' @param evidence          A vector of GO evidence codes to consider, see http://geneontology.org/docs/guide-go-evidence-codes/ . Default: any
#' @param method            The method to use for statistical testing; binom, fisher or chisq. Default: binom
#' @param p.adjust_method   The method for p-value adjustment, see p.adjust. Default: BH
#' @param cutoff            If given, only results with p-value less than or equal to this parameter will be reported. Default: 0.05
#' @param test_upstream     A logical to determine if GO terms upstream of those given in ATH_GOSLIM will also be tested for significance. Default: TRUE
#' @details
#' GO terms are found for all genes in the agi_list. For each term, associated genes are counted in the agi_list and in the rest of A. thaliana. A gene is considered associated if either it has a direct association according to ath_goslim or if it is associated with any of the offspring terms. A statistical test is then carried out to determine the likelihood of the term being over- or under-represented. P-values are then adjusted for multiple testing.
#' @return A labelled vector of adjusted p-values.
#' @export
#' @author Chris Field <fieldc@@ethz.ch>
#' @examples
#' None
compare_go_enrichment <- function(agi_lists, variants=TRUE, relationships="all", aspects="all", evidence="any", method="binom", p.adjust_method="BH", cutoff=0.05, test_upstream=TRUE){
    # Build a master list of go_ids
    all_go_ids <- unique(unlist(lapply(agi_lists, function(x) agi2go(x, variants=variants, relationships=relationships, aspects=aspects, evidence=evidence))))
    if(sum(!all_go_ids %in% keys(GO.db)) > 0){
        warning("Some GO IDs associated with these AGI are not listed in GO.db for reasons beyond comprehension. They will be ignored.")
    }
    all_go_ids <- all_go_ids[all_go_ids %in% keys(GO.db)]
    
    # If test_upstream then add all upstream go_ids for testing, but remove "all"
    if(test_upstream){
        upstream_ids <- unlist(lapply(all_go_ids, function(x) get_upstream(x)))
        all_go_ids <- unique(c(all_go_ids, upstream_ids))
        all_go_ids <- all_go_ids[all_go_ids!="all"]
    }

    # Test each go_id with each agi_list
    cat(paste("Testing", length(all_go_ids), "GO ids in", length(agi_lists), "conditions\n"))
    results <- lapply(agi_lists, function(x) lapply(all_go_ids, function(y) go_id_enrichment(y, x, variants=variants, relationships=relationships, evidence=evidence, method=method, report_counts=F)))
    p_value_table <- do.call(cbind, lapply(results, function(x) do.call(rbind, x)))
    rownames(p_value_table) <- all_go_ids
    colnames(p_value_table) <- names(agi_lists)

    # Apply the cutoff
    p_value_table <- p_value_table[apply(p_value_table, 1, function(x) any(x<=cutoff)),]
    
    return(p_value_table)    
}
