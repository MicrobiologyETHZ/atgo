#' Function to 
#'
#' @param agi               A vector of AGI codes for the A. thaliana genes of interest
#' @param variants          A logical indicating whether or not isoform variants are included as part of their locus. Default: TRUE
#' @param relationships     A vector of GO relationships to consider, see http://www.ontobee.org/ontology/RO .  Default: all
#' @param aspect            A vector of GO aspects to consider. F: molecular function; C: cellular component; P: biological process. Default: all
#' @param evidence          A vector of GO evidence codes to consider, see http://geneontology.org/docs/guide-go-evidence-codes/ . Default: any
#' @details
#' 
#' @return A list of GO IDs.
#' @export
#' @author Chris Field <fieldc@@ethz.ch>
#' @examples
#' None
agi2go <- function(agi, variants=TRUE, relationships="all", aspects="all", evidence="any"){
    if(relationships=="all"){
        relationships <- unique(ath_goslim$relationship)
    }
    if(aspects=="all"){
        aspects <- unique(ath_goslim$aspect)
    }
    if(evidence=="any"){
        evidences <- unique(ath_goslim$evidence_code)
    }

    if(variants){
        go_ids <- ath_goslim[ath_goslim$locus%in%agi & ath_goslim$relationship%in%relationships & ath_goslim$aspect%in%aspects & ath_goslim$evidence_code%in%evidences,]$go_id
        return(unique(go_ids))
    }else{
        go_ids <- ath_goslim[ath_goslim$object%in%agi & ath_goslim$relationship%in%relationships & ath_goslim$aspect%in%aspects & ath_goslim$evidence_code%in%evidences,]$go_id
        return(unique(go_ids))
    }
}
