#' @export
go2agi <- function(go_id_list, variants=FALSE, relationships="all", evidence="any"){
    if(relationships=="all"){
        relationships <- unique(ath_goslim$relationship)
    }
    if(evidence=="any"){
        evidences <- unique(ath_goslim$evidence_code)
    }

    if(variants){
        agis <- ath_goslim[ath_goslim$go_id%in%go_id_list & ath_goslim$relationship%in%relationships & ath_goslim$evidence_code%in%evidences,]$object
        return(unique(agis))
    }else{
        agis <- ath_goslim[ath_goslim$go_id%in%go_id_list & ath_goslim$relationship%in%relationships & ath_goslim$evidence_code%in%evidences,]$locus
        return(unique(agis))
    }
}
