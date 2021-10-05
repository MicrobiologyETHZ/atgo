agi2go <- function(agi, variants=TRUE, relationships="all", aspect="all", evidence="any"){
    if(relationships=="all"){
        relationships <- unique(ath_goslim$relationship)
    }
    if(aspect=="all"){
        aspects <- unique(ath_goslim$aspect)
    }
    if(evidence=="any"){
        evidences <- unique(ath_goslim$evidence_code)
    }

    if(variants){
        go_ids <- ath_goslim[ath_goslim$locus==agi & ath_goslim$relationship%in%relationships & ath_goslim$aspect%in%aspects & ath_goslim$evidence_code%in%evidences,]$go_id
        return(unique(go_ids))
    }else{
        go_ids <- ath_goslim[ath_goslim$object==agi & ath_goslim$relationship%in%relationships & ath_goslim$aspect%in%aspects & ath_goslim$evidence_code%in%evidences,]$go_id
        return(unique(go_ids))
    }
}
