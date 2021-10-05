go2agi <- function(go_id, variants=FALSE, relationships="all", evidence="any"){
    if(relationships=="all"){
        relationships <- unique(ath_goslim$relationship)
    }
    if(evidence=="any"){
        evidences <- unique(ath_goslim$evidence_code)
    }

    if(variants){
        agis <- ath_goslim[ath_goslim$go_id==go_id & ath_goslim$relationship%in%relationships & ath_goslim$aspect%in%aspects & ath_goslim$evidence_code%in%evidences,]$object
        return(unique(agis))
    }else{
        agis <- ath_goslim[ath_goslim$go_id==go_id & ath_goslim$relationship%in%relationships & ath_goslim$aspect%in%aspects & ath_goslim$evidence_code%in%evidences,]$locus
        return(unique(agis))
    }
}
