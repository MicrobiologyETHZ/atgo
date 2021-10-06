# ATGO

An R package to enable GO term enrichment testing in A. thaliana.

Various support functions are included but the main function is:

```
agi_go_enrichment             package:atgo             R Documentation

Function to look for GO term enrichment in a list of A. thaliana genes

Description:

     Function to look for GO term enrichment in a list of A. thaliana
     genes

Usage:

     agi_go_enrichment(
       agi_list,
       variants = TRUE,
       relationships = "all",
       aspects = "all",
       evidence = "any",
       method = "binom",
       p.adjust_method = "BH"
     )

Arguments:

agi_list: A vector of AGI codes for A. thaliana genes of interest

variants: A logical indicating whether or not isoform variants are
          included as part of their locus. Default: TRUE

relationships: A vector of GO relationships to consider, see
          http://www.ontobee.org/ontology/RO .  Default: all

evidence: A vector of GO evidence codes to consider, see
          http://geneontology.org/docs/guide-go-evidence-codes/ .
          Default: any

  method: The method to use for statistical testing; binom, fisher or
          chisq. Default: binom

p.adjust_method: The method for p-value adjustment, see p.adjust.
          Default: BH

  aspect: A vector of GO aspects to consider. F: molecular function; C:
          cellular component; P: biological process. Default: all

Details:

     GO terms are found for all genes in the agi_list. For each term,
     associated genes are counted in the agi_list and in the rest of A.
     thaliana. A gene is considered associated if either it has a
     direct association according to ath_goslim or if it is associated
     with any of the offspring terms. A statistical test is then
     carried out to determine the likelihood of the term being over- or
     under-represented. P-values are then adjusted for multiple
     testing.

Value:

     A labelled vector of adjusted p-values.
```
