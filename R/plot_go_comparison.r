#' Function to plot a heatmap of a comparison of GO term enrichment between conditions
#'
#' @param p_value_table     A data frame of p-values created by compare_go_enrichment()
#' @param ...               Other arguments passed to heatmap()
#' @details
#' 
#' @return None.
#' @export
#' @author Chris Field <fieldc@@ethz.ch>
#' @examples
#' None
plot_go_comparison <- function(p_value_table, ...){
    # Deal with zeroes
    pseudop <- min(p_value_table[p_value_table>0])/10
    toplot <- log10(p_value_table+pseudop)

    # Fetch labels
    goterms <- suppressMessages(select(GO.db, rownames(p_value_table), "TERM"))
    goterms <- goterms$TERM

    heatmap(toplot, labRow=goterms, ...)
}
