#' clusterDotPlot
#'
#' @param df Dataframe with two clustering columns
#' @param class_col Column name (string) for the reference classification (e.g., cell class)
#' @param cluster_col Column name (string) for the other clustering (e.g., output of algorithm)
#'
#' @return A ggplot2 heatmap of clustering overlap
clusterDotPlot <- function(df, class_col, cluster_col) {
    # Check required packages at start
    stopifnot(requireNamespace("ggplot2"), requireNamespace("reshape2"), requireNamespace("igraph"))

    # Drop NAs and "NA" strings in class_col
    df <- df[!is.na(df[[class_col]]) & df[[class_col]] != "NA", ]

    # Compute ARI (Adjusted Rand Index)
    ARI <- igraph::compare(df[[cluster_col]], df[[class_col]], method = "adjusted.rand")
    message(sprintf("ARI = %.2f", round(ARI, 2)))

    # Contingency table, convert to proportion by row
    tab <- table(df[[class_col]], df[[cluster_col]])
    prop_tab <- prop.table(tab, margin = 1) # Row-normalized

    # Hierarchical clustering for ordering
    h.row <- hclust(dist(prop_tab), method = "ward.D2")
    h.col <- hclust(dist(t(prop_tab)), method = "ward.D2")

    # Prepare for ggplot
    f <- reshape2::melt(prop_tab)
    f$Var1 <- factor(f$Var1, levels = rev(h.row$labels[h.row$order]))
    f$Var2 <- factor(f$Var2, levels = h.col$labels[h.col$order])

    # Plot
    ggplot2::ggplot(f, ggplot2::aes(x = Var2, y = Var1, color = value)) +
        ggplot2::geom_point(size = 3) +
        ggplot2::theme_bw() +
        ggplot2::scale_color_gradient(limits = c(0, 1), low = "white", high = "darkblue") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        ggplot2::xlab(cluster_col) +
        ggplot2::ylab(class_col)
}


#' get_data_path
#'
#' @param fname
#'
#' @returns
#' @export
#'
#' @examples
get_data_path <- function(fname) {
    local_path <- file.path("..", "inst", "extdata", fname)
    if (file.exists(local_path)) {
        return(local_path)
    }
    pkg_path <- system.file("extdata", fname, package = "NOMEPACCHETTO")
    if (nzchar(pkg_path)) {
        return(pkg_path)
    }
    stop("File not found: ", fname)
}

