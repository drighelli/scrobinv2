#' ClusterHeatmap
#'
#' @param df Dataframe with two clustering columns
#' @param class_col Column name (string) for the reference classification (e.g., cell ontology)
#' @param cluster_col Column name (string) for the other clustering (e.g., output of algorithm)
#'
#' @return A ggplot2 heatmap of clustering overlap
#' @export
#'
#' @examples
#' df <- data.frame(cell_type = cell_ontology_class, cluster = as.factor(clusters))
#' ClusterHeatmap(df, class_col = "cell_type", cluster_col = "cluster")
#'
ClusterHeatmap <- function(df, class_col, cluster_col) {
    require(ggplot2)
    require(reshape2)
    suppressMessages(require(mclust, quietly = TRUE))

    class_vals <- df[[class_col]]
    cluster_vals <- df[[cluster_col]]

    message(paste0("ARI = ", round(mclust::adjustedRandIndex(cluster_vals, class_vals), digits = 2)))

    # Remove NA values
    keep_idx <- !is.na(class_vals) & class_vals != "NA"
    df <- df[keep_idx, ]
    class_vals <- df[[class_col]]
    cluster_vals <- df[[cluster_col]]

    # Build contingency matrix
    c <- unique(cluster_vals)
    ont <- unique(class_vals)
    M <- matrix(data = 0, nrow = length(ont), ncol = length(c))
    colnames(M) <- c
    rownames(M) <- ont

    for (i in ont) {
        tmp_idx <- class_vals == i
        tmp_table <- table(cluster_vals[tmp_idx])
        M[i, names(tmp_table)] <- tmp_table / sum(tmp_table)
    }

    # Clustering rows and columns
    h.row <- hclust(dist(M), method = "ward.D2")
    h.col <- hclust(dist(t(M)), method = "ward.D2")

    # Melt and plot
    f <- melt(M)
    f$Var1 <- factor(as.character(f$Var1), levels = rev(h.row$labels[h.row$order]))
    f$Var2 <- factor(as.character(f$Var2), levels = h.col$labels[h.col$order])

    ggplot(data = f, aes(x = Var2, y = Var1, color = value)) +
        geom_point(size = 3) +
        theme_bw() +
        scale_color_gradient(limits = c(0, 1), low = "white", high = "darkblue") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        xlab(cluster_col) +
        ylab(class_col)
}
