plot_pca <- function(techs, file){
  df_tr <- techs |> 
    filter(module == "treatment")
  
  df <- df_tr |> 
    select(id, vertical:inv_es_biohazard) |> 
    select(
      -c(camping_wastewater, offices_wastewater, rain_water, runoff_water), 
      -contains("_dry"),
      -starts_with("m2_pe"),
      -starts_with("hlr_")
    ) |> 
    tibble::as_tibble() |> 
    tibble::column_to_rownames("id")
  
  pca_res <- df |> 
    FactoMineR::PCA(graph = FALSE)
  
  ind <- pca_res$ind$coord[, 1:2] |> 
    as.data.frame() |> 
    tibble::rownames_to_column() |> 
    mutate(type = df_tr$type) |> 
    mutate(rowname = str_trunc(rowname, 40))
  
  var <- pca_res$var$coord[, 1:2] |> 
    as.data.frame() |> 
    tibble::rownames_to_column() 
  
  r <- min((max(ind[, "Dim.1"]) - min(ind[, "Dim.1"])/(max(var[, "Dim.1"]) - 
          min(var[, "Dim.1"]))), (max(ind[, "Dim.2"]) - min(ind[, "Dim.2"])/(max(var[, 
          "Dim.2"]) - min(var[, "Dim.2"]))))
  
  colors <- c('#fdbf6f','#1f78b4','#33a02c','#b2df8a','#e31a1c','#fb9a99','#a6cee3','#ff7f00','#000000','#6a3d9a')
  
  p <- var |> 
    mutate(
      Dim.1 = Dim.1 * r * 0.7,
      Dim.2 = Dim.2 * r * 0.7,
    ) |> 
    ggplot(aes(x = Dim.1, y = Dim.2, label = rowname)) +
    geom_point(alpha = 0.2) +
    ggrepel::geom_text_repel(alpha = 0.2) +
    geom_point(data = ind, mapping = aes(color = type)) +
    ggrepel::geom_text_repel(data = ind, mapping = aes(color = type), show.legend = FALSE) +
    # scale_color_brewer(palette = "Paired", direction = -1) +
    scale_color_manual(values = rev(colors)) +
    coord_cartesian(ylim = c(-5, 4)) +
    labs(
      x = paste0("Dimension 1 (", round(pca_res$eig[1, 2], 2), "%)"),
      y = paste0("Dimension 2 (", round(pca_res$eig[2, 2], 2), "%)"),
      color = "Type"
    ) +
    theme_custom()
  ggsave(file, p, width = 8, height = 6)
}
