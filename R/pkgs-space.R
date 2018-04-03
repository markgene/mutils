# Drive Space Consumption of Installed R Packages
# Most of the code is borrowed from the blog post
# https://rud.is/b/2018/04/01/compute-visualize-drive-space-consumption-of-your-installed-r-packages/.


#' Total space consumption of installed R packages
#'
#' @return A character scalar
installed_pkgs_total_size <- function() {
  result <- installed.packages() %>%
    tibble::as_data_frame() %>%
    dplyr::mutate(pkg_dir = sprintf("%s/%s", LibPath, Package)) %>%
    dplyr::select(pkg_dir) %>%
    dplyr::mutate(pkg_dir_size = purrr::map_dbl(pkg_dir, ~{
      fs::dir_info(.x, all=TRUE, recursive=TRUE) %>%
        dplyr::summarise(tot_dir_size = sum(size)) %>%
        dplyr::pull(tot_dir_size)
    })) %>%
    dplyr::summarise(
      total_size_of_all_installed_packages=ggalt::Gb(sum(pkg_dir_size))
    ) %>%
    unlist()
  return(result)
}


#' Profile drive space consumption of installed R packages
#'
#' @return A \code{tbl_df} object has four columns: Package, Version,
#' pkg_dir, pkg_dir_size, which are package name, package version,
#' package installed directory, and the space the package consumes
#' in byte.
installed_pkgs_sizes <- function() {
  result <- installed.packages() %>%
    tibble::as_data_frame() %>%
    dplyr::mutate(pkg_dir = sprintf("%s/%s", LibPath, Package)) %>%
    dplyr::select(Package, Version, pkg_dir) %>%
    dplyr::mutate(pkg_dir_size = purrr::map_dbl(pkg_dir, ~{
      fs::dir_info(.x, all=TRUE, recursive=TRUE) %>%
        dplyr::summarise(tot_dir_size = sum(size)) %>%
        dplyr::pull(tot_dir_size)
    })) %>%
    dplyr::arrange(desc(pkg_dir_size))
  return(result)
}


#' Tree map of space consumption of installed R packages
#'
#' @return A \code{ggplot} object
installed_pkgs_space_treemap <- function() {
  installed.packages() %>%
    tibble::as_data_frame() %>%
    dplyr::mutate(pkg_dir = sprintf("%s/%s", LibPath, Package)) %>%
    dplyr::mutate(dir_info = purrr::map(pkg_dir, fs::dir_info, all=TRUE, recursive=TRUE)) %>%
    dplyr::mutate(dir_size = purrr::map_dbl(dir_info, ~sum(.x$size))) -> xdf

  dplyr::select(xdf, Package, dir_size) %>%
    dplyr::mutate(grp = "ROOT") %>%
    dplyr::add_row(grp = "ROOT", Package="ROOT", dir_size=0) %>%
    dplyr::select(grp, Package, dir_size) %>%
    dplyr::arrange(desc(dir_size)) -> gdf

  dplyr::select(gdf, -grp) %>%
    dplyr::mutate(lab = sprintf("%s\n(%s)", Package, ggalt::Mb(dir_size))) %>%
    dplyr::mutate(lab = ifelse(dir_size > 1500000, lab, "")) -> vdf

  g <- igraph::graph_from_data_frame(gdf, vertices=vdf)
  # dir_size <- igraph::get.vertex.attribute(g, name = "dir_size")

  p <- ggraph::ggraph(g, "treemap", weight="dir_size") +
    ggraph::geom_node_tile(fill="lightslategray", size=0.25) +
    ggplot2::geom_text(
      ggplot2::aes(x, y, label=lab, size=dir_size),
      color="#cccccc", lineheight=0.875
    ) +
    ggplot2::scale_x_reverse(expand=c(0,0)) +
    ggplot2::scale_y_continuous(expand=c(0,0)) +
    ggplot2::scale_size_continuous(trans="sqrt", range = c(0.5, 8)) +
    ggraph::theme_graph() +
    ggplot2::theme(legend.position="none")
  return(p)
}
