# Enrichment analysis


#' Over-representation approach, MSigDB
#'
#' @title A wrapper of over-representation approach, MSigDB
#' @param gene a vector of gene symbol.
#' @param species an one-element character vector of species. Default to \code{"Homo sapiens"}.
#' @param gs_cat an one-element character vector of gene set category. Default to Hallmark gene sets.
#' @param gs_subcat an one-element character vector of gene set sub-category. Default to \code{NULL}.
#' @param pvalueCutoff adjusted pvalue cutoff on enrichment tests to report
#' @param pAdjustMethod  one of "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
#' @param universe background genes. If missing, the all genes listed in the database (eg TERM2GENE table) will be used as background.
#' @param minGSSize minimal size of genes annotated for testing
#' @param maxGSSize maximal size of genes annotated for testing
#' @param qvalueCutoff qvalue cutoff on enrichment tests to report as significant.  Tests must pass i) \code{pvalueCutoff} on unadjusted pvalues, ii) \code{pvalueCutoff} on adjusted pvalues and iii) \code{qvalueCutoff} on qvalues to be reported.
#' @return A \code{enrichResult} instance
#' @author Mark J Chen
OR_msigdb <- function(gene,
           species = "Homo sapiens",
           gs_cat = "H",
           gs_subcat = NULL,
           universe = NULL,
           pvalueCutoff = 1,
           pAdjustMethod = "BH",
           minGSSize = 5,
           maxGSSize = 10000,
           qvalueCutoff = 1) {
    db.df <-
      msigdbr(species = species,
              category = gs_cat,
              subcategory = gs_subcat)
    gs_info.df <- db.df %>%
      dplyr::select(gs_cat, gs_subcat, gs_id, gs_name, gs_description) %>%
      dplyr::distinct()
    t2g.df <- dplyr::select(db.df, gs_id, gene_symbol)
    t2n.df <- db.df %>%
      dplyr::select(gs_id, gs_name) %>%
      dplyr::distinct()
    # Test
    # sample(t2g.df$gene_symbol, 100) %>% unique()
    or.res <-
      enricher(
        gene = gene,
        TERM2GENE = t2g.df,
        TERM2NAME = t2n.df,
        universe = universe,
        pAdjustMethod = pAdjustMethod,
        pvalueCutoff = pvalueCutoff,
        qvalueCutoff = qvalueCutoff,
        minGSSize = minGSSize,
        maxGSSize = maxGSSize
      )
    or.df <- as.data.frame(or.res) %>%
      dplyr::left_join(gs_info.df, by = c("ID" = "gs_id", "Description" = "gs_name"))
    list(enrichResult = or.res, df = or.df)
  }


#' Over-representation approach, MSigDB, mouse wrapper 1
#'
#' @title A wrapper of over-representation approach, MSigDB, mouse
#' @param gene a vector of gene symbol.
#' @oaram excel_file an one-element character vector of output Excel file path
#' @param species an one-element character vector of species. Default to \code{"Mus musculus"}.
#' @param pvalueCutoff adjusted pvalue cutoff on enrichment tests to report
#' @param pAdjustMethod  one of "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
#' @param universe background genes. If missing, the all genes listed in the database (eg TERM2GENE table) will be used as background.
#' @param minGSSize minimal size of genes annotated for testing
#' @param maxGSSize maximal size of genes annotated for testing
#' @param qvalueCutoff qvalue cutoff on enrichment tests to report as significant.  Tests must pass i) \code{pvalueCutoff} on unadjusted pvalues, ii) \code{pvalueCutoff} on adjusted pvalues and iii) \code{qvalueCutoff} on qvalues to be reported.
#' @return A \code{enrichResult} instance
#' @author Mark J Chen
#' @export
OR_msigdb.mouse1 <- function(gene,
                             excel_file,
                             species = "Mus musculus",
                             universe = NULL,
                             pvalueCutoff = 1,
                             pAdjustMethod = "BH",
                             minGSSize = 5,
                             maxGSSize = 10000,
                             qvalueCutoff = 1) {
  gs_cats <- c("H", "C2", "C3", "C4", "C5", "C6", "C7", "C8")
  lapply(gs_cats, function(gs_cat) {
    message(glue::glue("MSigDB category {gs_cat}"))
    OR_msigdb(
      gene,
      species = species,
      gs_cat = gs_cat,
      gs_subcat = NULL,
      universe = universe,
      pvalueCutoff = pvalueCutoff,
      pAdjustMethod = pAdjustMethod,
      minGSSize = minGSSize,
      maxGSSize = maxGSSize,
      qvalueCutoff = qvalueCutoff
    )
  }) -> or.resLst
  names(or.resLst) <- gs_cats
  # Save in Excel file
  wb <- openxlsx::createWorkbook()
  for (gs_cat in gs_cats) {
    tab_name <- ifelse(gs_cat == "H", "Hallmark", gs_cat)
    openxlsx::addWorksheet(wb, tab_name)
    openxlsx::writeData(wb, sheet = tab_name, or.resLst[[gs_cat]]$df)
  }
  openxlsx::addWorksheet(wb, "Gene")
  openxlsx::writeData(wb, sheet = "Gene", gene)
  openxlsx::saveWorkbook(
    wb,
    file = excel_file,
    overwrite = TRUE
  )
  or.resLst
}
