% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enrichment.R
\name{OR_msigdb}
\alias{OR_msigdb}
\title{A wrapper of over-representation approach, MSigDB}
\usage{
OR_msigdb(
  gene,
  species = "Homo sapiens",
  gs_cat = "H",
  gs_subcat = NULL,
  universe = NULL,
  pvalueCutoff = 1,
  pAdjustMethod = "BH",
  minGSSize = 5,
  maxGSSize = 10000,
  qvalueCutoff = 1
)
}
\arguments{
\item{gene}{a vector of gene symbol.}

\item{species}{an one-element character vector of species. Default to \code{"Homo sapiens"}.}

\item{gs_cat}{an one-element character vector of gene set category. Default to Hallmark gene sets.}

\item{gs_subcat}{an one-element character vector of gene set sub-category. Default to \code{NULL}.}

\item{universe}{background genes. If missing, the all genes listed in the database (eg TERM2GENE table) will be used as background.}

\item{pvalueCutoff}{adjusted pvalue cutoff on enrichment tests to report}

\item{pAdjustMethod}{one of "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"}

\item{minGSSize}{minimal size of genes annotated for testing}

\item{maxGSSize}{maximal size of genes annotated for testing}

\item{qvalueCutoff}{qvalue cutoff on enrichment tests to report as significant.  Tests must pass i) \code{pvalueCutoff} on unadjusted pvalues, ii) \code{pvalueCutoff} on adjusted pvalues and iii) \code{qvalueCutoff} on qvalues to be reported.}
}
\value{
A \code{enrichResult} instance
}
\description{
Over-representation approach, MSigDB
}
\author{
Mark J Chen
}
