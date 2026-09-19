\name{protvis_architecture}
\alias{protvis_schema_version}
\alias{protvis_schema}
\alias{protvis_standardize_dataset}
\alias{protvis_assay}
\alias{register_protvis_assay}
\alias{protvis_provenance}
\alias{as_QFeatures}
\alias{from_QFeatures}
\alias{protvis_stage_dependencies}
\alias{protvis_invalidate_downstream}
\alias{protvis_workflow_status}
\alias{protvis_qc_summary}
\alias{protvis_sample_qc}
\alias{protvis_sage_qc}
\title{ProtVis dataset architecture, provenance, workflow state, and QC}
\description{
Utilities for the canonical ProtVis_dataset schema v4, append-only analysis runs, assay access,
structured provenance, dependency-aware workflow invalidation, project QC,
Sage QC, and optional Bioconductor QFeatures interoperability.
}
\usage{
protvis_schema_version()
protvis_schema()
protvis_standardize_dataset(object)
protvis_assay(object, level = c("protein", "peptide", "psm"))
register_protvis_assay(object, level = c("psm", "peptide"), data, source = NULL)
protvis_provenance(object)
as_QFeatures(object)
from_QFeatures(object, assay = "protein")
protvis_stage_dependencies()
protvis_invalidate_downstream(dataset, stage, reason = NULL,
  include_stage = TRUE, clear_results = TRUE)
protvis_workflow_status(dataset)
protvis_qc_summary(object)
protvis_sample_qc(object)
protvis_sage_qc(psms)
}
\arguments{
\item{object, dataset}{A \code{ProtVis_dataset}; for \code{from_QFeatures}, a QFeatures object.}
\item{level}{Canonical assay level.}
\item{data}{PSM- or peptide-level table to register.}
\item{source}{Optional assay source label.}
\item{assay}{Protein-level assay name in a QFeatures object.}
\item{stage}{Canonical ProtVis workflow stage.}
\item{reason}{Reason recorded for invalidation.}
\item{include_stage}{Whether the selected stage itself is invalidated.}
\item{clear_results}{Whether invalidated analysis results are removed.}
\item{psms}{A Sage-style PSM data frame.}
}
\value{
The schema helpers return lists or version strings; assay and provenance
helpers return the requested data; workflow/QC helpers return data frames or
QC lists; conversion functions return ProtVis_dataset or QFeatures objects.
}
\details{
QFeatures conversion is optional and requires QFeatures,
SummarizedExperiment, and S4Vectors. Legacy ProtVis_dataset objects are
upgraded in memory without discarding their existing analysis state. Schema v4 also maintains a project-wide append-only run registry, workflow dependency graph, and artifact index while preserving module-local run stores and legacy latest-result views. Schema v4 keeps expression_data as the active protein matrix and stores repeated module analyses as append-only run histories under analysis_results.
}
