\name{ProtVis_dataset}
\alias{ProtVis_dataset}
\alias{create_protvis_dataset}
\alias{as_protvis_dataset}
\alias{validate_protvis_dataset}
\title{Create and validate a ProtVis dataset}
\description{
  Creates an independent ProtVis S4 dataset for proteomics expression data,
  metadata, annotations, analysis results, checkpoints, and provenance.
}
\details{
  ProtVis_dataset does not inherit from mass_dataset or require the
  massdataset package. Its validity method enforces exact alignment between
  expression columns and sample_info, and between expression rows and
  variable_info. The feature_info field is an alias of variable_info. The
  annotation slot has a fixed three-table schema: eggnog_output,
  GO_annotation, and KEGG_annotation. There are no annotation_table or
  ms2_data slots.
}
\usage{
create_protvis_dataset(expression_data, sample_info = NULL,
  variable_info = NULL, variable_info_note = NULL,
  sample_info_note = NULL, annotation = list(), metadata = list(),
  other_files = list(), activated = "expression_data")

ProtVis_dataset(...)

as_protvis_dataset(object)

validate_protvis_dataset(object, strict = TRUE)
}
\arguments{
  \item{expression_data}{Protein-by-sample data.frame or matrix.}
  \item{sample_info}{Optional sample metadata table.}
  \item{variable_info}{Optional protein metadata table.}
  \item{variable_info_note}{Optional variable metadata documentation.}
  \item{sample_info_note}{Optional sample metadata documentation.}
  \item{annotation}{A named list containing the data.frames
  \code{eggnog_output}, \code{GO_annotation}, and
  \code{KEGG_annotation}. Missing components are created as empty
  data.frames.}
  \item{metadata}{Optional project metadata list.}
  \item{other_files}{Optional imported-file provenance list.}
  \item{activated}{Name of the active core data component.}
  \item{object}{A ProtVis_dataset, mass_dataset, or legacy ProtVis object.}
  \item{strict}{Whether to reject malformed non-numeric expression columns.}
  \item{...}{Arguments passed to create_protvis_dataset.}
}
\value{
  create_protvis_dataset and ProtVis_dataset return an independent S4
  ProtVis_dataset. validate_protvis_dataset returns TRUE invisibly on success
  and signals a descriptive error otherwise.
}
\seealso{
  \code{\link{import_protvis}},
  \code{\link{run_protvis_pipeline}},
  \code{\link{save_protvis_checkpoint}}
}
