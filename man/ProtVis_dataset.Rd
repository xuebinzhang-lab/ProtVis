\name{ProtVis_dataset}
\alias{ProtVis_dataset}
\alias{create_protvis_dataset}
\alias{validate_protvis_dataset}
\title{Create and validate a ProtVis dataset}
\description{
  Creates the standardized ProtVis_dataset container for proteomics expression
  data, sample metadata, variable metadata, annotations, analysis results,
  checkpoints, and process provenance.
}
\usage{
create_protvis_dataset(expression_data, sample_info = NULL,
  variable_info = NULL, variable_info_note = NULL,
  sample_info_note = NULL, annotation = list(), metadata = list(),
  other_files = list())

ProtVis_dataset(...)

validate_protvis_dataset(object, strict = TRUE)
}
\arguments{
  \item{expression_data}{Protein-by-sample data.frame or matrix.}
  \item{sample_info}{Optional sample metadata table.}
  \item{variable_info}{Optional protein metadata table.}
  \item{variable_info_note}{Optional variable metadata documentation.}
  \item{sample_info_note}{Optional sample metadata documentation.}
  \item{annotation}{Optional annotation list or table.}
  \item{metadata}{Optional project metadata list.}
  \item{other_files}{Optional imported-file provenance list.}
  \item{object}{A ProtVis_dataset object.}
  \item{strict}{Whether to reject malformed non-numeric expression columns.}
  \item{...}{Arguments passed to create_protvis_dataset.}
}
\value{
  create_protvis_dataset and ProtVis_dataset return a ProtVis_dataset object.
  validate_protvis_dataset returns TRUE invisibly on success and signals a
  descriptive error otherwise.
}
\seealso{
  \code{\link{import_protvis}},
  \code{\link{run_protvis_pipeline}},
  \code{\link{save_protvis_checkpoint}}
}
