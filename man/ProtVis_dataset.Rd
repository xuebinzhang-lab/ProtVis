\name{ProtVis_dataset}
\alias{ProtVis_dataset}
\alias{create_protvis_dataset}
\alias{as_protvis_dataset}
\alias{protvis_as_mass_dataset}
\alias{validate_protvis_dataset}
\title{Create and validate a ProtVis dataset}
\description{
  Creates a tidyMass-compatible S4 dataset for proteomics expression data,
  metadata, annotations, analysis results, checkpoints, and provenance.
}
\usage{
create_protvis_dataset(expression_data, sample_info = NULL,
  variable_info = NULL, variable_info_note = NULL,
  sample_info_note = NULL, annotation = list(), metadata = list(),
  other_files = list(), ms2_data = list(),
  annotation_table = data.frame(), activated = "expression_data")

ProtVis_dataset(...)

as_protvis_dataset(object)

protvis_as_mass_dataset(object)

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
  \item{ms2_data}{Optional tidyMass MS2 data list.}
  \item{annotation_table}{Optional tidyMass annotation table.}
  \item{activated}{The activated tidyMass slot.}
  \item{object}{A ProtVis_dataset, mass_dataset, or legacy ProtVis object.}
  \item{strict}{Whether to reject malformed non-numeric expression columns.}
  \item{...}{Arguments passed to create_protvis_dataset.}
}
\value{
  create_protvis_dataset and ProtVis_dataset return an S4 ProtVis_dataset that
  inherits from mass_dataset. protvis_as_mass_dataset returns an exact
  mass_dataset suitable for persistent storage. validate_protvis_dataset
  returns TRUE invisibly on success and signals a descriptive error otherwise.
}
\seealso{
  \code{\link{import_protvis}},
  \code{\link{run_protvis_pipeline}},
  \code{\link{save_protvis_checkpoint}}
}
