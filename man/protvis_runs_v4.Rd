\name{protvis_runs_v4}
\alias{add_protvis_run}
\alias{record_protvis_output}
\alias{protvis_result_registry}
\alias{protvis_result}
\alias{protvis_result_history}
\alias{protvis_latest_result}
\alias{protvis_activate_result}
\alias{protvis_workflow_graph}
\alias{protvis_artifacts}
\title{Schema-v4 append-only ProtVis analysis runs}
\description{
Record, inspect, and reactivate immutable analysis runs in a
\code{ProtVis_dataset}. Schema v4 preserves every execution instead of
overwriting previous results. Core preprocessing stages retain complete
protein-by-sample matrix states; ordinary analyses retain result tables,
statistics, plot data and configuration, files, parameters, and dependencies.
}
\usage{
add_protvis_run(
  object,
  module,
  method = NULL,
  category = NULL,
  status = "success",
  parameters = list(),
  tables = list(),
  matrices = list(),
  statistics = list(),
  plot_data = list(),
  plot_config = list(),
  files = list(),
  dependencies = NULL,
  parent_run_id = NULL,
  source = "ProtVis",
  activate_matrix = FALSE,
  expression_data = NULL,
  sample_info = NULL,
  variable_info = NULL,
  legacy_value = NULL,
  record_process = TRUE
)

record_protvis_output(
  object,
  module,
  method = NULL,
  category = NULL,
  parameters = list(),
  tables = list(),
  matrices = list(),
  statistics = list(),
  plot_data = list(),
  plot_config = list(),
  files = list(),
  dependencies = NULL,
  status = "success"
)

protvis_result_registry(object, module = NULL, category = NULL)
protvis_result(object, run_id)
protvis_result_history(object, module, category = NULL)
protvis_latest_result(object, module, category = NULL)
protvis_activate_result(object, run_id)
protvis_workflow_graph(object)
protvis_artifacts(object)
}
\arguments{
  \item{object}{A \code{ProtVis_dataset}.}
  \item{module}{Stable module identifier, for example
  \code{"normalization"}, \code{"gsea"}, or \code{"wgcna_modules"}.}
  \item{method}{Method or analysis engine used by the run.}
  \item{category}{Optional result category. When omitted, ProtVis infers one
  from the module name.}
  \item{status}{Run status, usually \code{"success"}, \code{"error"}, or
  \code{"skipped"}.}
  \item{parameters}{Named list of analysis parameters.}
  \item{tables}{Named list of result data frames.}
  \item{matrices}{Named list of result matrices.}
  \item{statistics}{Named list of scalar, vector, or structured statistics.}
  \item{plot_data}{Named list of data used to reproduce plots.}
  \item{plot_config}{Named list of visualization settings.}
  \item{files}{Named list of input/output artifact paths.}
  \item{dependencies}{Optional character vector of upstream run identifiers.}
  \item{parent_run_id}{Optional immediate parent run identifier.}
  \item{source}{Source label for the run.}
  \item{activate_matrix}{Whether this run should become the active matrix
  state.}
  \item{expression_data}{Optional protein-by-sample matrix to retain and
  activate.}
  \item{sample_info}{Optional sample metadata snapshot for a matrix state.}
  \item{variable_info}{Optional protein metadata snapshot for a matrix state.}
  \item{legacy_value}{Optional legacy result object retained inside the run.}
  \item{record_process}{Whether to append the corresponding process/provenance
  event and seal the run.}
  \item{run_id}{A schema-v4 run identifier.}
}
\value{
\code{add_protvis_run()}, \code{record_protvis_output()}, and
\code{protvis_activate_result()} return an updated
\code{ProtVis_dataset}. Registry and graph functions return data frames or
lists. Result accessors return one run or a list of runs.
}
\details{
The project-wide canonical run store is
\code{analysis_results$v4$runs}. The logical fields
\code{result_registry}, \code{workflow}, and \code{artifacts} are backed by
\code{metadata}; this avoids changing the physical S4 slot layout and keeps
older serialized ProtVis projects readable.

Module-local append-only stores such as
\code{analysis_results$metaproteomics$runs} remain supported. Their newest run
is mirrored into the project-wide registry. Assignments made by older modules
to top-level \code{analysis_results$<module>} entries are also automatically
captured as schema-v4 runs while the top-level value remains available as a
compatibility/latest view.

For matrix-producing preprocessing stages, the complete expression matrix plus
sample and protein metadata state is retained. Activating such a historical run
updates \code{expression_data} and the aligned metadata while preserving all
newer runs in history.
}
