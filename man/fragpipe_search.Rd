\name{fragpipe_search}
\alias{fragpipe_search_server}
\alias{fragpipe_search_ui}
\alias{protvis_fragpipe_executable}
\alias{protvis_fragpipe_qc}
\alias{protvis_fragpipe_status}
\alias{protvis_fragpipe_workflow_presets}
\alias{protvis_fragpipe_workflows}
\alias{protvis_fragpipe_write_manifest}
\alias{protvis_install_fragpipe}
\alias{run_fragpipe_search}
\title{FragPipe search integration}
\description{
Install or locate an official FragPipe runtime, prepare headless workflow and
manifest files, run FragPipe from ProtVis, inspect runtime status and search
quality control, and expose the Shiny Search backend.
}
\usage{
protvis_fragpipe_executable(path = NULL)

protvis_fragpipe_status(path = NULL)

protvis_install_fragpipe(
  version = "24.0",
  directory = file.path(path.expand("~"), ".protvis", "tools", "fragpipe"),
  launch_windows = TRUE
)

protvis_fragpipe_workflow_presets()

protvis_fragpipe_workflows(path = NULL)

protvis_fragpipe_write_manifest(
  spectra_paths,
  path,
  sample_info = NULL,
  data_type = "DDA"
)

run_fragpipe_search(
  fasta,
  spectra_paths,
  output_directory,
  workflow = "LFQ + MBR",
  sample_info = NULL,
  data_type = "DDA",
  fragpipe_path = NULL,
  tools_folder = NULL,
  python = NULL,
  diann = NULL,
  threads = 1L,
  ram = 0L,
  dry_run = FALSE
)

protvis_fragpipe_qc(bundle)

fragpipe_search_ui(id)

fragpipe_search_server(id, shared_state)
}
\arguments{
  \item{path}{Optional FragPipe executable, installation directory, workflow
  manifest output path, or local runtime path, depending on the function.}
  \item{version}{FragPipe release tag requested from the official release.}
  \item{directory}{Parent directory for a ProtVis-managed FragPipe runtime.}
  \item{launch_windows}{Whether to launch the downloaded official Windows
  installer.}
  \item{spectra_paths}{Paths to spectra inputs supported by the selected
  FragPipe workflow.}
  \item{sample_info}{Optional sample metadata used to construct the manifest.}
  \item{data_type}{FragPipe manifest data type, typically \code{"DDA"} or
  \code{"DIA"}.}
  \item{fasta}{Protein FASTA database path.}
  \item{output_directory}{Directory used for FragPipe outputs.}
  \item{workflow}{Installed workflow file, workflow path, or preset name.}
  \item{fragpipe_path}{Optional FragPipe executable or installation directory.}
  \item{tools_folder}{Optional FragPipe companion-tools directory.}
  \item{python}{Optional Python executable or directory.}
  \item{diann}{Optional DIA-NN executable used by applicable workflows.}
  \item{threads}{Number of worker threads.}
  \item{ram}{Maximum RAM in GB; zero lets FragPipe choose.}
  \item{dry_run}{Whether to request FragPipe headless dry-run validation.}
  \item{bundle}{A FragPipe search bundle returned by
  \code{run_fragpipe_search()}.}
  \item{id}{Shiny module identifier.}
  \item{shared_state}{ProtVis application shared reactive state.}
}
\value{
Runtime and workflow helpers return paths, status lists, or data frames.
\code{run_fragpipe_search()} returns a search bundle containing status, logs,
runtime metadata, PSM, peptide and protein tables, and generated files. The
Shiny module functions return UI/server module objects.
}
\details{
ProtVis does not redistribute separately licensed FragPipe companion binaries.
The installer helper obtains the official platform release at runtime. Search
results are integrated into \code{ProtVis_dataset} by the Search module with
provenance and PSM/peptide assay registration.
}
