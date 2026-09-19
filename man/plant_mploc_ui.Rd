\name{plant_mploc_ui}
\alias{plant_mploc_ui}
\alias{plant_mploc_server}
\title{Plant-mPLoc Shiny Module}
\usage{
plant_mploc_ui(id)

plant_mploc_server(id)
}
\arguments{
\item{id}{Module namespace identifier.}
}
\value{
The UI function returns a Shiny UI definition. The server function returns a
Shiny module server value containing the current prediction.
}
\description{
Provides sequence input, guarded online submission, result display, raw
response inspection, and CSV download for Plant-mPLoc predictions.
}
