The small DIA-NN, FragPipe, Skyline, OpenMS, Proteome Discoverer, and
Spectronaut files in this directory are source-specific adapter views of the
real `Maxquant_Export.xlsx` maize proteomics export bundled in this package.
Each compact file retains exactly 5,000 valid protein groups (rows selected
from the upstream export without duplication) and two real B73/Y12 TMT
measurements, one channel per group, so the examples stay small while still
supporting a two-group DEP comparison.
They are converted into source-specific table layouts so every adapter can be
exercised without shipping raw mass-spectrometry files.  They are not
simulated values and should not be interpreted as independent software
searches.

An accuracy audit removed fields that were not present in the upstream table:
the DIA-NN q-values, Skyline peptide counts, OpenMS peptide counts, and
invented gene descriptions are no longer included.  Only upstream protein
identifiers and intensity measurements are retained.

Upstream data and metadata:
https://github.com/xuebinzhang-lab/ProtVis/blob/dev/inst/extdata/Maxquant_Export.xlsx

The B73/Y12 sample mapping is retained by
`.protvis_builtin_sample_info()` and includes the organism, upstream file
identifier, TMT batch, replicate, and source URL used by DEP and enrichment
analysis.  The full MaxQuant workbook remains available for users who need
all source rows; the compact adapters are intended for fast examples.

Use protvis_builtin_datasets() to list the files and references, and
load_protvis_builtin_data(source = "DIA-NN") (or another listed source) to
load one directly into a ProtVis_dataset. The resulting object can be passed
to run_protvis_pipeline(), checkpoint functions, and downstream modules.

Maxquant_Export.xlsx remains the full reproducible MaxQuant example. Its
importer removes rows marked Reverse, Potential contaminant, or Only
identified by site by default.
