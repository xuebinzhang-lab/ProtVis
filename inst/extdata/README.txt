The small files in this directory are reproducible built-in fixtures for the
ProtVis_dataset import adapters. They contain a few proteins and samples so
they are suitable for examples and automated tests without making the package
large. They are shaped like exports documented by the corresponding vendors
or community standards; they are not raw mass-spectrometry files.

Use protvis_builtin_datasets() to list the files and references, and
load_protvis_builtin_data(source = "DIA-NN") (or another listed source) to
load one directly into a ProtVis_dataset. The resulting object can be passed
to run_protvis_pipeline(), checkpoint functions, and downstream modules.

Maxquant_Export.xlsx remains the full reproducible MaxQuant example. Its
importer removes rows marked Reverse, Potential contaminant, or Only
identified by site by default.
