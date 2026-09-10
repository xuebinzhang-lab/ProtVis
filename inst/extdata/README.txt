The small DIA-NN, FragPipe, Skyline, and OpenMS files in this directory are
subsets of a real Candida albicans quantitative proteomics experiment.  The
measurements and protein identifiers come from the `proteinGroups.txt` test
dataset distributed with TraianProt (GPL-3.0), with WT and WT_H2O2 runs kept
as four biological replicates per condition.  The files are converted into
source-specific table layouts so each adapter can be exercised without
shipping raw mass-spectrometry files.  They are not simulated values.

Upstream data and metadata:
https://github.com/SamueldelaCamaraFuentes/TraianProt/tree/main/inst/extdata

The WT/WT_H2O2 sample mapping is retained by
`.protvis_builtin_sample_info()` and includes the organism and accession
fields used by DEP and enrichment analysis.

Use protvis_builtin_datasets() to list the files and references, and
load_protvis_builtin_data(source = "DIA-NN") (or another listed source) to
load one directly into a ProtVis_dataset. The resulting object can be passed
to run_protvis_pipeline(), checkpoint functions, and downstream modules.

Maxquant_Export.xlsx remains the full reproducible MaxQuant example. Its
importer removes rows marked Reverse, Potential contaminant, or Only
identified by site by default.
