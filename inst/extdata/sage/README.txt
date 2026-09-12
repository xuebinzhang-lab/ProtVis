Sage executable

The Windows package includes the Sage executable (`windows/sage.exe`). ProtVis
uses this bundled executable automatically with `protvis_sage_executable()`.
The compressed copy (`windows/sage.exe.gz`) is retained as a fallback for
package layouts that omit the uncompressed binary. On Linux/macOS, install
Sage separately and ensure that `sage` is on PATH.
