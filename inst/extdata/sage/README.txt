Sage executable

The package includes platform-specific Sage executables:

- `windows/sage.exe` for Windows
- `Linux/sage` for Linux
- `macOS/ARM64/sage` for Apple Silicon Macs
- `macOS/Intel/sage` for Intel Macs

ProtVis selects the bundled executable for the current operating system and
architecture with `protvis_sage_executable()`. If no bundled executable is
available, it falls back to `sage` on PATH.
