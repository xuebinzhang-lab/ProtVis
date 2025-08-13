utils::globalVariables(
  c(
    # MaxQuant related column names
    "Only identified by site",
    "Reverse",
    "Potential contaminant",
    "Protein IDs",
    "ID",

    # Data processing related column names
    "sample_id",
    "sample_group",
    "value_fix",
    "intensity",
    "group",
    "maxquant_id",
    "Value",

    # Special symbols
    ".",

    # Other variables that may need to be declared
    "rowname"
  )
)

# icon --------------------------------------------------------------------

correlation_icon <- HTML('<svg viewBox="0 0 16 16" width="20" height="20" fill="currentColor">
        <rect x="2" y="2" width="12" height="12" stroke="currentColor" fill="none"/>
        <!-- Points below the line -->
        <circle cx="4" cy="12" r="1"/>
        <circle cx="5" cy="10.5" r="1"/>
        <circle cx="7" cy="9" r="1"/>
        <circle cx="9" cy="7.5" r="1"/>
        <circle cx="11" cy="6" r="1"/>
        <!-- Points above the line -->
        <circle cx="4.5" cy="9" r="1"/>
        <circle cx="6" cy="7.5" r="1"/>
        <circle cx="8" cy="5.5" r="1"/>
        <circle cx="10" cy="4" r="1"/>
        <circle cx="12" cy="2.5" r="1"/>
        <!-- Diagonal line -->
        <line x1="3" y1="13" x2="13" y2="3" stroke="currentColor" stroke-width="1.5"/>
    </svg>')

expression_pattern_icon <- HTML('<svg viewBox="0 0 16 16" width="20" height="20">
        <!-- 4x4 heatmap grid with varying grayscale -->
        <rect x="1" y="1" width="3" height="3" fill="#000000"/>
        <rect x="5" y="1" width="3" height="3" fill="#333333"/>
        <rect x="9" y="1" width="3" height="3" fill="#666666"/>
        <rect x="13" y="1" width="3" height="3" fill="#999999"/>

        <rect x="1" y="5" width="3" height="3" fill="#333333"/>
        <rect x="5" y="5" width="3" height="3" fill="#666666"/>
        <rect x="9" y="5" width="3" height="3" fill="#999999"/>
        <rect x="13" y="5" width="3" height="3" fill="#cccccc"/>

        <rect x="1" y="9" width="3" height="3" fill="#666666"/>
        <rect x="5" y="9" width="3" height="3" fill="#999999"/>
        <rect x="9" y="9" width="3" height="3" fill="#cccccc"/>
        <rect x="13" y="9" width="3" height="3" fill="#eeeeee"/>

        <rect x="1" y="13" width="3" height="3" fill="#999999"/>
        <rect x="5" y="13" width="3" height="3" fill="#cccccc"/>
        <rect x="9" y="13" width="3" height="3" fill="#eeeeee"/>
        <rect x="13" y="13" width="3" height="3" fill="#ffffff"/>
    </svg>')

dimensionality_reduction_analysis_icon <- HTML('<svg viewBox="0 0 16 16" width="20" height="20" fill="currentColor">
        <!-- Square outline -->
        <rect x="2" y="2" width="12" height="12" stroke="currentColor" fill="none"/>

        <!-- Left vertical ellipse with 3 points (taller than wide) -->
        <ellipse cx="4.5" cy="8" rx="1.5" ry="3" stroke="currentColor" fill="none"/>
        <circle cx="4" cy="6.5" r="0.8"/>  <!-- Top point -->
        <circle cx="4.5" cy="8" r="0.8"/>   <!-- Center point -->
        <circle cx="5" cy="9.5" r="0.8"/>   <!-- Bottom point -->

        <!-- Right vertical ellipse with 3 points (spaced farther) -->
        <ellipse cx="11.5" cy="8" rx="1.5" ry="3" stroke="currentColor" fill="none"/>
        <circle cx="11" cy="6.5" r="0.8"/>  <!-- Top point -->
        <circle cx="11.5" cy="8" r="0.8"/>  <!-- Center point -->
        <circle cx="12" cy="9.5" r="0.8"/>   <!-- Bottom point -->
    </svg>')

imputation_settings_icon <- HTML('<svg viewBox="0 0 24 24" width="24" height="24" fill="currentColor">
        <!-- Left square with NA -->
        <rect x="3" y="8" width="8" height="8" stroke="currentColor" stroke-width="1.2" fill="none"/>
        <text x="7" y="12" font-size="5" font-weight="bold" text-anchor="middle" dominant-baseline="middle">NA</text>

        <!-- Right square with num -->
        <rect x="13" y="8" width="8" height="8" stroke="currentColor" stroke-width="1.2" fill="none"/>
        <text x="17" y="12" font-size="4" font-weight="bold" text-anchor="middle" dominant-baseline="middle">num</text>
    </svg>')
