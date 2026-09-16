# Spatial Working Memory Development

Analysis code for the manuscript "Working Memory Development in an Adaptive Learning Environment: Cross-Sectional and Longitudinal Analyses of Primary School Children".

## Data
The data are not included in this repository. The data used in this study are proprietary and therefore cannot be shared publicly. However, access to the data is available under a research agreement with Prowise Learn. Researchers interested in obtaining access may contact the corresponding author, who can facilitate communication with Prowise Learn.

The scripts expect the data files in a `data/` folder in the project root. Therefore, place the data files in `data/` and run the scripts. Filtered data and plots are also saved in this folder. 

## Repository structure

```
spatial-wm-development/
├── LICENSE
├── README.md
├── scripts
│   ├── cross_sectional
│   │   ├── 00-full_data_selection_crossSec.R
│   │   ├── 01-full_crossSec_analysis1_setSize_Prop.R
│   │   └── 02-full_crossSec_analysis_main.R
│   └── longitudinal
│       ├── analyses
│       │   ├── 01-longt_meanfield_model_training_fits_strict.R
│       │   ├── 02-longt_meanfield_model_comparison_strict.R
│       │   ├── 03-longt_meanfield_full_final_model_fits_strict.R
│       │   ├── 04-longt_meanfield_training_item_estimates_strict.R
│       │   └── 05-longt_meanfield_full_model_plot.R
│       └── data_and_plots
│           ├── 00-full_data_selection_longt.R
│           ├── 01-full_longt_training_data.R
│           ├── 02-full_longt_data_visualize.R
│           ├── 03-full_longt_time_variables.R
│           └── 04-raw_data_percentages.R
└── spatial-wm-development.Rproj
```

## How to run
Run the scripts in `scripts/` in numbered order. 

### Cross-sectional analyses (`scripts/cross_sectional/`)

| Script | What it does |  Plot in main text |
|---|---|---|
| `00-full_data_selection_crossSec.R` | Selects the cross-sectional sample | – |
| `01-full_crossSec_analysis1_setSize_Prop.R` | [Analysis 1: proportion correct by set size] | [Figure 6] |
| `02-full_crossSec_analysis_main.R` | Main cross-sectional analysis | [Figure 7] |

### Longitudinal analyses

Run `data_and_plots/` first, then `analyses/`.

**Data preparation (`scripts/longitudinal/data_and_plots/`)**

| Script | What it does | Plot in main text |
|---|---|---|
| `00-full_data_selection_longt.R` | Selects the longitudinal sample | – |
| `01-full_longt_training_data.R` | Creates the training data subset | – |
| `02-full_longt_data_visualize.R` | Visualizes the longitudinal data | [Figure 9] |
| `03-full_longt_time_variables.R` | Creates the time variables | – | [Figure 5]
| `04-raw_data_percentages.R` | Descriptive percentages of the raw data | – |

**Models (`scripts/longitudinal/analyses/`)**

| Script | What it does | Plot in main text |
|---|---|---|
| `01-longt_meanfield_model_training_fits_strict.R` | Fits candidate models to the training data | – |
| `02-longt_meanfield_model_comparison_strict.R` | Compares the candidate models | - |
| `03-longt_meanfield_full_final_model_fits_strict.R` | Fits the selected model to the full data | – |
| `04-longt_meanfield_training_item_estimates_strict.R` | Item estimates from the training data | – |
| `05-longt_meanfield_full_model_plot.R` | Model prediction  | [Figure 8] |


## File paths

All paths in the scripts are relative to the project folder, built with the [`here`](https://here.r-lib.org/) package, so no paths need to be
changed. The data are not included in this repository; to rerun the
analyses, place the data in a `data/` folder in the project root.

## License
Code is released under the MIT License.