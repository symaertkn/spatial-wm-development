# Spatial Working Memory Development

Analysis code for the manuscript "WM ".

## Data
The data are not included in this repository. [One sentence on how
the data can be obtained, matching your manuscript's data statement.]

## Repository structure

```
spatial-wm-development/
├── README.md              # this file
├── LICENSE
├── spatial-wm-development.Rproj
├── scripts/               # analysis code, run in numbered order
│   ├── 01_preprocess.R
│   ├── 02_fit_models.R
│   └── 03_figures.R
└── output/
    ├── figures/           # figures in the paper
    └── models/            # saved model fits
```

## How to run
Run the scripts in `scripts/` in numbered order:

| Script | What it does | Paper output |
|---|---|---|
| `01_preprocess.R` | [cleans the data] | – |
| `02_fit_models.R` | [fits the models] | Table 2 |
| `03_figures.R` | [makes the figures] | Figures 1–3 |

## License
Code is released under the MIT License.