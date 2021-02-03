Coup forecasts
==============

Code and data to create the coup forecasts at https://www.predictiveheuristics.com/forecasts.

This repo is organized around tasks:

- `data-sources/`: input data source-specific code that cleans and imputes
- `make-data/`: combine various data sources to create the full dataset
- `models`: modeling-related things, including tuning experiments and the forecast models
- `website`: makes the website

Each task folder is self-contained. It will not draw on files from other folders, and output that serves as input for another task (e.g. `data-sources` creates data pieces that serve as input for `make-data`) is not automatically copied over. But each task folder has as needed an `update-inputs.R` script that will do the relevant copying over. 

## License

The forecasts are licensed under the [Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/) license (CC BY 4.0). The underlying code and data transformations are licensed under a [MIT license](https://github.com/andybega/forecaster2/LICENSE.md).
