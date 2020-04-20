V-Dem: Varieties of Democracy
=============================

The clean data script extract a small-ish set of core variables from the V-Dem core data, transforms them to the G&W state list, and adds several variable transformations like year to year changes in the indicators. 

The input is the latest V-Dem core data file. This is too big to keep on git, so it might need to be re-downloaded; `input/[latest V-Dem version, e.g. v10]/V-Dem-CY-Core-[vN].csv`.

Variables are imputed in some cases through mostly carry-back of the first observed value for series that are missing initial historical values. 