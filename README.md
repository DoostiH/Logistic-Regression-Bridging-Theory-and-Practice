# Logistic Regression: Bridging Theory and Practice

This repository contains the R code and datasets accompanying the book:

> **Logistic Regression: Bridging Theory and Practice**
> Hassan Doosti
> Chapman and Hall/CRC (Taylor & Francis), 2026
> Print ISBN 9781041248125 · eBook ISBN 9781003743675 · DOI 10.1201/9781003743675

📖 *Book page:* https://www.taylorfrancis.com/books/mono/10.1201/9781003743675/logistic-regression-hassan-doosti

---

## Repository Structure

```
Logistic-Regression-Bridging-Theory-and-Practice/
├── README.md
├── ERRATA.md
├── renv.lock
├── data/
│   └── (datasets used in the book)
└── R/
    ├── Chapter01_Fundamentals.R
    ├── Chapter02_Separation.R
    ├── Chapter03_RareEvents.R
    ├── Chapter04_Overdispersion.R
    ├── Chapter05_VariableSelection.R
    ├── Chapter06_Multicollinearity.R
    ├── Chapter07_Nonlinearity.R
    ├── Chapter08_Interactions.R
    ├── Chapter09_Diagnostics.R
    ├── Chapter10_Validation.R
    ├── Chapter11_Longitudinal.R
    ├── Chapter12_MultinomialOrdinal.R
    ├── Chapter13_MissingData.R
    ├── Chapter14_SurveyData.R
    ├── Chapter15_BayesianCausal.R
    └── Chapter16_Reporting.R
```

The R scripts are organised by chapter and can be run independently.

---

## R Version and Packages

All code was written and tested using **R version 4.3.3**. The packages required
for each chapter are listed at the beginning of the respective R script and in the
corresponding book chapter.

While every effort has been made to ensure the code runs correctly at the time of
writing, R and its packages are updated regularly, and their behaviour, function
names, or default arguments may change over time. To support exact reproduction,
the package versions used are recorded in [`renv.lock`](renv.lock); you can restore
that environment with:

```r
install.packages("renv")   # if not already installed
renv::restore()            # installs the exact package versions used in the book
```

Where a package change materially affects a result reported in the book, it is noted
in the [errata](ERRATA.md). If you encounter an issue, please check the errata first,
then open an issue (see **Feedback, Issues, and Errata** below).

---

## How to Use

1. Clone or download the repository:

   ```bash
   git clone https://github.com/DoostiH/Logistic-Regression-Bridging-Theory-and-Practice.git
   ```

2. (Recommended) Restore the exact package environment:

   ```r
   renv::restore()
   ```

3. Open the R script corresponding to the chapter you are reading (in the `R/` folder).

4. If not using `renv`, install any required packages listed at the top of the script,
   for example:

   ```r
   install.packages(c("package1", "package2"))
   ```

5. Run the code in RStudio or any R environment of your choice.

---

## A Note on Datasets

Many examples in the book use **simulated data**, which allows the true
data-generating parameters to be known so that each method's ability to recover them
can be assessed directly. Where an example is simulated, this is stated in the text.

A smaller number of examples use **real, publicly available datasets** (for example,
the Cleveland Heart Disease and Pima Indians Diabetes data), which are cited in the
relevant scripts and chapters. Companion analyses applying selected methods to real
datasets are being added to this repository over time.

Datasets bundled with the book are in the [`data/`](data/) folder. Where data are
sourced from public repositories or packages, appropriate citations and links are
provided within the relevant R scripts and book chapters.

---

## Suggested Reading Paths

The book is designed so that most chapters can be read relatively independently after
Chapter 1. The following reading paths are suggested for readers with specific interests:

- **Clinical and epidemiological researchers:** Ch 1 → Ch 3 → Ch 13 → Ch 15 → Ch 16
- **Machine learning practitioners:** Ch 1 → Ch 5 → Ch 6 → Ch 7 → Ch 10
- **Survey researchers:** Ch 1 → Ch 14 → Ch 13 → Ch 9
- **Researchers facing convergence problems:** Ch 1 → Ch 2 → Ch 3 → Ch 6
- **Those developing prediction models:** Ch 1 → Ch 5 → Ch 9 → Ch 10 → Ch 16
- **Longitudinal data analysts:** Ch 1 → Ch 11 → Ch 13 → Ch 9
- **Causal inference researchers:** Ch 1 → Ch 15 → Ch 13 → Ch 16

---

## Feedback, Issues, and Errata

A list of known errors and corrections is maintained in [`ERRATA.md`](ERRATA.md),
keyed to the printing of the book so you can check the status against your own copy.

If you encounter a problem with the code, find an error or typo, or have a suggestion
for improvement, please
[open an issue](https://github.com/DoostiH/Logistic-Regression-Bridging-Theory-and-Practice/issues)
or contact the author directly. Reported items are reviewed, and confirmed corrections
are added to the errata. Your feedback is welcome and helps strengthen the book for
future readers.

---

## Citation

If you use the code or datasets from this repository in your research, please cite the
book as:

> Doosti, H. (2026). *Logistic Regression: Bridging Theory and Practice.*
> Chapman and Hall/CRC. https://doi.org/10.1201/9781003743675

**BibTeX:**

```bibtex
@book{Doosti2026,
  author    = {Doosti, Hassan},
  title     = {Logistic Regression: Bridging Theory and Practice},
  publisher = {Chapman and Hall/CRC},
  year      = {2026},
  doi       = {10.1201/9781003743675},
  isbn      = {9781041248125}
}
```

---

## License

The code in this repository is made available for educational and research purposes.
<Confirm the licensing terms with the publisher and state them here — e.g., an MIT
license for the code, or a link to the book's terms of use. Consider adding a
`LICENSE` file.>
