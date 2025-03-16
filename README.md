# SoN_2023_Birds

[![MIT License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

This repository contains the **essential scripts and data** needed to **recreate the analyses and figures** for the **bird group** in the [**State of Nature Report 2023**](https://hamaarag.org.il/report/%d7%93%d7%95%d7%97-%d7%9e%d7%a6%d7%91-%d7%94%d7%98%d7%91%d7%a2-2022-%d7%9b%d7%a8%d7%9a-%d7%9e%d7%92%d7%95%d7%95%d7%9f-%d7%91%d7%99%d7%95%d7%9c%d7%95%d7%92%d7%99/) by Hamaarag.

---

## License

This project is licensed under the **MIT License** – see the [LICENSE](LICENSE) file for details.

[![MIT License](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)

---

## Contents

This repository is organized into the following directories:

- **`R/`** – Contains R scripts for data preparation, analysis, and visualization. Key components include:
  - **Data Preparation**: Scripts for preprocessing bird data before analysis.
  - **Analyses by Region**: RMarkdown files conducting analyses for different ecological regions.
  - **Plotting Functions**: Scripts for generating figures used in the report.

- **`data/`** – Includes input datasets required for the analyses.
  - **Processed Data**: Pre-prepared datasets used for analysis.
  - **Raw Data**: Trait tables and species interaction tables.

- **`output/`** – Stores the output of the analyses.
  - Currently, this folder contains **results from the models run within the RMarkdown scripts**, stored as `.rds` files.
  - Future outputs may include summary statistics, tables, and figures.

- **Project Configuration**:
  - **`SoN_2023_Birds.Rproj`** – RStudio project file.
  - **`renv/`** & **`renv.lock`** – Dependency management files for reproducibility.
  - **`.gitignore`** & **`.Rprofile`** – Configuration and environment settings.

- **Documentation**:
  - **`README.md`** – This documentation file.
  - **`LICENSE`** – License details.

---

## How to Use

To reproduce the analyses and figures:

1. **Clone the repository**:
   ```sh
   git clone https://github.com/Hamaarag/SoN_2023_Birds.git
   ```
2. **Run the R scripts**:
   ```sh
   Rscript analysis_script.R
   Rscript figures_script.R
   ```

---

## Citation

If you use this repository or its contents, please cite **both** the main report and this repository.

### **State of Nature Report 2023 – Biodiversity**
Grossbard S, Renan I (Eds). 2024. *State of Nature Report 2023 – Biodiversity*.  
Hamaarag – Israel’s National Ecosystem Assessment Program, Steinhardt Museum of Natural History, Tel Aviv University.

### **SoN_2023_Birds Repository**
Hamaarag. *State of Nature Report 2023 - Birds*. Public Repository, 2024.  
Available at: [https://github.com/Hamaarag/SoN_2023_Birds](https://github.com/Hamaarag/SoN_2023_Birds)

---

## Contact

For questions or feedback, contact **Hamaarag** at [info@hamaarag.org.il](mailto:info@hamaarag.org.il).

