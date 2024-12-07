# Report

[Report.pdf](./Report.pdf) is the pdf getting rendered from [Report.qmd](./Report.qmd). You can Generate it yourself by : 

### Instructions for PDF Rendering

This report requires a TeX distribution for PDF rendering. If you do not have one installed, please follow these steps:

1. **Install TinyTeX (recommended)**:
   - Open your terminal and run:
     ```bash
     quarto install tinytex
     ```
   - TinyTeX will be automatically set up for Quarto.

2. **Alternative Installation**:
   - You may also install a different TeX distribution if preferred, such as **TeXLive** (Linux, macOS) or **MiKTeX** (Windows).

3. **Preview the PDF**:

    > (make sure you are in the ./Reports Directory)
    - Run this in your terminal:
    ```
    quarto preview Report.qmd --to pdf --no-watch-inputs --no-browse
    ```
    - You can also run: (to just generate the pdf file)
    ```
    quarto render Report.qmd --to pdf
    ```
--- 