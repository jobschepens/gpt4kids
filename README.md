# GPT4Kids: LLM-Generated Linguistic Corpora

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.16412822.svg)](https://doi.org/10.5281/zenodo.16412822)

## Setup

### For Paper Reproduction:

1. Clone the repository.
2. Download the research data from Zenodo: https://doi.org/10.5281/zenodo.16412822
Unzip the data into the data-original directory so the folder structure matches the repository layout.

**Note**: The Zenodo archive (exp123.zip) contains the LLM-generated experimental corpora for all three studies. Additional reference datasets (books.csv, SUBTLEX-DE, childLex) may need separate acquisition for full reproduction.

## Data and Code

### DOI's
- **Code DOI**: `10.5281/zenodo.16415370`
- **Data DOI**: `10.5281/zenodo.16412822` - LLM-generated experimental corpora

## Data Availability

### Included in Zenodo Archive
- **Experiment 1**: GPT generated corpora vs childLex comparison data
- **Experiment 2**: Temperature and audience variation experimental data  
- **Experiment 3**: Open-weight LLMs (Llama, DeepSeek) generated corpora

### Additional Reference Data (May Require Separate Downloads):
- **books.csv**: German children's book titles (for corpus generation)
- **SUBTLEX-DE**: German frequency reference corpus
- **childLex**: Children's reading corpus baseline  
- **devel/**: Reading performance behavioral data


## 🔧 Dependencies
- R (>= 4.0): [detailed package list in environment.yml]
- Python (>= 3.8): For corpus generation