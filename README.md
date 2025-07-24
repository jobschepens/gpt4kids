# GPT4Kids: LLM-Generated Linguistic Corpora Study

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.16412822.svg)](https://doi.org/10.5281/zenodo.16412822)

> **Comparing LLM-generated word frequency measures to traditional corpus-based measures for German children's reading performance.**

## 🎯 Setup

### For Paper Reproduction:

1. Clone the repository.
2. Download the research data from Zenodo: https://doi.org/10.5281/zenodo.16412822
Unzip the data into the data-original directory so the folder structure matches the repository layout.

**Note**: The Zenodo archive (exp123.zip) contains the LLM-generated experimental corpora for all three studies. Additional reference datasets (books.csv, SUBTLEX-DE, childLex) may need separate acquisition for full reproduction.

## 📚 Data Management

### Dual DOI  
- **Code DOI**: `10.5281/zenodo.XXXXXX` - Methodology, software, reproducibility
- **Data DOI**: `10.5281/zenodo.16412822` - LLM-generated experimental corpora

### Why Separate Storage?
- ✅ **Faster collaboration**: Lightweight repo (~50MB vs 843MB)
- ✅ **Journal compliance**: Meets open data & software requirements  
- ✅ **Long-term preservation**: CERN-backed permanent storage
- ✅ **Flexible updates**: Data and code can evolve independently
- ✅ **FAIR principles**: Findable, Accessible, Interoperable, Reusable

## 📊 Data Availability

### Included in Zenodo Archive (DOI: 10.5281/zenodo.16412822):
- **Experiment 1**: GPT-3.5 generated corpora vs childLex comparison data
- **Experiment 2**: Temperature and audience variation experimental data  
- **Experiment 3**: Open-weight LLMs (Llama, DeepSeek) generated corpora

### Additional Reference Data (May Require Separate Acquisition):
- **books.csv**: German children's book titles (for corpus generation)
- **SUBTLEX-DE**: German frequency reference corpus
- **childLex**: Children's reading corpus baseline  
- **devel/**: Reading performance behavioral data

*The core LLM-generated experimental data is preserved in the Zenodo archive. Reference datasets may need separate acquisition for full reproduction.*

## 🔧 Technical Details

### Dependencies:
- **R** (≥ 4.0): `tm`, `udpipe`, `ggplot2`, `lme4`, `tidyverse`
- **Python** (≥ 3.8): `openai`, `pandas`, `jupyter` (for corpus generation)
- **German linguistic model**: UDPipe German-GSD (auto-downloaded)

### Conditions & Variables:
- **Temperature**: 0.5 (low) vs 0.9 (high) for text generation
- **Audience**: Adult-directed vs child-directed prompts
- **Models**: GPT-3.5, Llama, DeepSeek with length variations

```


## 🔧 Dependencies
- R (>= 4.0): [detailed package list in environment.yml]
- Python (>= 3.8): For corpus generation
- UDPipe German model: Auto-downloaded in setup
