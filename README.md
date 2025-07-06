# 📊 Business & Marketing Data Driven Analysis

This repository showcases two consulting-style data science projects focused on real-world applications in public health and retail analytics. Each project leverages data modeling, visual storytelling, and domain insight to deliver strategic recommendations.

---

## 👥 Authors

- **Antonella Convertini**
- **Valeria Riccardo**
- **Aleksandr Dudakov**
- **Timur Rezepov**
- **Andreas Casini**

---

## 📁 Repository Structure

Business_Marketing_Analysis/

├── EDA.ipynb # Python notebook for retail data analysis

├── EDA slides.pptx # Summary presentation of EDA results

├── Consulting.r # R script: healthcare expenditure panel model

├── Consulting_v2.1.pdf # Business-style PDF presentation of health results

├── README.md # This file


---

## 🛒 Project 1: Retail Data Analysis & Customer Segmentation

**Files**: `EDA.ipynb`, `EDA slides.pptx`

### 🧭 Objective

Explore customer behavior and optimize marketing strategy by analyzing sales, returns, and user engagement across countries.

### 🔍 Key Insights

- **Shopping Time Patterns**: Users mostly purchase on **Thursday mornings** and **Monday afternoons**.
- **Best Countries**: Netherlands, Australia, Singapore — high CLV, low return rates.
- **Worst Countries**: USA, Saudi Arabia, Bahrain — high refund rates reduce profit.
- **Customer Segmentation**: RFM clustering identifies 4 customer profiles with different retention and monetization potential.

### 📈 KPIs Analyzed

- **CLV** – Customer Lifetime Value  
- **ARPU** – Average Revenue per User  
- **AOV** – Average Order Value  
- **AIPU** – Average Items per User  
- **Refund Rate**

### 📦 Technologies

- Python
- Jupyter Notebook
- pandas, seaborn, scikit-learn, plotly

---

## 🏥 Project 2: Health Expenditure and Life Expectancy

**Files**: `Consulting.r`, `Consulting_v2.1.pdf`

### 🧭 Objective

Estimate the marginal effect of healthcare spending on population longevity using World Bank panel data (2000–2021).

### 🌍 Data Sources

- [Health Expenditure per Capita, PPP (World Bank)](https://data.worldbank.org/indicator/SH.XPD.CHEX.PP.CD)
- [Life Expectancy at Birth (World Bank)](https://data.worldbank.org/indicator/SP.DYN.LE00.IN)

### 🧠 Methodology

- Fixed-effects panel data regression with regional interaction terms
- Controlling for unobserved country-specific characteristics
- Visualization and model reporting in R and PDF format

### 🔍 Key Insights

- **+3.8 years** of life expectancy gained on average by **doubling health expenditure**
- **Sub-Saharan Africa** and **South Asia** show strongest marginal gains
- **North America** shows lowest responsiveness despite high spending

### 📦 Technologies

- R, RStudio
- `plm` and `tidyverse` packages

- Python
- pip install pandas numpy matplotlib seaborn scikit-learn plotly
