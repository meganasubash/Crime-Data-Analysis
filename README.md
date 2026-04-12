# Crime Data Analysis with Weather Correlation

## 📌 Project Overview

This project analyzes crime data from 2020 to present and integrates historical weather data to explore patterns and relationships between environmental conditions and crime occurrences.

---

## 📊 Dataset

* Crime dataset: Contains records of reported crimes including date, location, and victim details
* Weather dataset: Retrieved using the Open-Meteo API (temperature and precipitation)

---

## ⚙️ Technologies Used

* R (data processing & analysis)
* Libraries:

  * data.table
  * dplyr
  * lubridate
  * httr
  * jsonlite
  * ggplot2

---

## 🔄 Data Pipeline

1. Load crime dataset
2. Clean and format date fields
3. Fetch historical weather data using API
4. Merge crime and weather datasets
5. Perform exploratory data analysis (EDA)
6. Apply clustering (K-Means)
7. Generate visualizations
8. Predictive analysis
9. Containerization
10. Power BI DashBoard

---

## 🚀 How to Run the Project

1. Clone the repository:

   ```
   git clone <your-repo-link>
   ```

2. Create a `data/` folder and add the dataset:

   ```
   data/Crime_Data.csv
   ```

3. Run the R script:

   ```
   source("scripts/your_script.R")
   ```

---

## 📁 Project Structure

```id="p4l8xo"
Crime-Data-Analysis/
│
├── data/              # (ignored)
├── outputs/           # plots & results
├── scripts/           # R scripts
├── .gitignore
├── README.md
```

---

## 👩‍💻 Author

Megana Subash
Riddhima Garg
