# 🏆 Reserve Estimation for Insurance Claims

## 📌 Project Overview  
This project focuses on **estimating insurance reserves** using **Run-Off Triangles** and **Machine Learning models**. Due to **missing data (75%)**, we employed **Monte Carlo Simulation with a Pareto distribution** to generate synthetic values, ensuring accurate reserve predictions.

The final model was deployed in **Streamlit**, allowing users to visualize and predict claim reserves dynamically.

---

## 📊 **Project Architecture & Workflow**  
Below is the high-level architecture and methodology we followed:

![Project Workflow](./assets/workflow_diagram.png)  
*(Replace with the correct image path once uploaded)*  

### **1️⃣ Data Preprocessing**  
- Removed **duplicates**.  
- Engineered **Development Year**.  
- Extracted **Sinistre Year** from *Date de Survenance*.  

### **2️⃣ Data Aggregation**  
- Grouped data by **Sinistre Year** & **Development Year**.  
- Calculated **Total Règlement**.  
- Reshaped data into a **Run-Off Triangle** format.  

### **3️⃣ Handling Missing Data**  
- Found **66% missing values in the upper triangle, 75% missing overall**.  
- Used **Monte Carlo Simulation (Pareto Distribution)** to generate missing values.  

### **4️⃣ Machine Learning Model Selection**  
- **Random Forest** for the first three sub-branches.  
- **Linear Regression** for the fourth sub-branch (due to limited data).  

### **5️⃣ Deployment in Streamlit**  
- Built an **interactive web app** for visualization and prediction.  
- Allows **data uploads, reserve forecasting, and model adjustments**.  

---

## 🚀 **How to Run the Project Locally**  

### **1️⃣ Clone the Repository**  
```bash
git clone https://github.com/your-username/reserve-estimation.git
cd reserve-estimation
