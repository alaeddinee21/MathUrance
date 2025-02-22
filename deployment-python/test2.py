import streamlit as st
import pandas as pd
import numpy as np
import pickle

# Title of the app
st.title("Machine Learning Prediction App")

# Sidebar for model selection
st.sidebar.header("Select Sub-Branch")
sub_branch = st.sidebar.radio(
    "Choose a sub-branch:",
    ["Responsabilité Civile", "Incendie", "Risque Simple", "CAT-NAT"]
)

# Load the appropriate model based on the selected sub-branch
@st.cache_resource
def load_model(sub_branch):
    model_paths = {
        "Responsabilité Civile": r"C:\Users\hp\Desktop\Streamlitwebsite\responsibilitycivilemodel.pkl",
        "Incendie": r"C:\Users\hp\Desktop\Streamlitwebsite\incendiemodel.pkl",
        "Risque Simple": r"C:\Users\hp\Desktop\Streamlitwebsite\risquesimple.pkl",
        "CAT-NAT": r"C:\Users\hp\Desktop\Streamlitwebsite\catnat.pkl"
    }
    with open(model_paths[sub_branch], 'rb') as file:
        model = pickle.load(file)
    return model

model = load_model(sub_branch)

# Step 1: Upload a dataset
st.header("1. Upload Your Dataset")
uploaded_file = st.file_uploader("Choose a CSV file", type="csv")

if uploaded_file is not None:
    # Load the uploaded data
    data = pd.read_csv(uploaded_file)
    st.write("### Preview of Uploaded Data")
    st.write(data.head())

    # Step 2: Make predictions
    st.header("2. Make Predictions")
    if st.button("Predict"):
        # Ensure the model is a scikit-learn pipeline or similar
        if hasattr(model, 'predict'):
            # Make predictions
            predictions = model.predict(data)
            data['Predictions'] = predictions  # Add predictions to the dataset
            st.write("### Predictions")
            st.write(data)

            # Step 3: Export predictions
            st.header("3. Export Predictions")
            csv = data.to_csv(index=False).encode('utf-8')
            st.download_button(
                label="Download Predictions as CSV",
                data=csv,
                file_name='predictions.csv',
                mime='text/csv',
            )
        else:
            st.error("The loaded model is not valid or does not have a `predict` method.")
else:
    st.info("Please upload a CSV file to get started.")