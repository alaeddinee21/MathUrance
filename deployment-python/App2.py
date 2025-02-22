import streamlit as st
import pandas as pd
import numpy as np
import time

# Configuration de la page
st.set_page_config(page_title="Insurance Models", layout="wide")

# Style CSS personnalisé
st.markdown("""
<style>
[data-testid="stHeader"] {
    background: #2c3e50;
    color: white;
}
.css-1vq4p4l {
    border: 1px solid #3498db;
    border-radius: 10px;
    padding: 20px;
}
</style>
""", unsafe_allow_html=True)

# Gestion des données uploadées
if 'uploaded_data' not in st.session_state:
    st.session_state['uploaded_data'] = None

# Sidebar navigation
st.sidebar.title("📊 Modèles Actuariels")
model_type = st.sidebar.radio("Sélection du Modèle", 
                            ["Régression Linéaire", 
                             "Forêt Aléatoire", 
                             "XGBoost", 
                             "Réseau de Neurones"])

# Fonction commune d'upload
def upload_section():
    uploaded_file = st.file_uploader("📤 Importer données CSV", type="csv")
    if uploaded_file:
        df = pd.read_csv(uploaded_file)
        st.session_state['uploaded_data'] = df
        st.success("Données chargées avec succès!")
        st.dataframe(df.head(3))
    return uploaded_file

# Fonction pour exporter les prédictions
def export_predictions(predictions):
    df = pd.DataFrame(predictions, columns=["Predictions"])
    csv = df.to_csv(index=False).encode('utf-8')
    st.download_button(
        label="📥 Exporter les Prédictions",
        data=csv,
        file_name="predictions.csv",
        mime="text/csv",
    )

# Page Régression Linéaire
if model_type == "Régression Linéaire":
    st.header("📈 Modèle de Régression Linéaire")
    
    upload_section()
    
    if st.button("🔮 Prédiction", key="lin_reg"):
        if st.session_state['uploaded_data'] is not None:
            with st.spinner("Calcul en cours..."):
                time.sleep(1.5)
                # Simulation de résultat
                st.subheader("Résultats de Prédiction")
                col1, col2 = st.columns(2)
                with col1:
                    st.metric("Réserves Prédites", "5,230,000 DA", "± 2.3%")
                    st.line_chart(np.random.randn(20))
                with col2:
                    st.write("**Coefficients du Modèle**")
                    st.json({"intercept": 12000, "slope": 2.34})
                
                # Export predictions
                predictions = np.random.randn(20)  # Simulated predictions
                export_predictions(predictions)
        else:
            st.warning("Veuillez d'abord importer des données!")

# Page Forêt Aléatoire
elif model_type == "Forêt Aléatoire":
    st.header("🌳 Modèle de Forêt Aléatoire")
    
    upload_section()
    
    if st.button("🔮 Prédiction", key="rf"):
        if st.session_state['uploaded_data'] is not None:
            with st.spinner("Analyse en cours..."):
                time.sleep(2)
                st.subheader("Prédictions")
                col1, col2 = st.columns(2)
                with col1:
                    st.metric("Estimation Totale", "8,450,000 DA", "± 1.8%")
                    st.progress(0.78)
                with col2:
                    st.write("**Importance des Variables**")
                    st.bar_chart(pd.DataFrame({
                        'Variables': ['Branche', 'Produit', 'Année'],
                        'Importance': [0.45, 0.3, 0.25]
                    }))
                
                # Export predictions
                predictions = np.random.randn(20)  # Simulated predictions
                export_predictions(predictions)
        else:
            st.warning("Données manquantes!")

# Page XGBoost
elif model_type == "XGBoost":
    st.header("🚀 Modèle XGBoost")
    
    upload_section()
    
    if st.button("🔮 Prédiction", key="xgb"):
        if st.session_state['uploaded_data'] is not None:
            with st.spinner("Optimisation..."):
                time.sleep(2.5)
                st.subheader("Prédictions")
                st.balloons()
                col1, col2 = st.columns([2,1])
                with col1:
                    st.area_chart(np.random.randn(20, 3))
                with col2:
                    st.metric("Score SHAP", "0.89", "0.02")
                    st.metric("Précision", "92%", "-3%")
                
                # Export predictions
                predictions = np.random.randn(20)  # Simulated predictions
                export_predictions(predictions)
        else:
            st.error("Importez des données d'abord!")

# Page Réseau de Neurones
else:
    st.header("🧠 Réseau de Neurones Profonds")
    
    upload_section()
    
    if st.button("🔮 Prédiction", key="nn"):
        if st.session_state['uploaded_data'] is not None:
            with st.spinner("Entraînement du réseau..."):
                time.sleep(3)
                st.subheader("Sortie du Modèle")
                tab1, tab2 = st.tabs(["Prédictions", "Architecture"])
                with tab1:
                    st.line_chart(np.random.randn(50))
                    st.metric("Perte", "0.023", "-12%")
                with tab2:
                    st.image("https://miro.medium.com/v2/resize:fit:1400/1*DW0Ccmj1hZ0OvSXi7Kz1MQ.png", 
                            width=300)
                
                # Export predictions
                predictions = np.random.randn(50)  # Simulated predictions
                export_predictions(predictions)
        else:
            st.warning("Données requises!")