Proyecto-DataMining

Parte 2 del proyecto de curso

Universidad de San Carlos de Guatemala Facultad de Ingenieria Escuela de Estudios de Postgrado

Maestria en Ingenieria para la Industria con Especialidad en Ciencias de la Computación

MIICC408 - Introducción a la Minería de Datos Cat. Ing. MSC. Kevin Lajpop Sección A
Presentado por: Carlos Alberto Rios Calderon Carnet: 1000-17078

Requerimientos para uso y replica del repositorio

Hardware: Procesador: Intel i5 (8ª generación o superior) o AMD Ryzen 5. RAM: Mínimo 8 GB. Recomendado: 16 GB Disco: SSD (mínimo 256 GB) Se tienen alrededor de 800,000 registros

Software: R (versión ≥ 4.0) – Interprete RStudio – Entorno de desarrollo Paquetes específicos en R

    Tidyverse
    Stats
    dplyr

Para el uso en Python
# Creando el entorno de trabajo
# Importar librerías básicas
import pandas as pd
import numpy as np

# Visualización de los datos
import matplotlib.pyplot as plt
import seaborn as sns

# Preprocesamiento
from sklearn.preprocessing import LabelEncoder, StandardScaler
from sklearn.model_selection import train_test_split

# Librerías de la red neuronal (Keras)
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Dropout
