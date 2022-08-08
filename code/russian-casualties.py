# Setup
import numpy as np
import pandas as pd

import plotly
import plotly.graph_objs as go
import plotly.express as px
from plotly.subplots import make_subplots

# Load data Russian casualties
russian_casualties = pd.read_csv("data/russia_losses_personnel.csv")

# Load data Russian equipment
russian_equipment = pd.read_csv("data/russia_losses_equipment.csv")

