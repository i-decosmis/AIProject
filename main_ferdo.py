import pandas as pd
import pm4py
from pm4py.algo.discovery.heuristics import algorithm as heuristics_miner
from pm4py.objects.conversion.log import converter as log_converter
from pm4py.objects.petri_net.exporter.variants import pnml as pnml_exporter
from pm4py.objects.petri_net.importer.variants import pnml as pnml_importer

import warnings
from pm4py.visualization.petri_net import visualizer as pn_visualizer
# ignoro l'userwarning perchè specifico il formato dei dati, quindi è un warning inesistente
warnings.filterwarnings("ignore", category=UserWarning)

############################ Creo un unico file csv ################################################################################################################

"""
files = ['dataset\interaction_data_677179.csv', 'dataset\interaction_data_699333.csv', 'dataset\interaction_data_700655.csv',
         'dataset\interaction_data_703453.csv', 'dataset\interaction_data_703453.csv', 'dataset\interaction_data_704985.csv', 'dataset\interaction_data_706218.csv']
li = []

for filename in files:
    df = pd.read_csv(filename, index_col=None, header=0)
    li.append(df)

frame = pd.concat(li, axis=0, ignore_index=True)
frame.to_csv('combined_dataset.csv', index=False)

"""

# Leggo i dati dal CSV
dataframe = pd.read_csv('combined_dataset.csv')
dataframe['timestamp'] = pd.to_datetime(
    dataframe['timestamp'], format='%Y-%m-%d %H:%M:%S.%f', utc=True)

# Controllo se trovo NaT valori nelle colonne timestamp
timestamp_series = pd.to_datetime(
    dataframe['timestamp'], format='%Y-%m-%d %H:%M:%S.%f', utc=True, errors='coerce')  # qui specifico il formato, quindi dovrebbe processare correttamente, infatti ho ignorato il warning
if timestamp_series.isnull().values.any():
    print("Warning: There are NaT values in the timestamp column")

# Formatto il dataframe
dataframe = pm4py.format_dataframe(
    dataframe, case_id='id_exec', activity_key='activity', timestamp_key='timestamp')

# Converto all' oggetto event log
event_log = log_converter.apply(dataframe)

# Scopro il modello di rete di Petri dall'event_log utilizzando l'algoritmo Heuristics Miner
net, initial_marking, final_marking = heuristics_miner.apply(event_log)

# Esporto infine la rete in formato pnml
pnml_exporter.export_net(net, initial_marking, 'output_heuristic_miner.pnml')


#################### VISUALIZZAZIONE DEL FILE PNML #####################################

# Carica la rete di Petri dal file PNML
petri_net, initial_marking, final_marking = pnml_importer.import_net(
    'output_heuristic_miner.pnml')

# Visualizza la rete di Petri secondo l'algoritmo heruistic miner
gviz = pn_visualizer.apply(petri_net, initial_marking, final_marking)
pn_visualizer.view(gviz)
