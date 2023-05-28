import pandas as pd
import pm4py
from pm4py.algo.discovery.heuristics import algorithm as heuristics_miner
from pm4py.objects.conversion.log import converter as log_converter
from pm4py.objects.petri_net.exporter.variants import pnml as pnml_exporter
from pm4py.objects.petri_net.importer.variants import pnml as pnml_importer
import warnings
import graphviz
from pm4py.visualization.petri_net import visualizer as pn_visualizer
warnings.filterwarnings('ignore')

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


def get_event_log(csv_file: str):
    data_frame = pd.read_csv(csv_file)

    data_frame["timestamp"] = data_frame["timestamp"].astype("datetime64[ns]")
    data_frame = pm4py.format_dataframe(data_frame, case_id='timestamp',
                                        activity_key='action', timestamp_key='timestamp')

    return pm4py.convert_to_event_log(data_frame)


# Scopro il modello di rete di Petri dall'event_log utilizzando l'algoritmo Heuristics Miner
net, initial_marking, final_marking = heuristics_miner.apply(
    get_event_log("combined_dataset.csv"))

# Esporto infine la rete in formato pnml
pnml_exporter.export_net(net, initial_marking,
                         'output/output_heuristic_miner.pnml')


#################### VISUALIZZAZIONE DEL FILE PNML #####################################

# Carica la rete di Petri dal file PNML
petri_net, initial_marking, final_marking = pnml_importer.import_net(
    'output/output_heuristic_miner.pnml')

# Salvo nella cartella output la rete di Petri secondo l'algoritmo Heruistic miner
gviz = pn_visualizer.apply(petri_net, initial_marking, final_marking)
pn_visualizer.save(gviz, "output/euristic_petri.png")
