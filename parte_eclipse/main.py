from pm4py.algo.discovery.inductive import algorithm as inductive_miner
from pm4py.statistics.traces.generic.log import case_statistics
import xml.etree.ElementTree as ET
import pandas as pd
import pm4py
from pm4py.algo.discovery.heuristics import algorithm as heuristics_miner
from pm4py.objects.conversion.log import converter as log_converter
from pm4py.objects.petri_net.exporter.variants import pnml as pnml_exporter
from pm4py.objects.petri_net.importer.variants import pnml as pnml_importer
import warnings
from pm4py.objects.log.obj import EventLog
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
    data_frame = pd.read_csv("parte_eclipse/combined_dataset.csv")
    data_frame["timestamp"] = data_frame["timestamp"].astype("datetime64[ns]")
    data_frame = pm4py.format_dataframe(
        data_frame, case_id='id_exec', activity_key='action', timestamp_key='timestamp')
    event_log = pm4py.convert_to_event_log(data_frame)
    return event_log


def heuristic(event_log: EventLog):
    # Scopro il modello di rete di Petri dall'event_log utilizzando l'algoritmo Heuristics Miner
    net, initial_marking, final_marking = heuristics_miner.apply(event_log)

    # Esporto infine la rete in formato pnml
    pnml_exporter.export_net(net, initial_marking,
                             'parte_eclipse/output/output_heuristic.pnml')

    petri_net, initial_marking, final_marking = pnml_importer.import_net(
        'parte_eclipse/output/output_heuristic.pnml')
    # Salvo nella cartella output la rete di Petri secondo l'algoritmo Heruistic miner
    gviz = pn_visualizer.apply(petri_net, initial_marking, final_marking)
    pn_visualizer.save(gviz, "parte_eclipse/output/euristic_petri.png")


def alpha(event_log: EventLog):
    # Scopro il modello di rete di Petri dall'event_log utilizzando l'algoritmo Alpha
    net, initial_marking, final_marking = pm4py.discover_petri_net_alpha(
        event_log)

    # Esporto infine la rete in formato pnml
    pnml_exporter.export_net(net, initial_marking,
                             'parte_eclipse/output/output_alpha.pnml')

    petri_net, initial_marking, final_marking = pnml_importer.import_net(
        'parte_eclipse/output/output_alpha.pnml')
    # Salvo nella cartella output la rete di Petri secondo l'algoritmo Heruistic miner
    gviz = pn_visualizer.apply(petri_net, initial_marking, final_marking)
    pn_visualizer.save(gviz, "parte_eclipse/output/alpha_petri.png")


def inductive(event_log: EventLog):
    # Scopro il modello di rete di Petri dall'event_log utilizzando l'algoritmo Heuristics Miner
    print("Starting inductive_miner algorithm")
    net, initial_marking, final_marking = pm4py.discover_petri_net_inductive(
        event_log)
    print("finisci")
    # Esporto infine la rete in formato pnml
    pnml_exporter.export_net(net, initial_marking,
                             'parte_eclipse/output/output_inductive.pnml')

    petri_net, initial_marking, final_marking = pnml_importer.import_net(
        'parte_eclipse/output/output_inductive.pnml')

    # Salvo nella cartella output la rete di Petri secondo l'algoritmo Heruistic miner
    gviz = pn_visualizer.apply(petri_net, initial_marking, final_marking)
    pn_visualizer.save(gviz, "parte_eclipse/output/inductive_petri.png")


def compare_pnml_files(heuristic, alpha, inductive, element):
    tree_1 = ET.parse(heuristic)
    root_1 = tree_1.getroot()
    elements_1 = root_1.findall(f'.//{element}')
    count_1 = len(elements_1)

    tree_2 = ET.parse(alpha)
    root_2 = tree_2.getroot()
    elements_2 = root_2.findall(f'.//{element}')
    count_2 = len(elements_2)

    tree_3 = ET.parse(inductive)
    root_3 = tree_3.getroot()
    elements_3 = root_3.findall(f'.//{element}')
    count_3 = len(elements_3)

    print(f'Number of {element} in {heuristic}: {count_1}')
    print(f'Number of {element} in {alpha}: {count_2}')
    print(f'Number of {element} in {inductive}: {count_3}')


def get_all_duration_case(log: EventLog):
    # controlla la durata di tutti i case
    log = get_event_log("parte_eclipse/combined_dataset.csv")
    case_durations = case_statistics.get_all_case_durations(
        log, parameters={"timestamp_key": "time:timestamp"})

    for i, duration in enumerate(case_durations):
        print(f"Case {i} duration: {duration}")


heuristic(get_event_log("parte_eclipse/combined_dataset.csv"))
alpha(get_event_log("parte_eclipse/combined_dataset.csv"))
inductive(get_event_log("parte_eclipse/combined_dataset.csv"))

get_all_duration_case("parte_eclipse/combined_dataset.csv")

# Confronta il numero di 'place' nei file
compare_pnml_files("parte_eclipse/output/output_heuristic.pnml",
                   "parte_eclipse/output/output_alpha.pnml", "parte_eclipse/output/output_inductive.pnml", "place")

# Confronta il numero di 'transition' nei file
compare_pnml_files("parte_eclipse/output/output_heuristic.pnml",
                   "parte_eclipse/output/output_alpha.pnml", "parte_eclipse/output/output_inductive.pnml", "transition")
