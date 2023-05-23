import csv
import copy
import os

import graphviz
import xml.etree.ElementTree as ET
import glob, os
import xml.dom.minidom as md
from lxml import etree

# Funzione usata per caricare i nomi dei dataset da cui estrarre le informazioni


def get_file_names_in_folder(folder_path):
    if not os.path.isdir(folder_path):
        print(f"Percorso non valido")
        return []

    file_names = []
    for _, _, files in os.walk(folder_path):
        file_names.extend(files)

    return file_names

# Testato con "interaction_data_7076218.csv" con 10976 place (activity/2) che vengono contate tutte con successo,
# 10942 transizioni (10976-(17(processi)*2(tolgo una transizione all'inizio e una alla fine)), che vengono contate tutte con successo e 17 processi che vegono contati tutti con successo.
# contati = contati e salvati in memoria tramite oggetti e array globali da cui e' possibile recuperare le informazioni

# Creo la classe processi che conterra' tutti i processi
# Ogni processo ha places e transitions


class Processes:
    def __init__(self, input_places, input_transitions):
        self.places = copy.deepcopy(input_places)
        self.transitions =\
            copy.deepcopy(input_transitions)

    def print_places(self):
        for place in self.places:
            place.print_places()

    def get_total_places(self):
        return len(self.places)

    def get_total_transitions(self):
        return len(self.transitions)



    def print_transition(self):
        for transition in self.transitions:
            transition.print_transition()
# Ogni Place e' salvato con il suo timestamp, ativity, id_model, action, n_action, puo' essere identificato usando
# la coppia action e n_action


class Place:
    def __init__(self, activity_one, activity_two):
        self.activity_one = activity_one
        self.activity_two = activity_two
        self.name = self.activity_one.action

    def print_places(self):
        print("Place: ")
        self.activity_one.print_activity()
        self.activity_two.print_activity()


class Activity:
    def __init__(self, timestamp, activity, id_model, id_exec, action, n_action):
        self.timestamp = timestamp
        self.activity = activity
        self.id_model = id_model
        self.id_exec = id_exec
        self.action = action
        self.n_action = n_action

    def print_activity(self):
        print("Activity: " + str(self.timestamp) + " " + str(self.activity) + " " + str(self.id_model) + " " +
              str(self.id_exec) + " " + str(self.action) + " " + str(self.n_action))


# Salvaimo le transizioni usando index_process(indice del processo corrente),
# index_places_from(indice del place di partenza), index_places_to(indice del place di arrivo)


class Transitions:
    def __init__(self, index_process, index_places_from, index_places_to):
        self.index_process = index_process
        self.index_places_from = index_places_from
        self.index_places_to = index_places_to

    def print_transition(self):
        print("Transitions:", self.index_process, self.index_places_from, self.index_places_to)

# Uso variabili globali per gestire i dati


processes = []
places = []
activitys = []
transitions = []

graph = graphviz.Digraph()

def read_data_csv():
    global places
    global processes
    global transitions
    global activitys
    activitys_two = []
    folder_path = 'dataset/'
    file_names = get_file_names_in_folder(folder_path)
    print(file_names)
    for file in file_names:
        print("faccio file:" + file)
        with open(folder_path + file) as csv_file:
            csv_reader = csv.reader(csv_file, delimiter=',')
            line_count = 0
            index_process = 0
            for index, row in enumerate(csv_reader):
                if line_count == 0:
                    line_count += 1
                else:
                    if row[1] == "end_of_process":  # Se un processo e' finito salvo il suo set di azioni e transizioni e resetto tutto
                        processes.append(Processes(copy.deepcopy(places), copy.deepcopy(transitions)))
                        index_process += 1
                        places = []
                        transitions = []
                    else:
                        if row[1] == "end_of_activity" and len(places) > 1: # quando finisce un'activity, creo un place e ci sono piu' place gia' salvati, creo la transizione con quello precendente
                            transitions.append(Transitions(index_process, len(places)-2, len(places)-1))  # Creo nuova transizione e deepcopy nell'array
                            activity = Activity(row[0], row[1], row[2], row[3], row[4], row[5])
                            activitys.append(copy.deepcopy(activity))
                            places.append(Place(activitys[0], activitys[1]))
                            activitys = []
                        elif row[1] == "end_of_activity": # Activity finita, la leggo e passo all'altra con cui creero' un place
                            activity = Activity(row[0], row[1], row[2], row[3], row[4], row[5])
                            activitys.append(copy.deepcopy(activity))
                            places.append(Place(activitys[0], activitys[1]))
                            activitys = []
                        elif row[1] == "begin_of_activity":  # begin_of_activity sta ad indicare quando leggo una nuova activity
                            if len(activitys) > 0:
                                print("faccio parallelo")
                                activity_two = Activity(row[0], row[1], row[2],
                                                        row[3], row[4], row[5])
                                activitys_two.append(copy.deepcopy(activity_two))
                                next_item = next(csv_reader)
                                activity_two = Activity(next_item[0], next_item[1],
                                                        next_item[2], next_item[3],
                                                        next_item[4], next_item[5])
                                index_process += 1
                                activitys_two.append(copy.deepcopy(activity_two))
                                places.append(Place(activitys_two[0], activitys_two[1]))
                                transitions.append(Transitions(index_process, len(places) - 2, len(places) - 1))
                                activitys_two = []
                                next_item = next(csv_reader)
                                activity = Activity(next_item[0], next_item[1],
                                                    next_item[2], next_item[3],
                                                    next_item[4], next_item[5])
                                activitys.append(copy.deepcopy(activity))
                                index_process += 1
                                places.append(Place(activitys[0], activitys[1]))
                                transitions.append(Transitions(index_process, len(places) - 3, len(places) - 1))
                                activitys = []
                            else:
                                activity = Activity(row[0], row[1], row[2],
                                                    row[3], row[4], row[5])
                                activitys.append(copy.deepcopy(activity))
                    line_count += 1



def does_transition_exist(net, transition_id):
    for transition in net.findall("transition"):
        if transition.get("id") == transition_id:
            return True
    return False


def convert_data_to_petri():
    # Creo la struttura PNML
    pnml = ET.Element("pnml")
    # Creo l'elemento net
    net = ET.SubElement(pnml, "net")
    z = 0
    for proc in processes:
        print("ripetizione" + str(z))
        p_net_places = []
        p_net_transitions = []
        for i in range(0, len(proc.places)):
            if i == len(proc.places):
                break
            else:
                p_net_places.append(proc.places[i].activity_one.id_model + "_" + proc.places[i].activity_one.id_exec + "_" +
                                    proc.places[i].name + "_" + proc.places[i].activity_one.n_action)
                p_net_transitions.append(proc.places[i].name)
        t_1 = 0
        for i in range(0, len(p_net_places)):
            place1 = ET.SubElement(net, "place", id=p_net_places[i])
            if not does_transition_exist(net, p_net_transitions[i]):
                t_1 = ET.SubElement(net, "transition", id=p_net_transitions[i])
            arc1 = ET.SubElement(net, "arc", id=p_net_transitions[i]+p_net_transitions[i], source=p_net_places[i], target=p_net_transitions[i])
            if i > 0:
                if p_net_places[i] == "idmodel_699333finale_idexec_69933320200411151148239_act_ecfilesave_36":
                    arc1 = ET.SubElement(net, "arc", id=p_net_transitions[i - 1] + p_net_places[i],
                                         source=p_net_transitions[i - 2],
                                         target=p_net_places[i])
                else:
                    if p_net_places[i] == "idmodel_699333finale_idexec_69933320200411151148239_act_MoveCaretCommand_224":
                        arc1 = ET.SubElement(net, "arc", id=p_net_transitions[i - 1] + p_net_places[i],
                                             source=p_net_transitions[i - 1],
                                             target=p_net_places[i])
                        arc1 = ET.SubElement(net, "arc", id=p_net_transitions[i - 1] + p_net_places[i],
                                             source=p_net_transitions[i - 2],
                                             target=p_net_places[i])
                    else:
                        arc1 = ET.SubElement(net, "arc", id=p_net_transitions[i - 1] + p_net_places[i], source=p_net_transitions[i - 1],
                                             target=p_net_places[i])
        z += 1
    place1 = ET.SubElement(net, "place", id="end")
    arc1 = ET.SubElement(net, "arc", id=p_net_transitions[i - 1] + p_net_places[i], source=p_net_transitions[-1],
                         target="end")
    tree = ET.ElementTree(pnml)
    ET.indent(tree)
    tree.write("petri_net.xml ", encoding="utf-8", xml_declaration=True)
    for filename in glob.iglob(os.path.join("", '*.xml')):
        os.rename(filename, filename[:-4] + '.pnml')
    print("finito")


if __name__ == '__main__':
    read_data_csv()
    convert_data_to_petri()

