import pm4py as pm4
import pandas as pd

list_data_677179 = []

data = pd.read_csv("user_data.csv")
for d in data.values:

    di_list = []
    j = 0

    for i in d:

        if j == 0:
            i = i.replace("entry(", "")

        if j == 5:
            i = i.replace(").", "")

        i = " ".join(i.split())
        di_list.append(i)

        j = j + 1

    list_data_677179.append(di_list)

df = pd.DataFrame(list_data_677179, columns=[
                  "timestamp", "activity", "id_model", "id_exec", "action", "n_action"])
df.to_csv("interaction_data_677179.csv", index=False)
print(df.head())
#######################################################################################################################################
print("lista 1--------------------------------------------------------------------------------------------------------------------------")

list_data_699333 = []

data = pd.read_csv("user_data.csv")
for d in data.values:

    di_list = []
    j = 0

    for i in d:

        if j == 0:
            i = i.replace("entry(", "")

        if j == 5:
            i = i.replace(").", "")

        i = " ".join(i.split())
        di_list.append(i)

        j = j + 1

    list_data_699333.append(di_list)

df = pd.DataFrame(list_data_699333, columns=[
                  "timestamp", "activity", "id_model", "id_exec", "action", "n_action"])
df.to_csv("interaction_data_699333.csv", index=False)
print(df.head())

#######################################################################################################################################
print("lista 2--------------------------------------------------------------------------------------------------------------------------")
list_data_700655 = []

data = pd.read_csv("user_data.csv")
for d in data.values:

    di_list = []
    j = 0

    for i in d:

        if j == 0:
            i = i.replace("entry(", "")

        if j == 5:
            i = i.replace(").", "")

        i = " ".join(i.split())
        di_list.append(i)

        j = j + 1

    list_data_700655.append(di_list)

df = pd.DataFrame(list_data_700655, columns=[
                  "timestamp", "activity", "id_model", "id_exec", "action", "n_action"])
df.to_csv("interaction_data_700655.csv", index=False)
print(df.head())

#######################################################################################################################################
print("lista 3--------------------------------------------------------------------------------------------------------------------------")
list_data_703453 = []

data = pd.read_csv("user_data.csv")
for d in data.values:

    di_list = []
    j = 0

    for i in d:

        if j == 0:
            i = i.replace("entry(", "")

        if j == 5:
            i = i.replace(").", "")

        i = " ".join(i.split())
        di_list.append(i)

        j = j + 1

    list_data_703453.append(di_list)

df = pd.DataFrame(list_data_703453, columns=[
                  "timestamp", "activity", "id_model", "id_exec", "action", "n_action"])
df.to_csv("interaction_data_703453.csv", index=False)
print(df.head())

#######################################################################################################################################
print("lista 4--------------------------------------------------------------------------------------------------------------------------")
list_data_704985 = []

data = pd.read_csv("user_data.csv")
for d in data.values:

    di_list = []
    j = 0

    for i in d:

        if j == 0:
            i = i.replace("entry(", "")

        if j == 5:
            i = i.replace(").", "")

        i = " ".join(i.split())
        di_list.append(i)

        j = j + 1

    list_data_704985.append(di_list)

df = pd.DataFrame(list_data_704985, columns=[
                  "timestamp", "activity", "id_model", "id_exec", "action", "n_action"])
df.to_csv("interaction_data_704985.csv", index=False)
print(df.head())

#######################################################################################################################################
print("lista 5--------------------------------------------------------------------------------------------------------------------------")
list_data_706218 = []

data = pd.read_csv("user_data.csv")
for d in data.values:

    di_list = []
    j = 0

    for i in d:

        if j == 0:
            i = i.replace("entry(", "")

        if j == 5:
            i = i.replace(").", "")

        i = " ".join(i.split())
        di_list.append(i)

        j = j + 1

    list_data_706218.append(di_list)

df = pd.DataFrame(list_data_706218, columns=[
                  "timestamp", "activity", "id_model", "id_exec", "action", "n_action"])
df.to_csv("interaction_data_706218.csv", index=False)
print(df.head())
