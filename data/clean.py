import json
import csv

def csv_to_json(csv_file_path, json_file_path):
    """
    Converts a CSV file to a JSON file.

    Args:
      csv_file_path: Path to the CSV file.
      json_file_path: Path to the output JSON file.

    This function reads a CSV file, converts each row into a dictionary, 
    and then writes the list of dictionaries to a JSON file. It handles 
    potential errors during file reading and CSV parsing.
    """
    try:
        with open(csv_file_path, 'r', encoding='utf-8') as csv_file:
            # Using csv.DictReader to read CSV data as dictionaries
            reader = csv.DictReader(csv_file)
            data = list(reader)

        with open(json_file_path, 'w', encoding='utf-8') as json_file:
            # Writing the list of dictionaries to a JSON file with indentation
            json.dump(data, json_file, indent=4)

    except FileNotFoundError:
        print(f"Error: CSV file not found at {csv_file_path}")
    except csv.Error as e:
        print(f"Error parsing CSV file: {e}")

# Example usage
if __name__ == "__main__":
    csv_file_path = '/Users/dayan/Code/SpecteralConflict/build2.0/data/2022-01-01-2023-01-01-Sri_Lanka.csv'
    json_file_path = 'output.json'
    csv_to_json(csv_file_path, json_file_path)
    print(f"CSV data converted to JSON and saved to {json_file_path}"
          
import pandas as pd
import numpy as np

# Parse all interactions
raw_data = """Rioters (Sri Lanka)\tGovernment of Sri Lanka (2019-); SJB: Samagi Jana Balavegaya
Rioters (Sri Lanka)\tLabor Group (Sri Lanka)
Protesters (Sri Lanka)\tSLPP: Sri Lanka People's Front
Rioters (Sri Lanka)\tGovernment of Sri Lanka (2019-)
Rioters (Sri Lanka)\tSLPP: Sri Lanka People's Front; Former Government of Sri Lanka (2019-); Government of Sri Lanka (2019-); SLPFA: Sri Lanka People's Freedom Alliance
Rioters (Sri Lanka)\tGovernment of Sri Lanka (2019-); SLPP: Sri Lanka People's Front
Protesters (Sri Lanka)\tSLPP: Sri Lanka People's Front
Rioters (Sri Lanka)\tLabor Group (Sri Lanka); SLPFA: Sri Lanka People's Freedom Alliance
Protesters (Sri Lanka)\tGovernment of Sri Lanka (2019-); SLPFA: Sri Lanka People's Freedom Alliance
Protesters (Sri Lanka)\tLabor Group (Sri Lanka); Women (Sri Lanka); Teachers (Sri Lanka)
Protesters (Sri Lanka)\tTNA: Tamil National Alliance; Government of Sri Lanka (2019-)
Protesters (Sri Lanka)\tBuddhist Group (Sri Lanka); Christian Group (Sri Lanka); IUSF: Inter University Students' Federation; Muslim Group (Sri Lanka); Students (Sri Lanka)
Protesters (Sri Lanka)\tWomen (Sri Lanka); SLFP: Sri Lanka Freedom Party
Protesters (Sri Lanka)\tACMC: All Ceylon Makkal Congress; CPSL: Communist Party of Sri Lanka; DLF: Democratic Left Front; JVP: Janatha Vimukthi Peramuna; Labor Group (Sri Lanka); LSSP: Lanka Sama Samaja Party; SJB: Samagi Jana Balavegaya; SLMC: Sri Lanka Muslim Congress; TPA: Tamil Progressive Alliance
Protesters (Sri Lanka)\tStudents (Sri Lanka)
Protesters (Sri Lanka)\tTeachers (Sri Lanka); Labor Group (Sri Lanka)
Protesters (Sri Lanka)\tUNP: United National Party
Protesters (Sri Lanka); Labor Group (Sri Lanka)\tNone
Protesters (Sri Lanka)\tLabor Group (Sri Lanka); Farmers (Sri Lanka)
Protesters (Sri Lanka)\tSJB: Samagi Jana Balavegaya; UPFA: United People's Freedom Alliance; Government of Sri Lanka (2019-)
Protesters (Sri Lanka)\tGovernment of Sri Lanka (2019-); Former Government of Sri Lanka (2019-)
Protesters (Sri Lanka)\tBuddhist Group (Sri Lanka); Christian Group (Sri Lanka); Muslim Group (Sri Lanka)
Protesters (Sri Lanka)\tLabor Group (Sri Lanka)
Protesters (Sri Lanka)\tHealth Workers (Sri Lanka); Teachers (Sri Lanka)
Protesters (Sri Lanka)\tJournalists (Sri Lanka)
Government of Sri Lanka (2019-); SLPP: Sri Lanka People's Front\tStudents (Sri Lanka)
Rioters (Sri Lanka)\tSLPP: Sri Lanka People's Front
Government of Sri Lanka (2019-); UPFA: United People's Freedom Alliance\tNone
UNP: United National Party\tGovernment of Sri Lanka (2019-); SLPP: Sri Lanka People's Front
Protesters (Sri Lanka)\tHealth Workers (Sri Lanka); Government of Sri Lanka (2019-); Labor Group (Sri Lanka)
Protesters (Sri Lanka)\tUPFA: United People's Freedom Alliance
Protesters (Sri Lanka)\tTPA: Tamil Progressive Alliance; Government of Sri Lanka (2019-)
Protesters (Sri Lanka)\tCatholic Christian Group (Sri Lanka); Health Workers (Sri Lanka); Lawyers (Sri Lanka)
Protesters (Sri Lanka)\tJournalists (Sri Lanka); Labor Group (Sri Lanka)
Protesters (Sri Lanka)\tProtesters (International)
Protesters (Sri Lanka)\tGovernment of Sri Lanka (2019-); SJB: Samagi Jana Balavegaya; UPFA: United People's Freedom Alliance
Protesters (Sri Lanka)\tGovernment of Sri Lanka (2019-); JVP: Janatha Vimukthi Peramuna
Protesters (Sri Lanka)\tNDMLP: New-Democratic Marxist-Leninist Party
Civilians (Sri Lanka); Journalists (Sri Lanka); Police Forces of Sri Lanka (2019-) Special Task Force\tNone
Protesters (Sri Lanka)\tSinhala Ravay; Sinhalese Ethnic Group (Sri Lanka)
Protesters (Sri Lanka)\tTNPF: Tamil National People's Front
Rioters (Sri Lanka)\tSJB: Samagi Jana Balavegaya
Police Forces of Sri Lanka (2019-)\tTNPF: Tamil National People's Front; Government of Sri Lanka (2019-)
Police Forces of Sri Lanka (2019-)\tWomen (Sri Lanka)
Protesters (Sri Lanka)\tHealth Workers (Sri Lanka); Students (Sri Lanka); Women (Sri Lanka)
Protesters (Sri Lanka)\tTamil Ethnic Group (Sri Lanka); Former Government of Sri Lanka (2019-)
Protesters (Sri Lanka)\tTamil Ethnic Group (Sri Lanka); Government of Sri Lanka (2019-); TNPF: Tamil National People's Front
Protesters (Sri Lanka)\tJournalists (Sri Lanka); Government of Sri Lanka (2019-); Former Government of Sri Lanka (2019-); TNA: Tamil National Alliance; TNPF: Tamil National People's Front
Protesters (Sri Lanka)\tTNA: Tamil National Alliance; Government of Sri Lanka (2019-); TNPF: Tamil National People's Front
Protesters (Sri Lanka)\tGovernment of Sri Lanka (2019-); TNA: Tamil National Alliance
Protesters (Sri Lanka)\tJVP: Janatha Vimukthi Peramuna; Government of Sri Lanka (2019-)
Police Forces of Sri Lanka (2019-)\tTamil Ethnic Group (Sri Lanka)
Protesters (Sri Lanka)\tTNA: Tamil National Alliance
Protesters (Sri Lanka)\tTamil Ethnic Group (Sri Lanka); Government of Sri Lanka (2019-); ITAK: Illankai Tamil Arasu Kachchi; Former Government of Sri Lanka (2019-); Students (Sri Lanka)
Protesters (Sri Lanka)\tTeachers (Sri Lanka); Students (Sri Lanka); Women (Sri Lanka)
Protesters (Sri Lanka)\tFishers (Sri Lanka); Government of Sri Lanka (2019-); Tamil Ethnic Group (Sri Lanka); TNA: Tamil National Alliance
Protesters (Sri Lanka)\tTamil Ethnic Group (Sri Lanka); Fishers (Sri Lanka); TNPF: Tamil National People's Front
Protesters (Sri Lanka)\tITAK: Illankai Tamil Arasu Kachchi; Government of Sri Lanka (2019-); Women (Sri Lanka)
Fishers (Sri Lanka); Military Forces of Sri Lanka (2019-)\tNone
Civilians (Sri Lanka)\tNone
Labor Group (Sri Lanka)\tGovernment of Sri Lanka (2019-)
Rioters (Sri Lanka)\tHenawatta Communal Group (Sri Lanka)
Protesters (Sri Lanka)\tLabor Group (Sri Lanka); UPFA: United People's Freedom Alliance; Government of Sri Lanka (2019-)
Protesters (Sri Lanka)\tNDMLP: New-Democratic Marxist-Leninist Party; TNA: Tamil National Alliance; Teachers (Sri Lanka)
Protesters (Sri Lanka)\tTNPF: Tamil National People's Front; Tamil Ethnic Group (Sri Lanka)
Unidentified Armed Group (Sri Lanka); √\tNone
Protesters (Sri Lanka)\tFormer Government of Sri Lanka (2019-); Government of Sri Lanka (2019-); SJB: Samagi Jana Balavegaya"""

# Split into interactions
interactions = [line.split('\t') for line in raw_data.split('\n')]

# Extract unique actors
actors = set()
for source, targets in interactions:
    if source != "None":
        for src in source.split('; '):
            actors.add(src)
    if targets != "None":
        for target in targets.split('; '):
            actors.add(target)

actors = sorted(list(actors))

# Create adjacency dictionary
adj_dict = {}
for actor in actors:
    adj_dict[actor] = {other: 0 for other in actors}

# Fill dictionary
for source, targets in interactions:
    if source != "None" and targets != "None":
        source_actors = source.split('; ')
        target_actors = targets.split('; ')
        
        for src in source_actors:
            for tgt in target_actors:
                if src in adj_dict and tgt in adj_dict:
                    adj_dict[src][tgt] += 1

# Create summary statistics
total_interactions = {actor: sum(connections.values()) for actor, connections in adj_dict.items()}
top_actors = dict(sorted(total_interactions.items(), key=lambda x: x[1], reverse=True)[:5])

# Create final output dictionary
output = {
    "adjacency_matrix": adj_dict,
    "statistics": {
        "total_actors": len(actors),
        "total_interactions": sum(total_interactions.values()),
        "most_active_actors": top_actors
    },
    "actor_list": actors
}

# Print the output
print(output))