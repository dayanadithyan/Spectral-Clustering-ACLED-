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
    print(f"CSV data converted to JSON and saved to {json_file_path}")