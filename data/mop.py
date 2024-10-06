import json
import numpy as np
from scipy.sparse import csr_matrix
from scipy.linalg import eig
import networkx as nx

def analyze_protest_data(data):
    """
    Analyzes protest and riot data to identify key actors and their interactions.

    Args:
      data: A list of dictionaries, where each dictionary represents a protest event.

    Returns:
      A tuple containing:
        - actor_mapping: A dictionary mapping actor names to their encoded indices.
        - incidence_matrix: A NumPy matrix representing the interactions between actors.
        - degree_matrix: A NumPy matrix representing the degree of each actor.
        - pagerank_scores: A dictionary of PageRank scores for each actor.
        - eigenvalues: A NumPy array of eigenvalues from the eigenanalysis.
        - eigenvectors: A NumPy array of eigenvectors from the eigenanalysis.
    """

    # 1. Filter for protests and riots
    filtered_data = [
        event for event in data
        if event["event_type"] in ("Protests", "Riots")
    ]

    # 2. Extract relevant data
    actors = []
    interactions = []
    for event in filtered_data:
        actors.extend(
            [
                event["actor1"],
                event["assoc_actor_1"],
                event["actor2"],
                event["assoc_actor_2"],
            ]
        )
        interactions.append(
            (
                event["actor1"],
                event["actor2"],
                int(event["inter1"]),
            )
        )

    # 3a. Encode categorical data
    unique_actors = list(set(actors))
    actor_mapping = {actor: i for i, actor in enumerate(unique_actors)}

    # 3b. Create incidence matrix
    rows = []
    cols = []
    data_vals = []
    for actor1, actor2, interaction in interactions:
        if actor1 in actor_mapping and actor2 in actor_mapping:
            rows.append(actor_mapping[actor1])
            cols.append(actor_mapping[actor2])
            data_vals.append(interaction)

    incidence_matrix = csr_matrix(
        (data_vals, (rows, cols)), shape=(len(unique_actors), len(unique_actors))
    ).toarray()

    # 3c. Create degree matrix
    degree_matrix = np.diag(np.sum(incidence_matrix, axis=1))

    # 4. Calculate PageRank scores
    graph = nx.from_numpy_array(incidence_matrix, create_using=nx.DiGraph)
    pagerank_scores = nx.pagerank(graph)

    # 5. Perform eigenanalysis
    eigenvalues, eigenvectors = eig(incidence_matrix)

    # 6. Log, print, and save outputs (example)
    print("Actor Mapping:")
    print(actor_mapping)
    print("\nIncidence Matrix:")
    print(incidence_matrix)
    print("\nDegree Matrix:")
    print(degree_matrix)
    print("\nPageRank Scores:")
    print(pagerank_scores)
    # ... (Add more logging and saving as needed)

    return (
        actor_mapping,
        incidence_matrix,
        degree_matrix,
        pagerank_scores,
        eigenvalues,
        eigenvectors,
    )

# --- Input JSON ---
def load_data_from_json(file_path):
    """
    Loads data from a JSON file.

    Args:
      file_path: Path to the JSON file.

    Returns:
      A list of dictionaries representing the data.
    """
    try:
        with open(file_path, 'r') as f:
            data = json.load(f)
        return data
    except FileNotFoundError:
        print(f"Error: JSON file not found at {file_path}")
        return None
    except json.JSONDecodeError as e:
        print(f"Error decoding JSON: {e}")
        return None

def save_matrix(matrix, filename):
    """Saves a NumPy matrix to a file in .npy format."""
    np.save(filename, matrix)

def save_results(results, filename):
    """Saves a dictionary of results to a JSON file, handling complex numbers."""

    def complex_handler(obj):
        if isinstance(obj, complex):
            return {"real": obj.real, "imag": obj.imag}
        # You can add more custom handlers for other non-serializable types if needed
        else:
            raise TypeError(f"Object of type {type(obj)} is not JSON serializable")

    with open(filename, 'w') as f:
        json.dump(results, f, indent=4, default=complex_handler)

if __name__ == "__main__":
    file_path = '/Users/dayan/Code/SpecteralConflict/output.json'  # Replace with your JSON file path
    data = load_data_from_json(file_path)
    if data:
        (
            actor_mapping,
            incidence_matrix,
            degree_matrix,
            pagerank_scores,
            eigenvalues,
            eigenvectors,
        ) = analyze_protest_data(data)

        # Save matrices
        save_matrix(incidence_matrix, "incidence_matrix.npy")
        save_matrix(degree_matrix, "degree_matrix.npy")

        # Save other results
        results = {
            "actor_mapping": actor_mapping,
            "pagerank_scores": pagerank_scores,
            "eigenvalues": eigenvalues.tolist(),  # Convert to list for JSON serialization
            "eigenvectors": eigenvectors.tolist(),
        }
        save_results(results, "analysis_results.json")