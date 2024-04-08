import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from statsmodels.tsa.seasonal import seasonal_decompose
import networkx as nx
from sklearn.cluster import SpectralClustering
import plotly.graph_objects as go

class FolderManager:
    def __init__(self, path):
        self.path = path

def createFolders(object):
    for folder in object.path:
        if not os.path.exists(folder):
            os.makedirs(folder)
            print("Created folder:", folder)
        else:
            print("Folder already exists:", folder)

def readData(file):
    return pd.read_csv(file)

def cleanDateFormat(date):
    return pd.to_datetime(date).dt.strftime("%d-%B")

def removeWhitespace(data):
    return data.apply(lambda x: x.str.replace(" ", ""))

def performInterruptedTimeSeriesAnalysis(data):
    # Perform seasonal decomposition
    result = seasonal_decompose(data, model='additive', period=12)
    trend = result.trend
    seasonal = result.seasonal
    residual = result.resid
    
    # Plot decomposition
    plt.figure(figsize=(12, 8))
    plt.subplot(411)
    plt.plot(data, label='Original')
    plt.legend(loc='upper left')
    plt.subplot(412)
    plt.plot(trend, label='Trend')
    plt.legend(loc='upper left')
    plt.subplot(413)
    plt.plot(seasonal, label='Seasonality')
    plt.legend(loc='upper left')
    plt.subplot(414)
    plt.plot(residual, label='Residuals')
    plt.legend(loc='upper left')
    plt.show()

def performSpectralGraphAnalysis(data):
    # Construct graph
    G = nx.from_pandas_edgelist(data, source='source', target='target')
    
    # Compute Laplacian matrix
    L = nx.normalized_laplacian_matrix(G)
    eigenvalues, eigenvectors = np.linalg.eigh(L.A)
    
    # Perform eigen clustering
    n_clusters = 3
    clustering = SpectralClustering(n_clusters=n_clusters, eigen_solver='arpack', affinity='nearest_neighbors')
    clustering.fit(eigenvectors[:, 1:n_clusters])
    cluster_labels = clustering.labels_
    
    # Visualize in 3D
    pos = nx.spring_layout(G, dim=3)
    edge_x = []
    edge_y = []
    edge_z = []
    for edge in G.edges():
        x0, y0, z0 = pos[edge[0]]
        x1, y1, z1 = pos[edge[1]]
        edge_x.append(x0)
        edge_x.append(x1)
        edge_x.append(None)
        edge_y.append(y0)
        edge_y.append(y1)
        edge_y.append(None)
        edge_z.append(z0)
        edge_z.append(z1)
        edge_z.append(None)
    
    edge_trace = go.Scatter3d(
        x=edge_x, y=edge_y, z=edge_z,
        line=dict(width=0.5, color='#888'),
        hoverinfo='none',
        mode='lines')
    
    node_x = []
    node_y = []
    node_z = []
    for node in G.nodes():
        x, y, z = pos[node]
        node_x.append(x)
        node_y.append(y)
        node_z.append(z)
    
    node_trace = go.Scatter3d(
        x=node_x, y=node_y, z=node_z,
        mode='markers',
        hoverinfo='text',
        marker=dict(
            showscale=True,
            colorscale='YlGnBu',
            color=cluster_labels,
            size=10,
            colorbar=dict(
                thickness=15,
                title='Cluster',
                xanchor='left',
                titleside='right'
            ),
            line_width=2))
    
    fig = go.Figure(data=[edge_trace, node_trace],
                    layout=go.Layout(
                        title='3D Network Visualization with Eigen Clustering',
                        titlefont_size=16,
                        showlegend=False,
                        hovermode='closest',
                        margin=dict(b=20, l=5, r=5, t=40),
                        annotations=[dict(
                            text="",
                            showarrow=False,
                            xref="paper", yref="paper",
                            x=0.005, y=-0.002)],
                        xaxis=dict(showgrid=False, zeroline=False, showticklabels=False),
                        yaxis=dict(showgrid=False, zeroline=False, showticklabels=False),
                        scene=dict(
                            xaxis=dict(visible=False),
                            yaxis=dict(visible=False),
                            zaxis=dict(visible=False)
                        )
                    ))

    fig.show()

def main():
    # Setup folders
    folders = ["1.Raw.Data", "2.Clean.Data", "3.Analysis", "4.Graphs", "5.Tables"]
    fm = FolderManager(folders)
    createFolders(fm)

    # Read Data
    main_raw = readData("ACLEDRAWSL.csv")
    raw2 = readData("ACLEDRAWSL.csv")
    raw3 = readData("ACLEDRAWSL.csv")

    # Clean Data
    raw3["date"] = cleanDateFormat(raw3["event_date"])
    raw3["event_date"] = cleanDateFormat(raw3["event_date"])
    spaceless_table = removeWhitespace(main_raw)

    # Time Series Analysis
    performInterruptedTimeSeriesAnalysis(main_raw)

    # Network Analysis
    performSpectralGraphAnalysis(raw2)

# Execute main script
main()
