#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Dec 19 20:52:42 2018

@author: nmtarr

Description:  A place to store variables and functions that are the same
throughout the repo.

!!!!!!!!!!!!!!!!!!!!! CURRENTLY INACTIVE
"""
species = 'Coccyzus americanus'
sp_id = 'bYBCUx0'
sp_gbif_key = 2496287
sp_TSN = 177831
workDir = 'Users/nmtarr/Documents/RANGES'

# Define a function for displaying the maps that will be created.
def MapShapefilePolygons(map_these, title):
    """
    Displays shapefiles on a simple CONUS basemap.  Maps are plotted in the order
    provided so put the top map last in the listself.

    NOTE: The shapefiles have to be in WGS84 CRS.

    (dict, str) -> displays maps, returns matplotlib.pyplot figure

    Arguments:
    map_these -- list of dictionaries for shapefiles you want to display in
                CONUS. Each dictionary should have the following format:
                    {'file': '/path/to/your/shapfile',
                    'linecolor': 'k',
                    'fillcolor': 'k',
                    'linewidth': 1,
                    'drawbounds': True}
    title -- title for the map.
    """
    # Packages needed for plotting
    import matplotlib.pyplot as plt
    from mpl_toolkits.basemap import Basemap
    import numpy as np
    from matplotlib.patches import Polygon
    from matplotlib.collections import PatchCollection
    from matplotlib.patches import PathPatch

    # Basemap
    fig = plt.figure(figsize=(12,8))
    ax = plt.subplot(1,1,1)
    map = Basemap(projection='aea', resolution='c', lon_0=-95.5, lat_0=39.5,
                  height=3400000, width=5000000)
    map.drawcoastlines(color='grey')
    map.drawstates(color='grey')
    map.drawcountries(color='grey')
    map.fillcontinents(color='green',lake_color='aqua')
    map.drawmapboundary(fill_color='aqua')

    for mapfile in map_these:
        # Add shapefiles to the map
        if mapfile['fillcolor'] == None:
            map.readshapefile(mapfile['file'], 'mapfile',
                              drawbounds=mapfile['drawbounds'],
                              linewidth=mapfile['linewidth'],
                              color=mapfile['linecolor'])
        else:
            map.readshapefile(mapfile['file'], 'mapfile',
                      drawbounds=mapfile['drawbounds'])
            # Code for extra formatting -- filling in polygons setting border
            # color
            patches = []
            for info, shape in zip(map.mapfile_info, map.mapfile):
                patches.append(Polygon(np.array(shape), True))
            ax.add_collection(PatchCollection(patches,
                                              facecolor= mapfile['fillcolor'],
                                              edgecolor=mapfile['linecolor'],
                                              linewidths=mapfile['linewidth'],
                                              zorder=2))

    #    # Make a legend
    #    handles, labels = plt.gca().get_legend_handles_labels()
    #    handles.extend(['mapfile'])
    #    labels.extend(["mapfile"])
    #    plt.legend(handles=handles, labels=labels)

    fig.suptitle(title, fontsize=20)
    return fig
