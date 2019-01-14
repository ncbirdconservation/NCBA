#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jan 11 16:29:24 2019

@author: nmtarr

Descripton: Display the shapefiles of the species occurrence that were 
generated with other code.

"""
# Define a function for displaying the maps that will be created.
def MapPolygonsFromSHP(map_these, title):
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

##########################################################

workDir = '/Users/nmtarr/Documents/RANGES'

shp1 = {'file': '/users/nmtarr/documents/ranges/inputs/ybcu_range',
        'drawbounds': False, 'linewidth': .5, 'linecolor': 'y', 
        'fillcolor': 'y'}

shp2 = {'file': '/users/nmtarr/documents/ranges/ybcu_circles',
        'drawbounds': True, 'linewidth': .5, 'linecolor': 'k', 
        'fillcolor': None}

# Display occurrence polygons
MapPolygonsFromSHP(map_these=[shp1, shp2],
                   title='Yellow-billed Cuckoo occurrence polygons - any month')

season_colors = {'Fall': 'red', 'Winter': 'white', 'Summer': 'magenta',
                    'Spring': 'blue'}
for period in ['Fall', 'Winter', 'Summer', 'Spring']: 
     shp1 = {'file': workDir + '/{0}_rng'.format(period),
                    'drawbounds': True, 'linewidth': 1, 
                    'linecolor': season_colors[period], 
                    'fillcolor': None}
     shp2 = {'file': workDir + '/{0}_occs'.format(period),
                    'drawbounds': True, 'linewidth': .5, 'linecolor': 'k', 
                    'fillcolor': None}
     title = "Yellow-billed Cuckoo occurrence polygons - {0}".format(period)  
     try:
         MapPolygonsFromSHP([shp1, shp2], title)
     except:
         print(period + " FAILED !!!!")
