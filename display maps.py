import pandas as pd
pd.set_option('display.width', 1000)
import sqlite3
import os
os.chdir('/')

"""
Displays shapefiles on a simple CONUS basemap.  Maps are plotted in the order
provided so put the top map last in the listself.

NOTE: The shapefiles have to be in WGS84 CRS.

(list) -> displays maps

Arguments:
map_these = list of paths for shapefiles you want to display in CONUS.

Example:
>>DisplayShapefiles(['/users/nmtarr/documents/ranges/ybcu_circles'])

"""

mapfile = '/users/nmtarr/documents/ranges/ybcu_circles'

# Packages needed for plotting
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap
import numpy as np
from matplotlib.patches import Polygon
from matplotlib.collections import PatchCollection
from matplotlib.patches import PathPatch

# Basemap
fig = plt.figure(figsize=(12,8))
ax  = fig.add_subplot(111)
map = Basemap(projection='aea', resolution='i', lon_0=-95.5, lat_0=39.5,
              height=3400000, width=5000000)
map.drawcoastlines(color='grey')
map.drawstates(color='grey')
map.drawcountries(color='grey')
map.fillcontinents(color='green',lake_color='aqua')
map.drawmapboundary(fill_color='aqua')

# Add shapefiles to the map
map.readshapefile(mapfile, 'mapfile', drawbounds=False)

patches = []

for info, shape in zip(map.mapfile_info, map.mapfile):
    patches.append(Polygon(np.array(shape), True))

ax.add_collection(PatchCollection(patches, facecolor= 'y', edgecolor='y',
                                  linewidths=1., zorder=2))
plt.show()
