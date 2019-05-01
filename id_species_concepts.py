"""
Description: Species-concepts change over time, sometimes with a spatial
component (e.g., changes in range delination of closely related species or
subspecies).  Retrieval of data for the wrong species-concept would introduce
error.  Therefore, the first step is to sort out species concepts of different
datasets to identify concepts that can be investigated.

For this project/effort, individual species-concepts will be identified,
crosswalked to concepts from various datasets, and stored in a table within
a database.

For now, a single species has been manually entered into species-concepts
for development.
"""
from pygbif import species
key0 = species.name_backbone(name="Drymarchon couperi", rank='species')
key = species.name_backbone(name = 'Ursus americanus', rank='species')['usageKey']
occurrences.search(taxonKey = key)
