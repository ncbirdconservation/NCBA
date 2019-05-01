#NOTE! this is overwritten by some notebooks, so update everywhere, if adding lines.
sp_id = 'bbcpex0'
summary_name = 'diablotin'
gbif_req_id = 'GBIFr8'
gbif_filter_id = 'GBIFf2'
ebird_req_id = None
ebird_filter_id = None
evaluation = 'eval_gbif1'
workDir = 'T:/RANGES/'
codeDir = 'T:/Scripts/RangeMapEvaluation/'
inDir = workDir + 'Inputs/'
outDir = workDir + 'Outputs/'
default_coordUncertainty = 1000
SRID_dict = {'WGS84': 4326, 'AlbersNAD83': 102008} # Used in file names for output.
spdb = outDir + sp_id + gbif_req_id + gbif_filter_id + '.sqlite'
