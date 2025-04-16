

# ORIGINAL ----------------------------------------------------------------

ENMTML (pred_dir = "./P", 
        occ_file = "ocorrencias.txt", 
        sp = "sp",
        x = "Longitude",
        y = "Latitude",
        min_occ = 50,
        thin_occ = NULL,
        part = c(method = "BOOT", replicates = '1', proportion = "0.7"),
        colin_var = ,
        sp_accessible_area = c(method = 'BUFFER', type = '1'),
        pseudoabs_method = c(method= 'GEO_ENV_CONST', width= '50'),
        pres_abs_ratio = 1,
        algorithm = "MXS",
        thr = c(type = "MAX_TSS"),
        msdm = c(method = 'OBR'),
        ensemble = c(method= 'SUP', metric= 'TSS'))

