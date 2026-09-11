# Phenotype codelists

The preparation script expects three public phenotype codelists from the
[ECHILD Phenotype Code List Repository](https://code.echild.ac.uk/) to be
available in this directory inside the SRS:

| Local filename | Phenotype | Repository page |
|---|---|---|
| `chc_hardelid_v1.csv` | Chronic health conditions (Hardelid), v1 | <https://code.echild.ac.uk/chc_hardelid_v1> |
| `ari_herbert_v1.csv` | Adversity-related injuries (Herbert), v1 | <https://code.echild.ac.uk/ari_herbert_v1> |
| `srp_nichobhthaigh_v2.csv` | Stress-related presentations (Ní Chobhthaigh), v2 | <https://code.echild.ac.uk/srp_nichobhthaigh_v2> |

The CSVs are not duplicated here at this stage. This keeps the analytical
repository tied to the maintained phenotype source and makes versioning
explicit. In an air-gapped SRS workflow, obtain the required files outside the
SRS and transfer them using the current approved mechanism before running
`R/01_prepare_dataset.R`.
