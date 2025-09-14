#### Outline of Code
PFAS-Code/Pub/Revision1/head_ex_r1.R


- PFAS-Code/Pub/Revision2/Bootstrap/bootstrap_iv.R: This bootstraps the standard errors for the two new IV specs (no domestic wells and no time trend in the first stage).
- PFAS-Code/Pub/Revision2/revision2.R: This reruns the main analysis and calls the following files:
    - PFAS-Code/Pub/Revision2/Figures/pws_fe.R: This creates Figure R2
    - PFAS-Code/Pub/Revision2/Tables/tables.R: This creates all the tables in the second round of revisions
    - PFAS-Code/Pub/Revision2/Figures/figure2_simple.R: This recreates Figure 2 from the main text after removing the moderately and very rows of preterm and low birthweight
    - PFAS-Code/Pub/Revision2/Figures/include_missing.R: This creates Figure R1 from the review response
    - PFAS-Code/Pub/Revision2/Tables/binary_sys_table.R: This creates Table R2 from the review response
- PFAS-Code/Pub/Revision2/Surface Water/surface_water.R: This creates Table R1 from the review response
