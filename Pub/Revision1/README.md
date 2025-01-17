#### Outline of Code
PFAS-Code/Pub/Revision1/head_ex_r1.R


- PFAS-Code/Pub/GIS/cont_watershed.R
	- This sets ppt to 500 and creates the watershed shapes for each contamination site
- PFAS-Code/Pub/Revision1/Bootstrap/bootstrap_iv.R
	- This runs the bootstrap algorithm for the continuous birth outcomes and the comparison of model 2 and model 1
- PFAS-Code/Pub/Revision1/Bootstrap/ppt500/bootstrap_iv.R
	- This runs the bootstrap for all IV outcomes with a cutoff of 500 ppt for a contaminated site
- PFAS-Code/Pub/Revision1/Robustness/drop_near_state/drop_near_state_head.R
	- This runs the robustness exercise where we drop all sites within 5km of a state border, for the logit specification
- PFAS-Code/Pub/Revision1/Robustness/relaxed_up/relaxed_up_head.R
	- This runs the robustness exercise where we use a relaxed upgradient specification: we only compare downgradient to non, for the logit specification
- PFAS-Code/Pub/Revision1/revision1_head.R
	- This creates most of the Revision tables and figures:
		- Tables R1, R2, R10, R6, R11, R12, R14, R15, R4 (and IV table from main text)
		- Figures R1, R2, R3, R7, R8, R9, R10, R11, R12 (and figure 3a, figure 2, the upgradient version of figure 2 and the birthweight amongst full gestation figures from the main text, with the estimation sample as the mean effect)
- PFAS-Code/Pub/Revision1/Tables/balance_sw_gw_dw_est.R
	- This creates Tables R7 and R8
- PFAS-Code/Pub/Revision1/Figures/change_thresh.R
	- This creates Table R9 and Figure R4
- PFAS-Code/Pub/Revision1/ppt500/iv_nh.R
	- This saves the standard errors for model 2 with a threshold of 500ppt and creates Figure R6a
- PFAS-Code/Pub/Revision1/Figures/Figure 2/iv_500_1000.R
	- This creates Figure R5  
- PFAS-Code/Pub/Revision1/national_costs_head500.R
	- This creates Figure R6b-d
- PFAS-Code/Pub/Revision1/Placebo/placebo_head.R
	- This creates Tables R3 and R5