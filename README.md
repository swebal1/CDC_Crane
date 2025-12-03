<img width="93" height="85" alt="logo1" src="https://github.com/user-attachments/assets/82e78f2d-df41-465b-a74a-d5c8f4365322" /><img width="81" height="87" alt="logo2" src="https://github.com/user-attachments/assets/c5223e2b-a531-42df-95bd-c74e651e0584" />                  

![pepfar_logo](https://github.com/user-attachments/assets/de6456c3-9527-4a26-af42-f8570889397c)<img width="99" height="82" alt="cdclogo" src="https://github.com/user-attachments/assets/93b94960-ae4c-4c20-8766-0581d9bae883" />



# CDC Crane Survey Analysis
The Crane survey was a PEPFAR-funded study conducted by the Centers for Disease Control and Prevention, in conjunction with Makrere University School of Public Health (Kampala, Uganda) and CDC Uganda. It collected biobehavioral information from communities across several districts in Uganda who are at disproportionately high risk of acquiring HIV, including transgender women (TGW), people who inject drugs (PWID), etc. Surveys were conducted using a respondent-driven sampling (RDS) method, and data collected included prevalence of HIV and HIV testing, experiences with discrimination and stigma, reproductive health metrics, alcohol and drug usage, and much more. 

This repo contains code for analyses of TGW in various districts of Kampala, along with a pooled analysis of TGW across 10 districts. These analyses were used to develop summary documents, highlighting pertinent biobehavioral characteristics across key populations to support ongoing HIV research and prevention efforts in Uganda. 

Survey weights were generated, to improve representativeness of the survey samples. Convergence plots for districts with low sample sizes were also created to assess viability of further analysis. Survey weights and convergence plots were created in RDS-Analyst. All other analyses were conducted in R. Visualizations were created in Miscrosoft Excel.

Below is an example of the summary document for TGW in Kampala, which received CDC clearance. Similar summaries were created for TGW across various districts, including a pooled summary, which analyzed data across all surveyed districts in Uganda.

The code for the TGW Kampala analysis is in "tgw_kampala_code.R," and the code for the pooled, cross-district TGW analysis is in "tgw_pooled_code.R"


### Example of Summary Document for TGW Kampala
<img width="927" height="616" alt="tgw_kampala_screenshot" src="https://github.com/user-attachments/assets/0a01e702-e4ea-4188-9069-60409f605a53" />


### Example of Convergence Plot (for Arua district, analyzing convergence of "outexp," which measured whether respondents had been contacted by outreach workers or peer educators talk about HIV prevention.

1: YES, IN THE LAST 6 MONTHS  

2: YES, BUT MORE THAN 6 MONTHS AGO  

3: NEVER 
<img width="1355" height="1016" alt="outexp_plot" src="https://github.com/user-attachments/assets/9b28f2d9-f69a-444d-94b9-2f95e0fe495e" />



