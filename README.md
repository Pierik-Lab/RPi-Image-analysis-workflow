# RPi-Image-analysis-workflow
Python and R scripts for image analysis to determine leaf angles and elongation
This how to is linked to: Spatiotemporal insights into Arabidopsis leaf elongation and movement from a novel, low-cost and open-source imaging platform

The complete flow contains 2 python scripts and 1 R script. Example set is used, and can be be found in the Cropped images folder. Images were cropped using IrfanView https://www.irfanview.com/

Step 1: Download the folder "Cropped images", save the folder somewhere. The folder structure is used in the scripts for names. Adjusting folder structure means you will need to adjust the scripts.

Step 2: Setting the points of interest for the second script to track. Use Setpoints example Github and go through the script. Example_Github_foldernames.txt is used at the start

Step 3: Example_Github_Ref_points.txt should have been created by Setpoints.

Step 4: Tracking the points of interest through the images. Use Measure all Example Github and go through the script

Step 5: Individual Excel files are created while running the script. They are provided as an example

Step 6: Analyzing the data and making graphs. Use Data_analyses_graphs_Example_Github.R Example data is one genotype and multiple treatments, so one-way ANOVA. Two-way ANOVA is provided in the script.
