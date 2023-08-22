# This is step 1 of a 2 step script to track points through multiple pictures

# Import the required packages
import cv2
import os
import numpy as np
import math



### Define function to register mouseclicks
point_matrix = np.zeros((2,2),int) # Matrix to store mouseclicks
counter = 0 # Counter to track mouseclicks
def mousePoints(event,x,y,flags,params):
    global counter
    # Left button mouse click event opencv
    if event == cv2.EVENT_LBUTTONDOWN:
        point_matrix[counter] = x,y 
        counter = counter + 1

# Click files
Foldernames = list() #List to store all foldername
lines = list() # List with liens to be written to output
 
os.chdir("//your directory") # Folder with all your data in it. Make sure to use double backslashes or forwardslashes

with open("Example_Github_foldernames.txt") as file:
    for line in file:
        line = line.strip()
        if line[0] != "#": # To ignore folders with a # infront to allow for easier adjustments
            print(line)
            Foldernames.append(line)
setpoints  = Foldernames

######################################################################
# After setting up the mouseclick function and the folders, set the iterator to 0

ITERATOR = 0 # To track which plant you are clicking
# Python starts counting at 0!
######################################################################
# Below is the actual script to determine the points in the images
# Run this part below per plant
# You can run it as one block until line 113, or as separate blocks
# Tip: To run a single line press f9. To run a block select the block and then press f9
# If you misclick don't close the window. This causes a crash! Hit any key, spacebar for instance to go to the next window or exit the CV2 window
#######################################################################
image_folder = Foldernames[ITERATOR]
print(image_folder)
# read in image names, sort and retreive the first image
images = [img for img in os.listdir(image_folder) if img.endswith(".jpg")]
images = sorted(images) # Automatically sorts alphabetically
frame = cv2.imread(os.path.join(image_folder, images[0]))

# Each picture should contain something of a known length
# Set pixel length 
# DO NOT CLOSE THE CV2 WINDOW IT WILL CRASH
counter = 0 # To track mouseclicks
point_matrix = np.zeros((2,2),int) # Matrix to store mouseclicks
cv2.namedWindow("Click 2 points 1cm apart, then press any key", cv2.WINDOW_NORMAL)
cv2.resizeWindow("Click 2 points 1cm apart, then press any key", 1000, 1000)
cv2.imshow("Click 2 points 1cm apart, then press any key", frame)
cv2.setMouseCallback("Click 2 points 1cm apart, then press any key", mousePoints)
cv2.waitKey(0) # Necessary for python kernel to not crash
cv2.destroyAllWindows()
pixlen = math.dist(point_matrix[0], point_matrix[1])
print(pixlen)

# The following points can be adjusted based on your own points of interest
# Set points junction and tip, order of clicking matters here. Select in exact order as told
# DO NOT CLOSE THE CV2 WINDOW IT WILL CRASH
counter = 0
point_matrix = np.zeros((2,2),int) # Matrix to store mouseclicks
cv2.namedWindow("Click junction, then leaftip, then press any key", cv2.WINDOW_NORMAL)
cv2.resizeWindow("Click junction, then leaftip, then press any key", 1000, 1000)
cv2.imshow("Click junction, then leaftip, then press any key", frame)
cv2.setMouseCallback("Click junction, then leaftip, then press any key", mousePoints)
cv2.waitKey(0) 
cv2.destroyAllWindows()
junctip_matrix = point_matrix

# Set points meristem
# The first and last picture are used here
# DO NOT CLOSE THE CV2 WINDOW IT WILL CRASH
counter=0
point_matrix = np.zeros((2,2),int) # Matrix to store mouseclicks
cv2.namedWindow("Click meristem, then press any key", cv2.WINDOW_NORMAL)
cv2.resizeWindow("Click meristem, then press any key", 1000, 1000)
cv2.imshow("Click meristem, then press any key", frame)
cv2.setMouseCallback("Click meristem, then press any key", mousePoints)
cv2.waitKey(0)
last_frame = cv2.imread(os.path.join(image_folder, images[-1]))
cv2.namedWindow("Click meristem, then press any key", cv2.WINDOW_NORMAL)
cv2.resizeWindow("Click meristem, then press any key", 1000, 1000)
cv2.imshow("Click meristem, then press any key", last_frame)
cv2.setMouseCallback("Click meristem, then press any key", mousePoints)
cv2.waitKey(0)
cv2.destroyAllWindows()
meristem_matrix = point_matrix

# Preview windows to check whether all clicks were according to plan
# If you are not content. Run the block of code above again.
# DO NOT CLOSE THE CV2 WINDOW IT WILL CRASH
preview = cv2.imread(os.path.join(image_folder, images[0]))
cv2.circle(preview,(junctip_matrix[0][0],junctip_matrix[0][1]),5,(255,0,0))
cv2.circle(preview,(junctip_matrix[1][0],junctip_matrix[1][1]),5,(0,255,0))
cv2.circle(preview,(meristem_matrix[0][0],meristem_matrix[0][1]),5,(0,0,255))
cv2.line(preview, (meristem_matrix[0][0],meristem_matrix[0][1]), (junctip_matrix[0][0],junctip_matrix[0][1]), (0, 0, 255))
cv2.line(preview, (junctip_matrix[0][0],junctip_matrix[0][1]), (junctip_matrix[1][0],junctip_matrix[1][1]), (0, 0, 255))
cv2.imshow("Preview, press any key to continue", preview)
cv2.waitKey(0)
cv2.destroyAllWindows()

#############################################################
# End of point selection
#############################################################

# Storing the locations in setpoints
line = [Foldernames[ITERATOR], str(pixlen), \
       str(junctip_matrix[0][0]) + "," + str(junctip_matrix[0][1]), \
       str(junctip_matrix[1][0]) + "," + str(junctip_matrix[1][1]), \
       str(meristem_matrix[0][0]) + "," + str(meristem_matrix[0][1]), \
       str(meristem_matrix[1][0]) + "," + str(meristem_matrix[1][1]) + "\n" ]
setpoints[ITERATOR] = line
ITERATOR += 1

#############################################################
# Run the part above per plant
# Tip run this if you are not content with your clicks: ITERATOR = ITERATOR -1
#############################################################

# Run this when you are done with all plants

# Check collection to see if you have no missing data
print(setpoints)

# Add header to setpoints
spheader   = ['name', 'pixlen', 'junction', 'tip', 'meri.start', 'meri.end\n']
setpoints.insert(0,spheader)

sep = '\t'
outdata = [sep.join(entry) for entry in setpoints]

with open('Example_Github_Ref_points.txt', 'w') as output:
    output.writelines(outdata)
# Uncomment this last line once you are 100% sure you are done
# output.close()

# You are done with step 1!