import cv2
import os
import statistics
import numpy as np
import xlsxwriter  
import imageio

refpointfile = "Example_Github_Ref_points.txt"
imagedir = "//your directory" # Folder with all your data in it. Make sure to use double backslashes or forwardslashes


os.chdir(imagedir)

lines = list()
with open(refpointfile) as file:
    for line in file:
        line = line.strip()
        if line[0] != "#": # To keep functionallity from R version where files with # get ignored
            print(line)
            lines.append(line)
file.close()

########################################################################
# Start of the big loop, all folders that are in Ref_points will be run one after the other
# A window will open showing the tracking live, don't close that window! The program will crash!
# The block below from line 29 until 179 should run in one go
########################################################################

ITERATOR = 0
for i in range(len(lines)-1):
    ITERATOR = i+1
    print(ITERATOR)

    gif_images = list() # List to store images to put in gif
    
    line = lines[ITERATOR]
    line = line.strip()
    line = line.split("\t")
    # Read in foldername and images
    Foldername = line[0]
    image_folder = Foldername
    tempname = image_folder.replace("\\","_")
    tempn2 = tempname.split("_")
    tempn3 = tempn2[-2], tempn2[-3], tempn2[-1] 
    name = '_'.join(tempn3) # Create the unique name that will be unique per plant for the excel sheets
    video_name = name+".gif" # Makes a gif of the tracking for later visual check
    images = [img for img in os.listdir(image_folder) if img.endswith(".jpg")]
    frame  = cv2.imread(os.path.join(image_folder, images[0]))
    
    pixlen = line[1]
    # Read in junction and tip coordinates
    #'name', 'pixlen', 'junction', 'tip', 'meri.start', 'meri.end'
    junctip_matrix = np.zeros((2,2),int)
    junctip_matrix[0][0] = line[2].split(",")[0]
    junctip_matrix[0][1] = line[2].split(",")[1]
    junctip_matrix[1][0] = line[3].split(",")[0]
    junctip_matrix[1][1] = line[3].split(",")[1]
    # Readin Meristem coordinates
    meristem_matrix = np.zeros((len(images),2),int) # Matrix to store meristem coordinates
    meristem_matrix[0][0] = line[4].split(",")[0]
    meristem_matrix[0][1] = line[4].split(",")[1]
    meristem_matrix[len(images) -1][0] = line[5].split(",")[0]
    meristem_matrix[len(images) -1][1] = line[5].split(",")[1]
    # Fill in meristem_matrix with estimated data. Because of evaporation, our meristem was moving
    xdiff = meristem_matrix[-1][0] - meristem_matrix[0][0]
    ydiff = meristem_matrix[-1][1] - meristem_matrix[0][1]
    xdelta = xdiff / len(images)
    ydelta = ydiff / len(images)
    for t in range(1, len(images)):
        meristem_matrix[t][0] = meristem_matrix[0][0] + t * xdelta
        meristem_matrix[t][1] = meristem_matrix[0][1] + t * ydelta

    # Create output file to store results in
    name = name + "_output.xlsx"
    book = xlsxwriter.Workbook(name, {'nan_inf_to_errors': True})     
    sheet = book.add_worksheet()
    firstrow = ["image", "xpet", "ypet", "xleaf", "yleaf", "xmer", "ymer", "pixlen","m.int", "ld.thr"]
    ld_thr = 0.5 # m.int and ld.thr are leftovers of the R version. We need them to make this work with the R analysis script
    column=0
    row=0
    for item in firstrow:
        sheet.write(row, column, item)
        column +=1
    
    # Rewrite the script to work on individual images rather than videos
    frame = cv2.imread(os.path.join(image_folder, images[0]))
    trackerjun = cv2.TrackerCSRT_create()
    trackertip = cv2.TrackerCSRT_create()
    bboxjun = junctip_matrix[0][0] - 30, junctip_matrix[0][1] - 20, 60, 40 # junction
    bboxtip = junctip_matrix[1][0] - 30, junctip_matrix[1][1] - 20, 60, 40 # tip
    # Initialize tracker with first frame and bounding box
    jun = trackerjun.init(frame, bboxjun)
    tip = trackertip.init(frame, bboxtip)
    cv2.imshow("Tracking", frame)
    cv2.waitKey(100) & 0xff # This waiting step is necessary to prevent crash
    t=1
    imagecount = len(images)
    prevtod = "light" # previous time of the day
    while t < imagecount:
        image = images[t]
        image = cv2.imread(os.path.join(image_folder, image))
        frame = image
        timer = cv2.getTickCount()
        
        b,g,r = cv2.split(frame)
        #print(statistics.mean(g[1]))
        if statistics.mean(g[1]) <= 70: # Trashold to distinguish between day and night, adjust when not working for your set-up
            #print("dark")
            frame = (b/255)*(b/255)*255
            frame = frame.astype(np.uint8)
            m_int = 0
            curtod = "dark" # current time of the day
        else:
            #print("light")
            frame = g
            m_int = 1
            curtod = "light"  # current time of the day

        if prevtod == curtod:
            # Update tracker
            jun, bboxjun = trackerjun.update(frame)
            tip, bboxtip = trackertip.update(frame)
        else:
            # re-initialize tracker
            bboxjun = junction[0] - 30, junction[1] - 20, 60, 40 # junction
            bboxtip = leaftip[0] - 30, leaftip[1] - 20, 60, 40 # tip
            jun = trackerjun.init(frame, bboxjun)
            tip = trackertip.init(frame, bboxtip)
        prevtod = curtod
        
        # Calculate Frames per second (FPS)
        fps = cv2.getTickFrequency() / (cv2.getTickCount() - timer);
        

        # Tracking success: collect data and write to file
        p1 = (int(bboxjun[0]), int(bboxjun[1]))
        p2 = (int(bboxjun[0] + bboxjun[2]), int(bboxjun[1] + bboxjun[3]))
        junction = (int(p1[0] + (p2[0]-p1[0])/2), int(p1[1] + (p2[1]-p1[1])/2))

        p1 = (int(bboxtip[0]), int(bboxtip[1]))
        p2 = (int(bboxtip[0] + bboxtip[2]), int(bboxtip[1] + bboxtip[3]))
        leaftip = (int(p1[0] + (p2[0]-p1[0])/2), int(p1[1] + (p2[1]-p1[1])/2))

        meristem = (int(meristem_matrix[t][0]), int(meristem_matrix[t][1]))
            
        cv2.circle(frame, junction, radius=4, color=(0, 0, 255), thickness=2 ) # place a point in the middle
        cv2.circle(frame, leaftip, radius=4, color=(0, 0, 255), thickness=2 ) # place a point in the middle
        cv2.circle(frame, meristem, radius=6, color=(0,255,0), thickness=2)
        cv2.line(frame, (meristem), (junction), (0, 0, 255))
        cv2.line(frame, (junction), (leaftip), (0, 0, 255))

            
        # Now let's write results to output file
        row = t 
        column = 0
        new_row = [t, junction[0], junction[1], leaftip[0], leaftip[1], meristem[0], meristem[1], pixlen, m_int, ld_thr]
        for item in new_row:
            sheet.write(row, column, item)
            column +=1
        
        
        # Display tracker type on frame
        cv2.putText(frame, "CRST" + " Tracker", (50,20), cv2.FONT_HERSHEY_SIMPLEX, 0.75, (50,170,50),2);
        # Display FPS  and stats on frame
        cv2.putText(frame, "FPS : " + str(int(fps)), (50,50), cv2.FONT_HERSHEY_SIMPLEX, 0.75, (50,170,50), 2);
                
        if t % 30 == 0:
            gif_images.append(frame)
        # Display result
        cv2.imshow("Tracking", frame)        
    
        # Exit if ESC pressed
        k = cv2.waitKey(1) & 0xff
        t += 1
        if k == 27 : break
    
    cv2.destroyAllWindows()
    book.close()
    imageio.mimsave(video_name, gif_images)