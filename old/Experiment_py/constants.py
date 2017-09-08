# -*- coding: utf-8 -*-
"""
Created on Sun Jun 11 23:00:56 2017

@author: Martin Vasilev
"""


#---------------------#
#  General settings:  #
#---------------------#
lab= True
checkPPL= False # draws a rectangle around sentence to make sure letter width is correct
expName = "DEVS" # used for saving data (keep short)
expDir= 'C:\Users\Martin Vasilev\Dropbox\pyTrack'
corpusFile= "C:\Users\EyeTracker\Desktop\Martin Vasilev\WinPython-PyGaze-0.5.1\Devs\sentences.txt"
#eyedatafile= 'test.edf'

# At 14 ppl, 1 letter subtends 0.3446 deg per visual angle (3 letters= 1.0338)
# With 50 pixel offset, screen fits max 133 characters

#---------------------#
#  Display settings:  #
#---------------------#
# screen resolution:
if lab:
	DISPSIZE = (1920, 1080) # P103j
else:
	DISPSIZE = (1366, 768) # laptop
 
	
 # laptop 
#DISPSIZE = (1024, 768) # lab
offsetX= 50 # sentence offset on x-axis
DISPTYPE = 'psychopy'
sentPos= (offsetX, DISPSIZE[1]/2)

#FGC = (-1, -1, -1) # text colour
#BGC = (1, 1, 1) # background colour 
FGC= (0,0,0)
BGC= (255, 255, 255)

#------------------------#
#  Experiment settings:  #
#------------------------#
Font= 'Courier New'
TextSize= 22
InstrTextSize= 32
GazeBoxSize= 40 # in pixels
GazeBoxColor= (-1, -1, -1)
gazeBoxDur= 100 # how many ms the eye needs to stay on the gaze box before triggering it
gazeBoxDisplayTime= 7 # how many seconds to wait to trigger the gaze box
TrialTimeout= 60
ncond=4
Maxtrials= 120
soundDur= 0.06

expSetup = {'Participant': ''}
           #'Condition (Randomize)': ''}
from psychopy import gui
dlg = gui.DlgFromDict(dictionary=expSetup, title= 'Run experiment: '+ expName)
LOGFILENAME= expName+ expSetup['Participant']


#---------------------#
#  Tracker settings:  #
#---------------------#
# Other constants:
#useFullscreen=True
caltype= "H3" # 3-point horizontal (use "HV9" for 9-point grid)
trackertype = 'eyelink'
saccvelthresh = 35 # degrees per second, saccade velocity threshold
saccaccthresh = 9500 # degrees per second, saccade acceleration threshold'	

#------------------------#
#  Additional functions: #
#------------------------#

# prints stimuli to edf file
def stim2edf(sent, sentPos, tracker):
	from psychopy.core import wait
	chars= list(sent) #get characters in sentence
	x_start= []
	x_end= []
	
	for i in range(0, len(sent)): # loop to calulate x position of letters
		if i==0:
			x_start.append(sentPos[0]);
			x_end.append(sentPos[0]+ Pix_per_Letter)
		else:
			x_start.append(x_end[i-1])
			x_end.append(x_end[i-1]+Pix_per_Letter)
			
	y_start= 520
	y_end= 560
	
	tracker.log('DISPLAY TEXT 1')
	
	for i in range(0, len(sent)): # print coords of letters on the screen
		tracker.log('REGION CHAR %d 1 %s %d %d %d %d' % (i, chars[i], x_start[i], y_start, x_end[i], y_end))
		wait(0.001) # wait time for consitency purposes with Eyetrack
		tracker.log('DELAY 1 MS')
		wait(0.001)


import ctypes

# calculates pixels per letter for fonts
def GetTextDimensions(text, points, font):
	class SIZE(ctypes.Structure):
		_fields_ = [("cx", ctypes.c_long), ("cy", ctypes.c_long)]

	hdc = ctypes.windll.user32.GetDC(0)
	hfont = ctypes.windll.gdi32.CreateFontA(points, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, font)
	hfont_old = ctypes.windll.gdi32.SelectObject(hdc, hfont)
	size = SIZE(0, 0)
	ctypes.windll.gdi32.GetTextExtentPoint32A(hdc, text, len(text), ctypes.byref(size))
	
	ctypes.windll.gdi32.SelectObject(hdc, hfont_old)
	ctypes.windll.gdi32.DeleteObject(hfont)
	
	return (size.cx, size.cy)

# PLEASE NOTE: this has been tested only with Courier New; use at own risk with other fonts
Pix_per_Letter= GetTextDimensions("a", TextSize, Font)[0]+2

print(Pix_per_Letter)

def getSent(corpusFile, ncond, start):
	with open(corpusFile, 'r') as f:
		corpus= f.readlines()
		corpus= [x.strip() for x in corpus]
		
	ID= range(1, len(corpus)+1)
	
	cond_t= range(start, ncond+1)
	if start>1:
		cond_t= cond_t + range(1, start)
	cond= cond_t*5
	
	# shuffle elements:
	c= list(zip(corpus, ID, cond))
	from random import shuffle
	shuffle(c)
	corpus, ID, cond = zip(*c)
	
	return(corpus, cond, ID)
	
def getBnds(sent, sentPos, Pix_per_Letter):
	c= " "
	pos= [pos for pos, char in enumerate(sent) if char== c]
	Bnd=[]
	for i in range(0, len(pos)):
		Bnd.append(sentPos[0]+ pos[i]*Pix_per_Letter+Pix_per_Letter)
	return(Bnd)


def get_design(ID):
	
	import os
	
	if not os.path.exists('Design'):
	    os.makedirs('Design')
	
	#ID= 48
	nsent= 120
	ncond= 3
	npos= 4 # num of target word positions
	
	item= range(1,nsent+1)
	
	S1= [1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46]
	S2= [2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47]
	S3= [3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48]
	
	P1= [1, 2, 3, 13, 14, 15, 25, 26, 27, 37, 38, 39]
	P2= [4, 5, 6, 16, 17, 18, 28, 29, 30, 40, 41, 42]
	P3= [7, 8, 9, 19, 20, 21, 31, 32, 33, 43, 44, 45]
	P4= [10, 11, 12, 22, 23, 24, 34, 35, 36, 46, 47, 48]
	
	
	if ID in S1:
		sound= ["SLC"]*int((nsent/ncond)) + ["STD"]*int((nsent/ncond)) + ["DEV"]*int((nsent/ncond))	
		
	if ID in S2:
		sound= ["STD"]*int((nsent/ncond)) + ["DEV"]*int((nsent/ncond)) + ["SLC"]*int((nsent/ncond))	
		
	if ID in S3:
		sound= ["DEV"]*int((nsent/ncond)) + ["SLC"]*int((nsent/ncond)) + ["STD"]*int((nsent/ncond))	
	
	####
	
	if ID in P1:
		pos= [2, 3, 4, 5]* int(nsent/npos)
	
	if ID in P2:
		pos= [3, 4, 5, 2]* int(nsent/npos)
		
	if ID in P3:
		pos= [4, 5, 2, 3]* int(nsent/npos)
		
	if ID in P4:
		pos= [5, 2, 3, 4]* int(nsent/npos)
		
	
	c= list(zip(item, sound, pos))
	matching = [s for s in c if s[1]=="SLC"]
	not_matching= [s for s in c if s[1]!="SLC"]
	
	STD = [i for i, s in enumerate(not_matching) if 'STD' in s[1]]
	first= [not_matching[STD[0]]]+ [not_matching[STD[1]]]+ [not_matching[STD[2]]]
	del not_matching[STD[0]]
	del not_matching[STD[1]-1]
	del not_matching[STD[2]-2]
	
	# randomise sounds' block:
	from random import shuffle
	shuffle(not_matching)
	shuffle(matching)
	
	pract= [(121, 'PRAC', 1), (122, 'PRAC', 1), (123, 'PRAC', 1), \
              (124, 'PRAC', 1), (125, 'PRAC', 1), (126, 'PRAC', 1)]
	shuffle(pract)
	
		
	if ID%2==1: # odd numbers
		B1= matching
		B2= not_matching
		design= pract+ B1+ first+B2
	else: # even numbers
		B1= not_matching
		B2= matching
		design= pract+ first+B1+B2
	#print(sound)
	#print(pos)
	#print(design)
	
	thefile = open('Design/P'+ str(ID)+ '.txt', 'w')
	thefile.write("item sound pos\n") # columns
	
	for item in design:
	  thefile.write("%s %s %s\n" % item)
	thefile.close()
	
	return(design)


def Quest(disp, scr, tracker, item, cond, Question, corr_ans, FGC= (0,0,0), TextSize= 22, Font= 'Courier New'):
	from psychopy import event
	from psychopy.core import wait
	import pylink	
	
	#allowedResp= ['y', 'n']
	allowedResp= ['2', '4']
	answered= False
	
	# Initial question stamps:
	tracker.log('TRIALID F%dI%dD1' % (cond, item))
	tracker.log('QUESTION_ANSWER %d' % (corr_ans))
	tracker.log('DELAY 500 MS')
	tracker.start_recording()
	wait(0.05)
	tracker.status_msg('Question F%dI%dD1' % (cond, item)) 
	wait(0.5)
	tracker.log('DISPLAY ON')
	tracker.log('SYNCTIME')
	
	##################
	scr.draw_text(text= Question, colour= FGC, font= Font, center=False, pos= sentPos, fontsize=TextSize)

	scr.draw_text(text= 'YES', colour= 'green', font= Font, center=False, pos= (sentPos[0], sentPos[1]+75), fontsize=TextSize)
	scr.draw_text(text= 'NO', colour= 'red', font= Font, center=False, pos= (sentPos[0]+300, sentPos[1]+75), fontsize=TextSize)
	
	disp.fill(scr)
	disp.show()	
	
	
	###	
	while not answered:
#		pressed= event.getKeys()
#		if any(i in pressed for i in allowedResp):
#			answered= True	
			#ans= int(pressed[0])
#			if pressed[0]=="n":
#				ans= 0
#			else:
#				ans= 1
#			tracker.stop_recording()
		
		pressed, time= pylink.getEYELINK().getLastButtonPress()
		if any(i in str(pressed) for i in allowedResp):
			answered= True	
#			ans= int(pressed)
#			print(ans)
			if pressed==4:
				ans=1
			else:
				ans=0
			tracker.stop_recording()
			
	# clear screen:
	scr.clear()
	disp.fill(scr)
	disp.show()
	
	# Print end of question stamps to edf:
	tracker.log('ENDBUTTON %d' % (ans))
	tracker.log('DISPLAY OFF')
	tracker.log('TRIAL_RESULT %d' % (ans))
	tracker.log('TRIAL OK')


#######

def openFiles():
	
	with open("sent.txt", 'r') as f:
		corpus= f.readlines()
		corpus= [x.strip() for x in corpus]
	
	with open("quest.txt", 'r') as f:
		quest= f.readlines()
		quest= [x.strip() for x in quest]
	
	with open("hasQuest.txt", 'r') as f:
		hasQuest= f.readlines()
		hasQuest= [x.strip() for x in hasQuest]
	
	with open("ans.txt", 'r') as f:
		ans= f.readlines()
		ans= [x.strip() for x in ans]
	
	return(corpus, quest, hasQuest, ans)
	