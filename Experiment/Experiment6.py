# -*- coding: utf-8 -*-
"""
Created on Sun Jun 11 23:00:56 2017

@author: Martin Vasilev

Eye-tracking experiment following Eyetrack's convention


11.08.2017: Added a mechanism to prevent overlap between sounds
18.08.2017: Added response box subject controls
"""


# import settings and libraries:
from constants import * # all experiment settings
import pylink
from psychopy import prefs
prefs.general['audioLib']= ['pygame']
from psychopy.visual import *
import random
from psychopy import core
from psychopy.core import *
from psychopy.event import waitKeys, Mouse
from psychopy.sound import Sound
from pygaze.eyetracker import EyeTracker
from pygaze.display import Display
from pygaze.screen import Screen
#import pygaze.libtime as timer

giveInstructions= True

#------------------------
# Experiment set-up part:
#------------------------

# open main screen:
win= Window(size=DISPSIZE, color= BGC)
fr= win.getActualFrameRate()# get frame rate for edf file
win.close()
#win=  Window(size=DISPSIZE, units= 'pix', fullscr= useFullscreen, color= BGC)
disp= Display()

scr= Screen(dispsize=DISPSIZE)

myMouse = Mouse(disp) # create a mouse object
myMouse.setVisible(0) # make the mouse invisible

globalClock= core.Clock()

#--------------
# Instructions:
#--------------

#if giveInstructions:
#    Instructions(disp, scr)


# Initialize Eye-tracker:
tracker= EyeTracker(disp, trackertype= 'eyelink',resolution= DISPSIZE, fgc= FGC, bgc= BGC,
		         saccvelthresh= saccvelthresh, saccaccthresh= saccaccthresh, eventdetection= "native",  logfilename= LOGFILENAME)

# Write some info into edf file:
tracker.log('DISPLAY COORDS %d %d %d %d' % (0, 0, DISPSIZE[0]-1, DISPSIZE[1]-1))
tracker.log('FRAMERATE %d' % (fr))
tracker.send_command("calibration_type= %s" % caltype) 											
tracker.calibrate()
#pylink.getEYELINK().doTrackerSetup(width=1920, height=1080)
	
#-------------------------
# Experimental trials:
#-------------------------

(corpus, quest, hasQuest, ans)= openFiles()

design= get_design(int(expSetup['Participant']))

#hasQuest= False

#(corpus,condition,ID)= getSent(corpusFile, ncond, int(expSetup["Condition (Randomize)"]))
ntrials= len(design)

for i in range(0, len(design)): # for each of the trials
	trialEnd= False

	item= design[i][0]
	
	# condition:
	if design[i][1]== "PRAC":
		cond= 9 # practice
	if design[i][1]== "SLC":
		cond= 1 # silence
	if design[i][1]== "STD":
		cond= 2 # standard
	if design[i][1]== "DEV":
		cond= 3 # deviant
	
	# position of sound (only applicable to deviant sounds)
	soundPos= design[i][2]
	#print(cond)	
	#print(soundPos)
	
	#cond= condition[i]
	sentenceString= corpus[design[i][0]-1]
	
	if cond<9: # if not practice
	
		# get word boundaries:
		Bnds= getBnds(sentenceString, sentPos, Pix_per_Letter)
		
		boundary1= Bnds[1]
		boundary2= Bnds[3]
		boundary3= Bnds[5]
		boundary4= Bnds[7]
		boundary5= Bnds[9]
		
		boundary1Crossed= False
		boundary2Crossed= False
		boundary3Crossed= False
		boundary4Crossed= False
		boundary5Crossed= False
			
		# time sound was played
		tPlay1= 0
		tPlay2= 0
		tPlay3= 0
		tPlay4= 0
		#tPlay5= 0	
			
		# time elapsed since sound was played
		tSound1= 0
		tSound2= 0
		tSound3= 0
		tSound4= 0
		#tSound5= 0
		
		# if number of sounds to be played is not known in advance, take max boundaries for longest sentence.
		# Then, here, say: if boundary n doesn't exists: Boundary n crossed== True (i.e. nothing happens during trial)
		
      # get sounds:
	if cond==2:
		sound1= Sound('standard.wav')
		sound2= Sound('standard.wav')
		sound3= Sound('standard.wav')
		sound4= Sound('standard.wav')
		sound5= Sound('standard.wav')
		
		sound_type= ["STD", "STD", "STD", "STD", "STD"]
	
	if cond==3:
		#print("if cond")
		# SOUND 1
		sound1= Sound('standard.wav') # 1st sound is always standard
		
		# SOUND 2
		if soundPos==2:
			print("pos2")
			sound2= Sound('deviant.wav')
		else:
			sound2= Sound('standard.wav')
		
		# SOUND 3
		if soundPos==3:
			print("pos3")
			sound3= Sound('deviant.wav')
		else:
			sound3= Sound('standard.wav')
		
		# SOUND 4
		if soundPos==4:
			print("pos4")
			sound4= Sound('deviant.wav')
		else:
			sound4= Sound('standard.wav')
			
		# SOUND 5
		if soundPos==5:
			print("pos5")
			sound5= Sound('deviant.wav')
		else:
			sound5= Sound('standard.wav')
			
		sound_type= ["STD", "STD", "STD", "STD", "STD"]
		sound_type[soundPos-1]= "DEV" # replace only 1 deviant
						
		
	stimuliOn= False
	
	while not stimuliOn: # repeats loop until gazebox is triggered within x seconds
	
		#print trial ID in EDF file:
		
		if item> Maxtrials:
			tracker.log('TRIALID P%dI%dD0' % (cond, item))
			# print trial ID on tracker screen:
			tracker.status_msg('TRIAL P%dI%dD0 (%d out of %d)' % (item, cond, i+1, ntrials)) 
		else:
			tracker.log('TRIALID E%dI%dD0' % (cond, item))
			# print trial ID on tracker screen:
			tracker.status_msg('TRIAL E%dI%dD0 (%d out of %d; %d done)' % (item, cond, i+1, ntrials, (i+1)/ntrials)) 
		
		# print boundary location:
		#tracker.log('BOUNDARY@ %d' % (boundary))
		if cond<9:
			tracker.log('CRITICAL REGION 1 @ %d %d' % (Bnds[1], Bnds[1+1]-Pix_per_Letter))
			tracker.log('CRITICAL REGION 2 @ %d %d' % (Bnds[3], Bnds[3+1]-Pix_per_Letter))
			tracker.log('CRITICAL REGION 3 @ %d %d' % (Bnds[5], Bnds[5+1]-Pix_per_Letter))
			tracker.log('CRITICAL REGION 4 @ %d %d' % (Bnds[7], Bnds[7+1]-Pix_per_Letter))
			tracker.log('CRITICAL REGION 5 @ %d %d' % (Bnds[9], Bnds[9+1]-Pix_per_Letter))

		
		# print text stimuli to edf:
		stim2edf(sentenceString, sentPos, tracker)
		#print(sentenceString)
		
		### Prepare trial stimuli:
			
		scr.draw_text(text= sentenceString, colour= FGC, font= Font, 
		              pos= sentPos, fontsize=TextSize, center=False)
		if checkPPL:
			lngth= len(sentenceString)*Pix_per_Letter
			scr.draw_rect(colour=FGC, x=offsetX, y=DISPSIZE[1]/2-GazeBoxSize/2, w=lngth,
					h=GazeBoxSize, pw=1, fill=False)
			
		
		# draw gaze box on tracker monitor:
		tracker.send_command("draw_filled_box %d %d %d %d %d" % (offsetX,
					DISPSIZE[1]/2-GazeBoxSize/2, offsetX+GazeBoxSize, DISPSIZE[1]/2-GazeBoxSize/2+GazeBoxSize, 15))
        		
        	for i in range(0, len(sentenceString)):
        		y1= DISPSIZE[1]/2-GazeBoxSize/2
        		y2= DISPSIZE[1]/2+GazeBoxSize/2
        		
        		if i==0:
        			x1= offsetX
        			x2= offsetX+ Pix_per_Letter
        		else:
        			x1= x2
        			x2= x2+Pix_per_Letter
        		if sentenceString[i]== " ":
        			tracker.send_command("draw_filled_box %d %d %d %d %d" % (x1, y1, x2, y2, 15))
        		else:
        			tracker.send_command("draw_box %d %d %d %d %d" % (x1, y1, x2,
                                     y2, 15))
        		#tracker.send_command("print_position= %d %d" % (i+1,15)) 
        		#tracker.send_command("echo %s" % (sentenceString[i]))
		
		if cond==2 or cond==3:
	        	# plot word boundary lines on the eyelink display monitor:
											
	        	for i in range(0, len(Bnds)):
				if i==1 or i==3 or i==5 or i==7 or i==9:
		        		x1= (Bnds[i], sentPos[1]+50)
		        		x2= (Bnds[i], sentPos[1]-50)
		        		tracker.send_command("draw_line %d %d %d %d %d" % (x1[0], x1[1], x2[0], x2[1], 4))  
	  
            # drift check:
		tracker.drift_correction()	
  
		tracker.start_recording()
		
            # prepare gaze screen:
		Gazescr= Screen(dispsize=DISPSIZE)
		Gazescr.draw_rect(colour=FGC, x=offsetX, y=DISPSIZE[1]/2-GazeBoxSize/2, w=GazeBoxSize,
					h=GazeBoxSize, pw=1, fill=True)
		gazeBnds_x= (offsetX, offsetX+GazeBoxSize)
		gazeBnds_y= (DISPSIZE[1]/2-GazeBoxSize/2, DISPSIZE[1]/2-GazeBoxSize/2+GazeBoxSize)
		# display gaze box:
		disp.fill(Gazescr)
		  
  
		gazeBoxTriggered=False
		onTarget= False
		gazeTimeOut= False
		gazeStart= globalClock.getTime()
		disp.show()
		tracker.log('GAZE TARGET ON')
		
		
		# loop that triggers the gaze-box
		while not gazeBoxTriggered and not onTarget:
			sample= tracker.sample() # get current eye position
			elapsedTime= globalClock.getTime()-gazeStart # time since gaze box appeared
			onTarget= sample[0]>= gazeBnds_x[0] and sample[0]<= gazeBnds_x[1] and sample[1]>= gazeBnds_y[0] and sample[1]<= gazeBnds_y[1]
			if onTarget: # the eye is on the gaze box
				wait(gazeBoxDur/1000)
				onTarget= sample[0]>= gazeBnds_x[0] and sample[0]<= gazeBnds_x[1] and sample[1]>= gazeBnds_y[0] and sample[1]<= gazeBnds_y[1]
				if onTarget: # eye still on gaze box after x ms
					gazeBoxTriggered= True
					stimuliOn= True
					#tracker.send_command("clear_screen %d" % (0))
				else:
					onTarget=False
			
			if elapsedTime> gazeBoxDisplayTime: # gaze box timeout
				tracker.log('TRIAL ABORTED')
				tracker.stop_recording()
				tracker.calibrate()
				onTarget=True
				gazeBoxTriggered=True

		
	tracker.log('GAZE TARGET OFF')
	tracker.log('DISPLAY ON')
	tracker.log('SYNCTIME')
														
	disp.fill(scr)
	disp.show()
	trialStart= globalClock.getTime()
	

	soundPlayed= False
	
	while not trialEnd:
		trialTime= globalClock.getTime()- trialStart
		#trialEnd= myMouse.getPressed()[0] # terminate trial when mouse is clicked (temporary)
		allowedResp= ['5']
		pressed, time= pylink.getEYELINK().getLastButtonPress()
		trialEnd= any(i in str(pressed) for i in allowedResp)
		#===========================
		xpos= tracker.sample()[0]
           
		if cond==2 or cond==3: # enter only if there are sounds to be presented..
								
			# SOUND 1:
			if xpos> boundary1 and not boundary1Crossed:
				tracker.log("BOUNDARY CROSSED 1")
				boundary1Crossed= True
	   			sound1.play()
				tracker.log("PLAY SOUND " + sound_type[0])
				tPlay1= globalClock.getTime()
			tSound1= globalClock.getTime()- tPlay1
			
			
			# SOUND 2:
			if xpos> boundary2 and not boundary2Crossed:
				if tSound1> soundDur:
					tracker.log("BOUNDARY CROSSED 2")
					boundary2Crossed= True
		   			sound2.play()
					tracker.log("PLAY SOUND " + sound_type[1])
				else:
					wait(soundDur-tSound1)
					tracker.log("BOUNDARY CROSSED 2")
					boundary2Crossed= True
		   			sound2.play()
					tracker.log("PLAY SOUND " + sound_type[1])
					tracker.log("SOUND_DELAYED 2")
				tPlay2= globalClock.getTime()
			tSound2= globalClock.getTime()- tPlay2
	
	
			# SOUND 3:
			if xpos> boundary3 and not boundary3Crossed:
				if tSound2> soundDur:
					tracker.log("BOUNDARY CROSSED 3")
					boundary3Crossed= True
		   			sound3.play()
					tracker.log("PLAY SOUND " + sound_type[2])
				else:
					wait(soundDur-tSound2)
					tracker.log("BOUNDARY CROSSED 3")
					boundary3Crossed= True
		   			sound3.play()
					tracker.log("PLAY SOUND " + sound_type[2])
					tracker.log("SOUND_DELAYED 3")
				tPlay3= globalClock.getTime()
			tSound3= globalClock.getTime()- tPlay3
			
			
			# SOUND 4:
			if xpos> boundary4 and not boundary4Crossed:
				if tSound3> soundDur:
					tracker.log("BOUNDARY CROSSED 4")
					boundary4Crossed= True
		   			sound4.play()
					tracker.log("PLAY SOUND " + sound_type[3])
				else:
					wait(soundDur-tSound3)
					tracker.log("BOUNDARY CROSSED 4")
					boundary4Crossed= True
		   			sound4.play()
					tracker.log("PLAY SOUND " + sound_type[3])
					tracker.log("SOUND_DELAYED 4")
				tPlay4= globalClock.getTime()
			tSound4= globalClock.getTime()- tPlay4
	
			
			# SOUND 5:
			if xpos> boundary5 and not boundary5Crossed:
				if tSound4> soundDur:
					tracker.log("BOUNDARY CROSSED 5")
					boundary5Crossed= True
		   			sound5.play()
					tracker.log("PLAY SOUND " + sound_type[4])
				else:
					wait(soundDur-tSound4)
					tracker.log("BOUNDARY CROSSED 5")
					boundary5Crossed= True
		   			sound5.play()
					tracker.log("PLAY SOUND " + sound_type[4])
					tracker.log("SOUND_DELAYED 5")
		if cond==1:
			# SOUND 1:
			if xpos> boundary1 and not boundary1Crossed:
				tracker.log("BOUNDARY CROSSED 1")
				boundary1Crossed= True
			# SOUND 2:
			if xpos> boundary2 and not boundary2Crossed:
				tracker.log("BOUNDARY CROSSED 2")
				boundary2Crossed= True
			# SOUND 3:
			if xpos> boundary3 and not boundary3Crossed:
				tracker.log("BOUNDARY CROSSED 3")
				boundary3Crossed= True
			# SOUND 4:
			if xpos> boundary4 and not boundary4Crossed:
				tracker.log("BOUNDARY CROSSED 4")
				boundary4Crossed= True
			# SOUND 5:
			if xpos> boundary5 and not boundary5Crossed:
				tracker.log("BOUNDARY CROSSED 5")
				boundary5Crossed= True


		if trialTime> TrialTimeout: # end trial automatically if no response by participant
			trialEnd= True
                 #tracker.log('TRIAL ABORTED')
			scr.clear(colour=BGC)
		
	scr.clear(colour=BGC) # clear subject screen
	disp.show()
	tracker.send_command("clear_screen %d" % (0)) # clear tracker screen	
	
	# end of trial messages:
	tracker.log('ENDBUTTON 5')
	tracker.log('DISPLAY OFF')
	tracker.log('TRIAL_RESULT 5')
	tracker.log('TRIAL OK')
	tracker.stop_recording()
	
	
	if int(hasQuest[item-1])==1:
		Quest(disp, scr, tracker, item, cond, quest[item-1], int(ans[item-1]))

tracker.close()
disp.close()
core.quit()
