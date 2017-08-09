# -*- coding: utf-8 -*-
"""
Created on Sun Jun 11 23:00:56 2017

@author: Martin Vasilev

Eye-tracking experiment following Eyetrack's convention

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
	
#-------------------------
# Experimental trials:
#-------------------------

(corpus,condition,ID)= getSent(corpusFile, ncond, int(expSetup["Condition (Randomize)"]))
ntrials= len(ID)

for i in range(0, ntrials): # for each of the trials
	trialEnd= False

	item= ID[i]
	cond= condition[i]
	sentenceString= corpus[i]
	
	# get word boundaries:
	Bnds= getBnds(sentenceString, sentPos, Pix_per_Letter)

	boundary1= Bnds[0]
	boundary2= Bnds[2]
	boundary3= Bnds[4]
	boundary4= Bnds[6]
	boundary5= Bnds[8]

	boundary1Crossed= False
	boundary2Crossed= False
	boundary3Crossed= False
	boundary4Crossed= False
	boundary5Crossed= False
	
	# if number of sounds to be played not known in advance, take max boundaries for longest sentence.
	# Then, here, say: if boundary n doesn't exists: Boundary n crossed== True (i.e. nothing happens during trial)
	
      # get sounds:
	if cond<4:
		sound1= Sound(value=500, secs= 0.05)
		sound2= Sound(value=500, secs= 0.05)
		sound3= Sound(value=500, secs= 0.05)
		sound4= Sound(value=500, secs= 0.05)
		sound5= Sound(value=500, secs= 0.05)
		
		sound_type= ["STD", "STD", "STD", "STD", "STD"]
	else:
		sound1= Sound(value=500, secs= 0.05)
		sound2= Sound(value=500, secs= 0.05)
		sound3= Sound(value=500, secs= 0.05)
		sound4= Sound(value=500, secs= 0.05)
		sound5= Sound(value=5000, secs= 0.05)

		sound_type= ["STD", "STD", "STD", "DEV", "STD"]
						
		
	stimuliOn= False
	
	while not stimuliOn: # repeats loop until gazebox is triggered within x seconds
	
		#print trial ID in EDF file:
		tracker.log('TRIALID E%dI%dD0' % (cond, item))
		# print trial ID on tracker screen:
		tracker.status_msg('TRIAL E%dI%dD0 (%d out of %d)' % (item, cond, i+1, ntrials)) 
		
		# print boundary location:
		#tracker.log('BOUNDARY@ %d' % (boundary))
		
		tracker.log('CRITICAL REGION 1 @ %d %d' % (Bnds[0], Bnds[0+1]-Pix_per_Letter))
		tracker.log('CRITICAL REGION 1 @ %d %d' % (Bnds[2], Bnds[2+1]-Pix_per_Letter))
		tracker.log('CRITICAL REGION 1 @ %d %d' % (Bnds[4], Bnds[4+1]-Pix_per_Letter))
		tracker.log('CRITICAL REGION 1 @ %d %d' % (Bnds[6], Bnds[6+1]-Pix_per_Letter))
		tracker.log('CRITICAL REGION 1 @ %d %d' % (Bnds[8], Bnds[8+1]-Pix_per_Letter))

		
		# print text stimuli to edf:
		stim2edf(sentenceString, sentPos, tracker)
		
		
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
		
        	# plot word boundary lines on the eyelink display monitor:
        	for i in range(0, len(Bnds)):
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
	

	while not trialEnd:
		trialTime= globalClock.getTime()- trialStart
		trialEnd= myMouse.getPressed()[0] # terminate trial when mouse is clicked (temporary)
		
		#===========================
		xpos= tracker.sample()[0]
           
		if xpos> boundary1 and not boundary1Crossed:
			pylink.getEYELINK().sendMessage("BOUNDARY CROSSED 1")
			boundary1Crossed= True
   			sound1.play()
			pylink.getEYELINK().sendMessage("PLAY SOUND " + sound_type[0])

		if xpos> boundary2 and not boundary2Crossed:
			pylink.getEYELINK().sendMessage("BOUNDARY CROSSED 2")
			boundary2Crossed= True
   			sound2.play()
			pylink.getEYELINK().sendMessage("PLAY SOUND " + sound_type[1])

		if xpos> boundary3 and not boundary3Crossed:
			pylink.getEYELINK().sendMessage("BOUNDARY CROSSED 3")
			boundary3Crossed= True
   			sound3.play()
			pylink.getEYELINK().sendMessage("PLAY SOUND " + sound_type[2])

		if xpos> boundary4 and not boundary4Crossed:
			pylink.getEYELINK().sendMessage("BOUNDARY CROSSED 4")
			boundary4Crossed= True
   			sound4.play()
			pylink.getEYELINK().sendMessage("PLAY SOUND " + sound_type[3])

		if xpos> boundary5 and not boundary5Crossed:
			pylink.getEYELINK().sendMessage("BOUNDARY CROSSED 5")
			boundary5Crossed= True
   			sound5.play()
			pylink.getEYELINK().sendMessage("PLAY SOUND " + sound_type[4])


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

tracker.close()
disp.close()
core.quit()
