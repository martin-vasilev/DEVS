global Visual const;

%% Visual settings
Visual.resX= 1920;
Visual.resY= 1080;
Visual.frameRate= 100;
Visual.offsetX= 50;
Visual.sentPos= [Visual.offsetX Visual.resY/2];
Visual.FGC= [0 0 0];
Visual.BGC= [255 255 255];
Visual.Pix_per_Letter= 14;
Visual.FontSize= 18; % ppl: 14; 16: 13ppl; 14: 11ppl

Visual.Font= 'Courier New';
Visual.TextSize= 18; %
Visual.InstrTextSize= 32;
Visual.GazeBoxSize= 40; % in pixels
Visual.GazeBoxColor= [0 0 0];
Visual.gazeBoxDur= 100; % how many ms the eye needs to stay on the gaze box before triggering it
Visual.gazeBoxDisplayTime= 7; % how many seconds to wait to trigger the gaze box

%% Experiment settings:
const.TrialTimeout= 60;
const.ncond= 4;
const.Maxtrials= 120;
const.soundDur= 0.06;
const.repetitons=1;
const.seeEye= false;
const.maxCross= 1800;

const.checkPPL= false;  % draws a rectangle around sentence to make sure letter width is correct
const.expName = 'DEVS'; % used for saving data (keep short)
const.caltype= 'H3';
const.saccvelthresh = 35;% degrees per second, saccade velocity threshold
const.saccaccthresh = 9500; % degrees per second, saccade acceleration threshold	
