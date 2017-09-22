% Distraction by deviant sounds during reading

% Martin R. Vasilev, 2017

global const; 

%% settings:
clear all;
clear mex;
clear functions;

cd('C:\Users\EyeTracker\Desktop\Martin Vasilev\DEVS-master')
addpath([cd '\functions'], [cd '\corpus'], [cd '\design']);

settings; % load settings
ExpSetup; % do window and tracker setup

%% Load stimuli and design:
importDesign;
load('sent.mat');
load('quest.mat');
load('hasQuest.mat');
load('corr_ans.mat');
const.ntrials= height(design);

%% Run Experiment:
runTrials;

%% Save file & Exit:
status= Eyelink('ReceiveFile');
Eyelink('Shutdown');

Screen('CloseAll');