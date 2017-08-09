cd('C:\Users\mvasilev\Dropbox\PhD\Noise\DSBND')

dur= 50; % in ms
Fs= 48000; % sampling Fq
len= (dur/1000)*Fs;

% White noise:
y= wgn(len,1,0);
y(:,2)= y;
audiowrite('white.wav',y,Fs, 'BitsPerSample', 16)


% Sine wave:
f=400; % Hz
Amp=1;
ts=1/Fs;
T=dur/1000;
t=0:ts:T;
y2= Amp* sin(2*pi*f*t);
y2(2,:)= y2;
y2= y2';

audiowrite('sine.wav',y2,Fs, 'BitsPerSample', 16)