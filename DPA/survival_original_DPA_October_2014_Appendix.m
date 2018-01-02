function [divergence_point] = survival_original_DPA_October_2014_Appendix(straps) %enter the number of bootstraps.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Appendix from Reingold & Sheridan (Frontiers in Psychology, Cognition). 
%
%Original DPA Procedure (Original-DPA)
%
%The code performs a survival analysis of fixation times as described by Reingold, Reichle, Glaholt, & Sheridan (2012).
%We are happy to answer any questions about the code (please e-mail sheridhr@gmail.com)
%If you are modifying the code for a new experiment, begin by changing the number of subjects, input file path, input file name, and f_prefix names. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%The constants below can be changed as neccesary for each experiment.
subs = 104; %number of subjects in the experiment.
binsize = 1;  % survival bin size in ms
window = 600; % size of the window in ms. Window size can be increased to accomodate subjects/conditions with longer durations. 
nbins = window/binsize; % survival bins % must divide evenly
b_starts = 0:binsize:binsize*nbins;
run_criterion = 5; %the number of bins in a row that must be significant (this criterion is used to determine the divergence point)


%Define the columns in the data input file
subject = 1; %column 1 contains the subject number
duration = 2; %column 2 contains the fixation duration
condition = 3; %column 3 indicates the  condition (1 = fast condition, 2 = slow condition).


%input file path.
dat_dir = '/Users/lab/Dropbox/Appendix/'; %location of the data input spreadsheet


%Assign the appropriate prefix name for the outputfiles
f_prefix = 'Simulated_DPA_Sample_Data'; %select a file name prefix for the condition. This name will be added to name of the figure files.
expt = 'reading_data'; %expt indicates that name of the experiment. This name will be added to name of the figure files.


%Open the input file.
sprintf('%sSimulated_DPA_Sample_Data.txt',dat_dir) %display the name of the data input file to the user.
in = fopen(sprintf('%sSimulated_DPA_Sample_Data.txt',dat_dir),'rt');
z = fgetl(in);
dat = fscanf(in,'%f',[3,inf]);
dat=dat';
fclose(in);

%define matrices.
real_srvl = []; %real grand mean survival curves
srvl_straps = []; %matrix of strapped survival curves
grp_fixns= {}; %grp_fixns stores the duration data for each condition and subject
all_B = []; %the B condition
all_A = []; %the A condition
current_subject_output = []; %matrix for storing data from each iteration


    sub_ID = 0; %this variable stores the participant ID

    for s = 1:subs; %for each subject, associate the x variable with the A condition data and the y variable with the B condition data (note: A is the fast condition, and B is the slow condition).

        sub_ID = sub_ID + 1; %increment the sub_ID by 1 (this way each subject has a unique ID, even if the same subjects were randomly selected multiple times).

        x = dat(dat(:,subject)==s & dat(:,condition) == 1,duration);
        grp_fixns{sub_ID,1} = x; 
        all_A(length(all_A)+1:length(all_A)+length(x)) = x;

        y = dat(dat(:,subject)==s & dat(:,condition) == 2,duration);
        grp_fixns{sub_ID,2} = y;
        all_B(length(all_B)+1:length(all_B)+length(y)) = y;


        for b = 1:nbins %calculate the survival values for each of the time bins (these survival values will be used to create the survival figures).
           real_srvl(1,b,sub_ID) = length(find(x>b_starts(b)))/length(x);
           real_srvl(2,b,sub_ID) = length(find(y>b_starts(b)))/length(y);
           real_srvl(3,b,sub_ID) = real_srvl(2,b,sub_ID) - real_srvl(1,b,sub_ID);
        end   
    end    

    m_srvl = mean(real_srvl,3); %store the mean survival function (averaged across participants).


    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Generate bootstraps by randomly resampling (with replacement) the data
    % from each condition and for each participant.
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


    %generate the bootstraps
    for bs = 1:straps
        fprintf(1,sprintf('%i..',bs)); %outprint the current bootstrap to the command window, so that the user can keep track of progress.
        if mod(bs,15)==1 & bs >1
            fprintf(1,'\n');
        end


        this_strap_dat = {};

        for sub_ID = 1:subs %randomly resample trial data for each subject, with replacement
            for c = 1:2
                this_strap_dat{sub_ID,c} = randsample(grp_fixns{sub_ID,c},length(grp_fixns{sub_ID,c}),true); %randomly sample the data from each condition
            end


            for b = 1:nbins
                this_strap_srvl(1,b,sub_ID) = length(find(this_strap_dat{sub_ID,1}>b_starts(b)))/length(this_strap_dat{sub_ID,1}); % compute survival percentages for each bin for this strap
                this_strap_srvl(2,b,sub_ID) = length(find(this_strap_dat{sub_ID,2}>b_starts(b)))/length(this_strap_dat{sub_ID,2});
                this_strap_srvl(3,b,sub_ID) = this_strap_srvl(2,b,sub_ID)-this_strap_srvl(1,b,sub_ID); %calculate the difference across conditions (condition B - condition A)        
      
            end
        end

       srvl_straps(:,:,bs) = mean(this_strap_srvl,3);  %average across participants and save the strap

    end

    fprintf(1,'\n');


    diff_ci = []; % matrix to hold confidence intervals on the difference between survival curves

    for b = 1:nbins
        % compute confidence intervals on the difference between A and B for each bin
        x = sort(srvl_straps(3,b,:)); 
        alpha = 0.001; %0.0005;

        diff_ci(1,b) = x(floor(straps*(1-alpha))); % upper bound on the difference
        diff_ci(2,b) = x(ceil(straps*alpha)); % lower bound on the difference
    end



    %determine the divergence_point
    splitpoint = 0;
    run_count = 0;
    
    for b = 1:nbins
        if diff_ci(2,b)>0
            splitpoint = b;
            run_count = run_count+1;
            
        else
            run_count = 0; 
        end
        if run_count == run_criterion
            break
        end
    end
    divergence_point = (splitpoint-(run_criterion-1)); %go back 4 points (to the first bin in the run of 5) to obtain the divergence point.



    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Display the results to the user.
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


    %create variables for percentage, and
    %the magnitude of the difference at the point of divergence
    

    if divergence_point > 0 %if there was a divergence_point, then display the results to the user. 

        %display the divergence_point to the user.
        fprintf(1,sprintf('\nSignificant divergence at bin %i (%i ms)\n',divergence_point,divergence_point*binsize));

        %display the percentage of fixations that are below the divergence point.
        fprintf(1,sprintf('\nPercentage of fixations shorter than divergence_point: %6.3f\n',(100 - mean(m_srvl(1:2,divergence_point))*100)));
        
    else
    end
end

