function [divergence_point] = survival_IP_DPA_Nov2016_Appendix(straps) %enter the number of bootstraps.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Appendix from Reingold & Sheridan (Quarterly Journal of Experimental Psychology).
%
%Individual Participant DPA procedure (IP-DPA)
%
%We are happy to answer any questions about the code (please e-mail hsheridan@albany.edu)
%If you are modifying the code for a new experiment, begin by changing the number of subjects, input file path, input file name, and f_prefix names. 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%The constants below can be changed as neccesary for each experiment.
subs = 48; %number of subjects in the experiment.
binsize = 1;  % survival bin size in ms
window = 800; % size of the window in ms. Window size can be increased to accomodate subjects/conditions with longer durations. 
nbins = window/binsize; % survival bins % must divide evenly
b_starts = 0:binsize:binsize*nbins;
run_criterion = 100; %the number of bins in a row that must be significant (this criterion is used to determine the divergence point)
num_vbin = 1200; %number of survival bins


%Define the columns in the data input file
subject = 1; %column 1 contains the subject number
duration = 2; %column 2 contains the fixation duration
condition = 3; %column 3 indicates the  condition (1 = fast condition, 2 = slow condition).


%input file path.
dat_dir = 'C:/Users/Martin Vasilev/Documents/DEVS/DPA/'; % '/Users/lab/Dropbox/Appendix/'; %location of the data input spreadsheet


%Assign the appropriate prefix name for the output files
f_prefix = 'DEVS_data_DPA'; %select a file name prefix.
expt = 'reading_data'; %expt indicates that name of the experiment.


%Open the input file.
sprintf('%sDEVS_data_DPA.txt',dat_dir) %display the name of the data input file to the user.
in = fopen(sprintf('%sDEVS_data_DPA.txt',dat_dir),'rt');
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
diffsmatrix_output=[];

        
for s = 1:subs; %for each subject, associate the x variable with the A condition data and the y variable with the B condition data (note: A is the fast condition, and B is the slow condition).
        

        x = dat(dat(:,subject)==s & dat(:,condition) == 1,duration);
        grp_fixns{s,1} = x; 
        all_A(length(all_A)+1:length(all_A)+length(x)) = x;

        y = dat(dat(:,subject)==s & dat(:,condition) == 2,duration);
        grp_fixns{s,2} = y;
        all_B(length(all_B)+1:length(all_B)+length(y)) = y;


        for b = 1:nbins %calculate the survival values for each of the time bins (these survival values will be used to create the survival figures).
            real_srvl(1,b,s) = length(find(x>b_starts(b)))/length(x);
            real_srvl(2,b,s) = length(find(y>b_starts(b)))/length(y);
            real_srvl(3,b,s) = real_srvl(2,b,s) - real_srvl(1,b,s);
        end   

        m_srvl = mean(real_srvl,3); %store the mean survival function (averaged across participants).


        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % Generate bootstraps by randomly resampling (with replacement) the data
        % from each condition and for each participant.
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        
        %generate the bootstraps
        for bs = 1:straps
            fprintf(1,sprintf('%i..',bs)); %outprint the current bootstrap to the command window, so that the user can keep track of progress.
            if mod(bs,15)==1 && bs >1
                fprintf(1,'\n');
            end

            this_strap_dat = {};


            for c = 1:2
               this_strap_dat{s,c} = randsample(grp_fixns{s,c},num_vbin,true); %randomly sample the data from each condition
               test =  this_strap_dat{s,c};
            end
            
            sorted_this_strap(1,:) = sort(this_strap_dat{s,1});  %sort data from condition 1
            sorted_this_strap(2,:) = sort(this_strap_dat{s,2});  %sort data from condition 2
            
            difference_matrix = sorted_this_strap(2,:) - sorted_this_strap(1,:);
          
            
            %determine the divergence_point
            splitpoint = 0;
            run_count = 0;

            
            for b = 1:num_vbin
                if difference_matrix(b)>0;
                    splitpoint = b;
                    run_count = run_count+1;
 
                else
                    run_count = 0;

                end
                if run_count == run_criterion
                     
                break
                else

                 end
            end
            
            
           if run_count == run_criterion; %If there was a divergence point...
               divergence_point = (splitpoint-(run_criterion-1)); %go back 4 points (to the first bin in the run of 5) to obtain the divergence point.
               was_there_divergence = 1;
           else %If there was no divergence point...
               divergence_point = num_vbin; %in this case, the criterion was never met, so by default the divergence point has been set to the maximum bin.
               was_there_divergence = 0;
               
           end    
          
            
            if divergence_point > 0;
                duration_at_divergence = mean(sorted_this_strap(:,divergence_point)); %find the duration coresponding to the divergence point (by taking the average of the two conditions at the point of divergence)
            else %if divergence_point was -4
                duration_at_divergence = num_vbin;
                divergence_point = num_vbin;
                was_there_divergence = 0;
            end
            
   
            %store the values from each bootstrap iteration.
            srvl_duration_at_divergence_bin(:,bs)= duration_at_divergence; 
            srvl_divergence_bin(:,bs) = divergence_point;         
            srvl_sorted_this_strap(:,:,bs) = sorted_this_strap(:,:); 
            srvl_straps_was_there_divergence(:,bs) = was_there_divergence;
                 
            
        end
                
        %store the median values for each subject, bin, and condition.
        for c = 1:2;
            for v_bin = 1:num_vbin;
            median_srvl_sorted_this_strap(c,v_bin,:) = median(srvl_sorted_this_strap(c,v_bin,:));
            matrix_median_srvl_sorted_this_strap(c,v_bin,s) = median_srvl_sorted_this_strap(c,v_bin);
            end
        end
        
  
      divergence_strap_alpha = .025;

      count_of_straps_with_divergence_points = sum(srvl_straps_was_there_divergence); %The sum of all the straps that produced divergence points.
      index_divergence_points_straps = logical(srvl_straps_was_there_divergence); %covert rvl_straps_was_there_divergenceto a logical vector, so that it can be used to index the straps with divergence points.
      
      if sum(srvl_straps_was_there_divergence)>0; %check if at least one of the straps produced a divergence point             
         median_srvl_duration_at_divergence_bin = median(srvl_duration_at_divergence_bin(index_divergence_points_straps)); %calculate the median of all of the straps that produced a divergence point
         sorted_srvl_duration_at_divergence_bin = sort(srvl_duration_at_divergence_bin(index_divergence_points_straps));
         srvl_duration_at_divergence_bin_ci_upper = sorted_srvl_duration_at_divergence_bin(floor(count_of_straps_with_divergence_points*(1-divergence_strap_alpha))); % upper bound on the difference
         srvl_duration_at_divergence_bin_ci_lower = sorted_srvl_duration_at_divergence_bin(ceil(count_of_straps_with_divergence_points* divergence_strap_alpha)); % lower bound on the difference
         median_srvl_divergence_bin = median(srvl_divergence_bin(index_divergence_points_straps)); %find the median divergence point
      else
         median_srvl_duration_at_divergence_bin = -999999; %The -999999 value indicates that there were no significant divergence points for this subject.
         sorted_srvl_duration_at_divergence_bin = -999999;  %The -999999 value indicates that there were no significant divergence points for this subject.
         srvl_duration_at_divergence_bin_ci_upper = -999999; %The -999999 value indicates that there were no significant divergence points for this subject.
         srvl_duration_at_divergence_bin_ci_lower = -999999; %The -999999 value indicates that there were no significant divergence points for this subject.
         median_srvl_divergence_bin = 999999; %The -999999 value indicates that there were no significant divergence points for this subject.     
      end
      
      
      
        %store the DPA results from each participant in a .txt file
        diffsmatrix_output = [diffsmatrix_output; s, median_srvl_divergence_bin, median_srvl_duration_at_divergence_bin, srvl_duration_at_divergence_bin_ci_upper, srvl_duration_at_divergence_bin_ci_lower, count_of_straps_with_divergence_points];

        textfilename = 'DPA_IP_Simulated_Output.txt'; 
        fid = fopen(textfilename, 'wt');
        for i = 1:size(diffsmatrix_output,1);
            fprintf(fid,  '%d\t%d\t%d\t%d\t%d\t%d\t\n', diffsmatrix_output(i,:));
        end
             
      fclose('all');
      close all;
      
end

