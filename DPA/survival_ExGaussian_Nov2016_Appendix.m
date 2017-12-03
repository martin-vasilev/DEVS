function  survival_ExGaussian_Nov2016_Appendix

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%Appendix from Reingold & Sheridan (Quarterly Journal of Experimental Psychology). 
%
%Ex-Gaussian survival procedure
%
%We are happy to answer any questions about the code (please e-mail hsheridan@albany.edu)
%If you are modifying the code for a new experiment, begin by changing the number of subjects, input file path, input file name, and prefix name.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prefix = 'Sample_data';


%input file path.
dat_dir = 'C:/Users/Martin Vasilev/Documents/DEVS/DPA/';


%output file path.
out_dir = 'C:/Users/Martin Vasilev/Documents/DEVS/DPA/'; %'F:\'; %'E:\Dropbox\april2016_final_extract_ex_gaussparameters\';


subs = 48; %number of subjects
binsize = 1;  % survival bin size in ms
window = 600; % size of the window in ms. Window size can be increased to accomodate subjects/conditions with longer durations. 
nbins = window/binsize; % survival bins % must divide evenly
window_range = 1:600;


DV_point_empirical_criterion = .015;


Ex_Gaussian_survival_output = [];


sprintf('%sDEVS_data_DPA.txt',dat_dir)
in = fopen(sprintf('%sDEVS_data_DPA.txt',dat_dir),'rt'); 
z = fgetl(in);
dat = fscanf(in,'%f',[7,inf]);
dat=dat';

fclose(in);



  for s = 1:subs; %for each subject

    %Create a matrix with 2 rows (one for each condition) and 3 columns (mu, sigma, tau)
    sub_dat(1,:) = dat(s, 2:4); %add the fast condition parameters to the first row of sub_dat
    sub_dat(2,:) = dat(s, 5:7); %add the slow condition parameters to the second row of sub_dat

 
    for i = 1:size(sub_dat,1) %for each row of sub_dat

        fits(i,:) = exgausspdf(sub_dat(i,1),sub_dat(i,2),sub_dat(i,3),window_range);

    end

    %calculate the cumulative function

    cumulative_fits = cumsum(fits,2);

    survival_fits = 1 - cumulative_fits;
    population_splitpoint = -1; %initialize the split point (Note: If a divergence point of -1 is obtained, this means that the DV_point_empirical_criterion was never reached)
    
    for i = 1:size(survival_fits,2);
        survival_difference = (survival_fits(2,i) - survival_fits(1,i));
        if survival_difference >= DV_point_empirical_criterion; 
            population_splitpoint = i;
           break
        end
    end

    population_divergence_point = population_splitpoint;
    
        
          
    
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %store the Ex Gaussian survival data in a .txt file
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
        Ex_Gaussian_survival_output = [Ex_Gaussian_survival_output; s, population_divergence_point];

        textfilename1 = 'C:\Users\Martin Vasilev\Documents\DEVS\DPA\Sample_data_Ex_Gaussian_survival_output.txt';
        fid = fopen(textfilename1, 'wt');
        for i = 1:size(Ex_Gaussian_survival_output,1);
            fprintf(fid,  '%d\t%d\t\n',Ex_Gaussian_survival_output(i,:));
        end
             
        fclose('all');
        close all;
        
  end
