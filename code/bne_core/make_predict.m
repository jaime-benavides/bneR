% load config file
run('./config.m');

%%%% ---------------------- %%%%
%%%% 1: Set up for Loop  %%%%
%%%% ---------------------- %%%%
t_begin = datetime(strjoin(date_begin,'-'), 'InputFormat','yyyy-MM-dd'); 
t_end = datetime(strjoin(date_end,'-'), 'InputFormat','yyyy-MM-dd'); 
t_period = t_begin:t_end;
t_ymd = yyyymmdd(t_period);
numdays = daysact(t_begin,t_end) + 1;
day_context = 1:1:numdays;
if date_begin(1) == date_end(1)
            years = date_begin(1);
        else
            years = date_begin(1):1:date_end(1);
end

% 1.e. extract components
[trainSpace, trainTime, trainPreds, trainAqs, num_points] =  ...
    extract_components(training, inp_base_model_names, time_var);

%%%% -------------------------------------------- %%%%
%%%% 2: Generate PPD's; loop over models and years %%%%
%%%% --------------------------------------------- %%%%
   
   % 2.a. generate model
    [W,RP,wvar,sigW,Zs,Zt,piZ,mse] = train(trainAqs, trainSpace, trainTime, trainPreds, ...
    scale_space_w, scale_time_w, scale_space_rp, scale_time_rp, scale_space_wvar, ...
    lambda_w, lambda_rp, time_var, seed, bne_mode, sample_n);
       
        % 2.c loop to generate ppd for each day 
        for j = day_context
            
            % 2.c bring in the data frame of gridded predictions
            target = readtable(append(target_name_1, ...
                num2str(t_ymd(j)), target_name_2));

            % 2.c. generate and write ppd summary
            predict(W,RP,sigW,wvar,Zs,Zt,piZ, ...
            target, 10, 'summarize ppd', inp_base_model_names, ...
              scale_space_w, scale_time_w, scale_space_rp, scale_time_rp, scale_space_wvar, time_var, sample_n, ...
                fullfile(exp_folder_path, expid,'out/'), ....
                append(output_name,num2str(t_ymd(j))))
        end                
                      

    
