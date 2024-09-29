% load config file
run('./config.m');

%%%% ---------------------- %%%%
%%%% 1: Set up for Loop  %%%%
%%%% ---------------------- %%%%



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
        
         % 2.b.loop to generate ppd for each year
    for t = temporal_context
    
        % 2.c loop to generate ppd for each day  %CARLOS--Added a loop for the daily context%
        for j = day_context
            
            if j < 10
                spacer = '_00'
            elseif j > 9 & j < 100
                spacer = '_0'
            else
                spacer = '_'
            end
            
            % 2.c bring in the data frame of gridded predictions
            target = readtable(append(target_name_1, ...
                num2str(t),spacer, num2str(j), target_name_2));

            % 2.c. generate and write ppd summary
            predict(W,RP,sigW,wvar,Zs,Zt,piZ, ...
            target, 10, 'summarize ppd', inp_base_model_names, ...
              scale_space_w, scale_time_w, scale_space_rp, scale_time_rp, scale_space_wvar, time_var, sample_n, ...
                fullfile(exp_folder_path, expid,'out/'), ....
                append(output_name, num2str(t),spacer, num2str(j)))
        end                
                        
            
    end

    
