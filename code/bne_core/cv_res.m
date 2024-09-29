run('./config.m');
[rmse, r2, coverage, me, slope] = make_cv(training, inp_base_model_names, ...
        scale_space_w, scale_time_w, scale_space_rp, scale_time_rp, scale_space_wvar, ...
        lambda_w, lambda_rp, time_var, seed, nfolds, sample_n);
fid = fopen(append(fullfile(exp_folder_path, expid,'out/'),'cv_res.txt'), 'w');
fprintf(fid, 'cross-validated results: %s is RMSE, %d is R2.\n',rmse,r2);

