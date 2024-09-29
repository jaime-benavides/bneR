% load config file
run('./config.m');

%%%% ---------------------- %%%%
%%%% 1: Optimize Parameters %%%%
%%%% ---------------------- %%%%

% 1b actually make the table
% 1b.i get all the combinations
grid_mat = combvec(scale_space_w_list, scale_time_w_list, ...
    scale_space_rp_list, scale_time_rp_list, scale_space_wvar_list, lambda_list, lambda_rp_list, ...
     seed_list, n_sample_list).';
% 1b.ii put them in a nice labeled table
grid = table;
grid.scale_space_w = grid_mat(:,1);
grid.scale_time_w = grid_mat(:,2);
grid.scale_space_rp = grid_mat(:,3);
grid.scale_time_rp = grid_mat(:,4);
grid.scale_space_wvar = grid_mat(:,5);
grid.lambda_w = grid_mat(:,6);
grid.lambda_rp = grid_mat(:,7);
grid.seed = grid_mat(:,8);
grid.sample_n = grid_mat(:,9);
grid.rmse = transpose(repelem(0, size(grid,1)));
grid.r2 = transpose(repelem(0, size(grid,1)));
grid.cover = transpose(repelem(0, size(grid,1)));
grid.mse = transpose(repelem(0, size(grid,1)));



%%%% ---------------------- %%%%
%%%% 2: Optimize Parameters %%%%
%%%% ---------------------- %%%%

for i = 1:1:size(grid,1)

    [rmse, r2, cover, mse] = make_cv(training, inp_base_model_names, ...
        grid.scale_space_w(i), grid.scale_time_w(i),  ...
        grid.scale_space_rp(i), grid.scale_time_rp(i), grid.scale_space_wvar(i), ...
        grid.lambda_w(i), grid.lambda_rp(i), time_var, ...
        grid.seed(i), nfolds, grid.sample_n(i));
    grid.rmse(i) = rmse; 
    grid.r2(i) = r2; 
    grid.cover(i) = cover; 
    grid.mse(i) = mse; 

        display(num2str(i))
        % update results table
    writetable(grid, append(fullfile(exp_folder_path, expid,'out/'), 'grid_search_', grid_search_name,'.csv'));
end
