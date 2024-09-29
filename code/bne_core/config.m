%%%%%% BNE config file
% input base model names (names and order need to match names and order
% from training and predictions files
pollutant = 'nox';
time_var = 'percentOfYear';
sp_context = 'california';
date_begin = ["2010","04","01"];
date_end = ["2010","06","30"];
exp_desc = append(pollutant, '_', sp_context, '_', time_var, '_');
inp_base_model_names = ["cams","equates","js","omi"];
exp_folder_path = '/data0/shr/bne/exp';
expid = 'exp_0094';
training = readtable(fullfile(exp_folder_path, expid, 'in', pollutant, 'training_datasets', 'training_cvfolds.csv'));
%% grid-search
grid_search_name = exp_desc;
%% set parameter values we will consider (only if running grid search)
scale_space_w_list = [2,1,0.5];
scale_time_w_list = [2,1,0.5];
scale_space_rp_list = [2,1,0.5];
scale_time_rp_list = [2,1,0.5];
scale_space_wvar_list = [2,1,0.5];
lambda_list = [0.3679,0.1353,0.0498,0.0183];
lambda_rp_list = [0.3679,0.1353,0.0498,0.0183];
seed_list = [1234];
n_sample_list = [1000,2000];
%% Cross-validation
% set parameters
scale_space_w = 2;
scale_time_w = 1;
scale_space_rp = 2;
scale_time_rp = 1;
scale_space_wvar = 2;
lambda_w = 0.3679;
lambda_rp = 0.3679;
seed = 1234;
nfolds = 10;
bne_mode = 'cv';
sample_n = 1000;
%% Prediction
output_name = append('ref_grid_preds_',exp_desc);
% input predictions are formed by target_name_1 + year (within predict function) + day (withn predict function) + target_name_2
target_name_1 = '/data0/shr/bne/exp/exp_0094/in/nox/prediction_datasets/preds_';
target_name_2 = '.csv';
