% Shift to annual: for countries where subannual data is
% only available for a limited number of years, and only annual data
% thereafter, then assume a gradual return to some given value, in order to
% match that annual data

% NB: This code ASSUMES that the subannual data is monthly
% --------------------------------------------------------

clear all; warning off;
ctry = 'BRA';

% load([ctry,'/model_fits_popupdated']);
load([ctry,'/model_fits']);
chck = [iso3,'/projections_raw3.mat'];
if isfile(chck)
    load(chck);
else
    load([iso3,'/projections_raw2.mat']);
end

% Load modified vector if available
chck = [ctry,'/sm_vec.mat'];
% chck = [ctry,'/trial_vec.mat'];
if isfile(chck); load(chck); vec0 = sm_vec; end

% % Load old disruption vector if extending
% load(['../../Phase 2/Coding/General2f/',[ctry],'/disruption_vector'],'vec0');
% opts.const = 0;
% % vec0(5:end) = smooth(smooth(vec0(5:end)));

opts.const = 0;

% --- Also, bring in annual data
if strcmp(iso3,'BRA')    
    C = readtable('Data/TB Notifications/annual_notifs_2023-07-24.csv');
    rows = find(strcmp(C.iso3,'BRA') & C.year>=2020);
    vals = C.c_newinc(rows)';
    annu_data = vals(2:end)/popn*1e5;    
else    
    load('Data/TB Notifications/preCOVID_notifs.mat');
    row = find(strcmp(mat.iso3,iso3));
    dat = mat{row,3:end};
    % Find which years are needed
    tmp = fillmissing(notif_dat,'next');
    nmissing = length(find(isnan(tmp)));
    nyrs_missing = nmissing/12;
    annu_data = dat(end-nyrs_missing+1:end)/popn*1e5;    
end
% Append to the annualised notification data
tmp1 = tmp(~isnan(tmp));
tmp2 = reshape(tmp1,12,length(tmp1)/12);
tmp3 = squeeze(sum(tmp2,1));
notif_dat_annu = [tmp3, annu_data];



% -------------------------
vec0_shift = {vec0, []};
nsteps = length(annu_data);

for inst = 1:nsteps
    if inst == 1
        prop = vec0_shift{1}(end);
    else
        prop = vec0_shift{2}(end);
    end
    hi = prop*1.2; lo = prop*0.8;
    vec0_shift{2} = [vec0_shift{2}, prop];
    
    pdif = 1; count = 1; proceed = 1;
    while abs(pdif)>0.01 && count<10 && proceed
        
        [pdif, aux] = simulate_disruptions_fn_v3_annual_shift(xs, vec0_shift, notif_dat_annu, betared, prm, ref, agg, sel, tend, lhd, gps, opts);
        sim = aux.sim;
        dat = aux.dat;
        
        if sim>dat
            hi = prop;
        else
            lo = prop;
        end
        prop = (hi+lo)/2;
        vec0_shift{2}(end) = prop;
        
        count = count+1;
        if hi-lo<0.005
            proceed = 0;
        end
    end
    fprintf('\n');
    
end

tmp1 = cat(4,inct,mort)*1e5;
[~,tmp4] = annualise(tmp1,1,true,2);
inca_pct = tmp4(:,:,:,1);
mrta_pct = tmp4(:,:,:,2);
noti_pct = permute(prctile(noti,[2.5,50,97.5],3),[3,1,2,4]);

saving = input("\n Save updated disruption vector? 0/1    ");
if saving
    %sm_vec = vec0;
    save([ctry,'/sm_vec_shifted_new'], 'vec0_shift', 'inca_pct', 'mrta_pct', 'noti_pct', 'notq_pct', 'noti','annu_data');
end






