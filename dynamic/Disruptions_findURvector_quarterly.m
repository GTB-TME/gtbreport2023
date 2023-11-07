% To inform under-reporting, find the notifications during disruptions,
% relative to COVID-free baseline

clear all; ctry = 'region_EUR';

load([ctry,'/model_fits']);
chck = [iso3,'/projections_raw3.mat'];
if isfile(chck)
    load(chck);
else
    load([iso3,'/projections_raw2.mat']);
end

vec1 = vec0;

% Comment out if there's no smoothed vector
% load([ctry,'/sm_vec']);
% vec1 = sm_vec;


% --- Find the UR vector and save 
% tmp1  = noti_pct; 

% Aggregate into quarters
dims = size(aux.noti);
tmp1 = reshape(aux.noti,[3, dims(1)/3, dims(2:end)])*1e5;
tmp1 = permute(prctile(squeeze(sum(tmp1,1)),[2.5,50,97.5],2),[2,1,3]);
tmp1(:,1:4,:) = [];
tmp2  = squeeze(tmp1(2,:,1,1));
tmp3  = tmp2(1:length(vec0));
tmp4  = max(1 - notif_dat./tmp3,0);
tmp5  = tmp4/max(tmp4);
URvec = tmp5;

% Make an alternative form, with a step function for anything below
% baseline
ind = find(tmp5==0,1,'last');
tmp5(ind:end) = 0;
tmp5(tmp5>0) = 1; %tmp5(12) = 0;
URvec2 = tmp5;

save([ctry,'/URvec'], 'URvec', 'URvec2');

return;
figure; 
subplot(2,1,1); plot(tmp4);
subplot(2,1,2); plot(tmp5);