% To inform under-reporting, find the notifications during disruptions,
% relative to COVID-free baseline

clear all; ctry = 'KGZ';

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
tmp1  = noti_pct; tmp1(:,1:12,:,:) = [];
tmp2  = squeeze(tmp1(2,:,1,1));
tmp3  = tmp2(1:length(vec0));
tmp4  = max(1 - notif_dat./tmp3,0);
tmp5  = tmp4/max(tmp4);
URvec = tmp5;

% Make an alternative form, with a step function for anything below
% baseline
ind = find(tmp5==0,1,'last');
if ind>24                                                                  % Somewhat arbitrary theshold in the event that country never quite recovered from disruption (e.g. KGZ)
    tmp5(ind:end) = 0;
end
tmp5(tmp5>0) = 1; %tmp5(12) = 0;
URvec2 = tmp5;

save([ctry,'/URvec'], 'URvec', 'URvec2');

return;
figure; 
subplot(2,1,1); plot(tmp4);
subplot(2,1,2); plot(tmp5);