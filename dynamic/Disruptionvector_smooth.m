% The disruption vector obtained using 'Disruptionvector_getinitial' 
% can show substantial variation between months that can be smoothed 
% without affecting the overall fits to notifications. This code performs
% that smoothing.

clear all; iso3 = 'KAZ';
% chck = [iso3,'/projections_raw3.mat'];
chck = [iso3,'/projections_raw_URbnew02.mat'];
load(chck);
% if isfile(chck)
%     load(chck);
% else
%     load([iso3,'/projections_raw2.mat']);
% end
sm_vec = vec0;


% Comment out if ok to use same sample as loaded
nsam = 200;
ix0 = size(xsto,1)/2;
dx  = round(ix0/nsam);
xs  = xsto(ix0:dx:end,:);
betared = 0.25 + 0.5*rand(1,size(xs,1));


indset = {};
% indset = {6:36};
if ~isempty(indset)
    for ii = 1:length(indset)
        inds = indset{ii};
        sm_vec(inds) = smooth(smooth(sm_vec(inds)));
    end
    [pdif, aux] = simulate_disruptions_fn_v3(xs, sm_vec, datafreq, notif_dat, betared, prm, ref, agg, sel, tend, lhd, gps, opts);
end

inc_pct  = permute(prctile(aux.inct,[2.5,50,97.5],2)*1e5,[2,1,3]);
mrt_pct  = permute(prctile(aux.mort,[2.5,50,97.5],2)*1e5,[2,1,3]);
noti_pct = permute(prctile(aux.noti,[2.5,50,97.5],3)*1e5,[3,1,2,4]);           % Dims: 1.Lo/Md/Hi, 2.Month, 3.Pu/Pr, 4.Scenario
notq_pct = permute(prctile(aux.notq,[2.5,50,97.5],3)*1e5,[3,1,2,4]);           % Dims: 1.Lo/Md/Hi, 2.Month, 3.Pu/Pr, 4.Scenario

% Also get annualised incidence, mortality estimates
tmp1 = cat(4,aux.inct,aux.mort)*1e5;
dims = size(tmp1);
tmp2 = reshape(tmp1,[12,dims(1)/12,dims(2:end)]);
tmp3 = squeeze(sum(tmp2,1));
tmp4 = prctile(tmp3,[2.5,50,97.5],2);
inca_pct = tmp4(:,:,:,1);
mrta_pct = tmp4(:,:,:,2);


% --- Plot the fits -------------------------------------------------------

ff=figure; set(ff,'Position',[440   386   886   412]);
subplot(1,2,1); hold on;
lw = 1.5; fs = 14; ts = 0.1; ms = 24;
cols = linspecer(2);

if strcmp(datafreq,'monthly')
    mat = squeeze(noti_pct(:,:,1,:));
    dt = 1/12;
    ylbl = 'Monthly notifications per 100k';
else
    mat = squeeze(notq_pct(:,:,1,:));
    dt = 1/4;
    ylbl = 'Quarterly notifications per 100k';
end

for ii = 1:2
    plt = mat(:,:,ii);
    pl2(ii,:) = plot(plt(2,:),'Color',cols(ii,:),'linewidth',lw); hold on;
    jbfill(1:size(plt,2),plt(3,:),plt(1,:),cols(ii,:),'None',1,ts); hold on;
end
pl2(3,:) = plot(1/dt + [1:length(notif_dat)], notif_dat,'.-','Color','g','linewidth',lw,'markersize',ms);
ylabel(ylbl);
% xinds = 1:skip:1/dt+length(notif_dat);

xhi = find(~isnan(notif_dat),1,'last');
xlim([1, 1/dt+xhi]);
ym = [0,max(notif_dat)*1.2]; ylim(ym);
%title([ctry, ' Notifications']);
title([iso3, ' Notifications']);
legend(pl2([3,1,2],:),'Data','Modelled baseline','Modelled disruption','location','SouthWest','autoupdate','off');
set(gca,'fontsize',fs);


% --- Plot the smoothed notifications -------------------------------------

subplot(1,2,2);
plot(sm_vec,'linewidth',lw);
title('Disruption vector');
xlim([1 length(sm_vec)]);
yl = ylim; yl(1) = 0; ylim(yl);
set(gca,'fontsize',fs);

return;

save([iso3,'/sm_vec_URbnew01'], 'sm_vec', 'inca_pct', 'mrta_pct', 'noti_pct', 'notq_pct', 'indset');