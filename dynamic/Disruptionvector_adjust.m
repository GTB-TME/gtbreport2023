% Code to test post-hoc refinements to the raw disruption vector obtained
% using 'Disruptionvector_getinitial'. This code is not used for all
% countries: its purpose is simply to test different possible refinements,
% to understand their effect on the overall fit.
% The variable 'opt' below can be set 
% to the following:
% - 'Point adjustment' allows specific terms in the raw vector to be
% changed, to see their effect on the overall fit
% - 'Further smoothing' performs a second round of smoothing, following use
% of 'Disruption_vector_smooth'
% - 'Other' allows any other adjustments to the disruption vector

clear all; warning off;
ctry = 'VNM';

% load([ctry,'/model_fits_popupdated']);
load([ctry,'/model_fits']);

fname = [iso3,'/projections_raw_URbnew02.mat'];
load(fname);

% chck = [iso3,'/projections_raw3.mat'];
% if isfile(chck)
%     load(chck);
% else
%     load([iso3,'/projections_raw2.mat']);
% end


% Load modified vector if available
% chck = [ctry,'/sm_vec.mat']; 
% chck = [ctry,'/trial_vec.mat']; 
% chck = [iso3,'/sm_vec_extended.mat'];

% if isfile(chck); load(chck); vec0 = sm_vec; end    


% Comment out if ok to use same sample as loaded
nsam = 200;
ix0 = size(xsto,1)/2;
dx  = round(ix0/nsam);
xs  = xsto(ix0:dx:end,:);
betared = 0.25 + 0.5*rand(1,size(xs,1));



opts.const = 0;

% -------------------------------------------------------------------------
% --- Do the simulations --------------------------------------------------

% opt = 'Point adjustment';
% opt = 'Further smoothing';
opt = 'Other';

if strcmp(opt, 'Point adjustment')

%     vec0([5:8]) = 0.7;
%     vec0(9:end) = [];
    vecsto = vec0;
    tmp = fillmissing(notif_dat,'next');
    nsteps = length(find(~isnan(tmp))) - length(vec0);
    if nsteps>0
        for inst = 1:nsteps
            fprintf('Step %0.5g ',inst);
            
            % --- Look for a close match with data for first two data points, else impose boundaries of +/- 20% of last point
            prop = vec0(end);
            hi = prop*1.2; lo = prop*0.8;
            vec0 = [vec0, prop];
            
            pdif = 1; count = 1; proceed = 1;
            while abs(pdif)>0.01 && count<10 && proceed
                
                [pdif, aux] = simulate_disruptions_fn_v3(xs, vec0, datafreq, notif_dat, betared, prm, ref, agg, sel, tend, lhd, gps, opts);
                if aux.sim > aux.dat
                    hi = prop;
                else
                    lo = prop;
                end
                prop = (hi+lo)/2;
                vec0(end) = prop;
                
                count = count+1;
                if hi-lo<0.005
                    proceed = 0;
                end
            end
            fprintf('\n');
        end
        inc_pct  = permute(prctile(aux.inct,[2.5,50,97.5],2)*1e5,[2,1,3]);
        mrt_pct  = permute(prctile(aux.mort,[2.5,50,97.5],2)*1e5,[2,1,3]);
        noti_pct = permute(prctile(aux.noti,[2.5,50,97.5],3)*1e5,[3,1,2,4]);   % Dims: 1.Lo/Md/Hi, 2.Month, 3.Pu/Pr, 4.Scenario
        notq_pct = permute(prctile(aux.notq,[2.5,50,97.5],3)*1e5,[3,1,2,4]);   % Dims: 1.Lo/Md/Hi, 2.Month, 3.Pu/Pr, 4.Scenario
    end
    
elseif strcmp(opt, 'Further smoothing')
    
    indset = {};
    
    % sm_vec(end) = 1;
    % indset = {5:36};
    % vec0(5) = 0.5;
    if ~isempty(indset)
        for ii = 1:length(indset)
            inds = indset{ii};
            %vec0(inds) = smooth(smooth(vec0(inds)));
            vec0(inds) = smooth(smooth(vec0(inds)));
        end
        [pdif, sim, dat, inct, mort, inct2, mort2, noti, notq] = simulate_disruptions_fn_v3(xs, vec0, datafreq, notif_dat, betared, prm, ref, agg, sel, tend, lhd, gps, opts);
    end
    inc_pct  = permute(prctile(aux.inct,[2.5,50,97.5],2)*1e5,[2,1,3]);
    mrt_pct  = permute(prctile(aux.mort,[2.5,50,97.5],2)*1e5,[2,1,3]);
    noti_pct = permute(prctile(aux.noti,[2.5,50,97.5],3)*1e5,[3,1,2,4]);       % Dims: 1.Lo/Md/Hi, 2.Month, 3.Pu/Pr, 4.Scenario
    notq_pct = permute(prctile(aux.notq,[2.5,50,97.5],3)*1e5,[3,1,2,4]);       % Dims: 1.Lo/Md/Hi, 2.Month, 3.Pu/Pr, 4.Scenario

elseif strcmp(opt, 'Other')
    % Any other tweaks to be explored
    
    %vec0(6:17) = 1;
    %vec0(18:36) = 1.1*vec0(18:36);
    %vec0([5:8]) = 0.6;
    %vec0(9:end) = smooth(smooth(vec0(9:end)));
    
    [~,aux] = simulate_disruptions_fn_v3(xs, vec0, datafreq, notif_dat, betared, prm, ref, agg, sel, tend, lhd, gps, opts);

    inc_pct  = permute(prctile(aux.inct,[2.5,50,97.5],2)*1e5,[2,1,3]);
    mrt_pct  = permute(prctile(aux.mort,[2.5,50,97.5],2)*1e5,[2,1,3]);
    noti_pct = permute(prctile(aux.noti,[2.5,50,97.5],3)*1e5,[3,1,2,4]);       % Dims: 1.Lo/Md/Hi, 2.Month, 3.Pu/Pr, 4.Scenario
    notq_pct = permute(prctile(aux.notq,[2.5,50,97.5],3)*1e5,[3,1,2,4]);       % Dims: 1.Lo/Md/Hi, 2.Month, 3.Pu/Pr, 4.Scenario
end

% -------------------------------------------------------------------------
% --- Plot the results ----------------------------------------------------

% --- Notification fits 

ff=figure; set(ff,'Position',[440   386   886   412]);
subplot(1,2,1); hold on;
lw = 1.5; fs = 14; ts = 0.1; ms = 24;
cols = linspecer(2);

if strcmp(datafreq,'monthly')
    mat = squeeze(noti_pct(:,:,1,:));
    dt = 1/12;
    ylbl = 'Monthly notifications per 100k';
elseif strcmp(datafreq,'quarterly')
    mat = squeeze(notq_pct(:,:,1,:));
    dt = 1/4;
    ylbl = 'Quarterly notifications per 100k';
elseif strcmp(datafreq,'annual')
    dims = size(noti);
    tmp1 = reshape(noti,[12,dims(1)/12,dims(2:end)])*1e5;
    tmp2 = squeeze(sum(tmp1,1));
    mat  = permute(prctile(tmp2,[2.5,50,97.5],2),[2,1,3]);
    dt = 1;
    ylbl = 'Annual notifications per 100k';
end

% if monthly
%     mat = squeeze(noti_pct(:,:,1,:));
%     dt = 1/12;
%     ylbl = 'Monthly notifications per 100k';
% else
%     mat = squeeze(notq_pct(:,:,1,:));
%     dt = 1/4;
%     ylbl = 'Quarterly notifications per 100k';
% end

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
yl = ylim; yl(1) = 0; ylim(yl);
%title([ctry, ' Notifications']);
title([iso3, ' Notifications']);
legend(pl2([3,1,2],:),'Data','Modelled baseline','Modelled disruption','location','SouthWest','autoupdate','off');
set(gca,'fontsize',fs);

% --- Smoothed notifications 

subplot(1,2,2);
plot(vec0,'linewidth',lw);
title('Disruption vector');
xlim([1 length(vec0)]);
yl = ylim; yl(1) = 0; ylim(yl);
set(gca,'fontsize',fs);

save(fname);

return;

saving = input("\n Save updated disruption vector? 0/1    ");
if saving
    % Also get annualised incidence, mortality estimates
    tmp1 = cat(4,aux.inct,aux.mort)*1e5;
    [~,tmp4] = annualise(tmp1,1,true,2);
    inca_pct = tmp4(:,:,:,1);
    mrta_pct = tmp4(:,:,:,2);
    noti = aux.noti;
    
    sm_vec = vec0;
    save([ctry,'/sm_vec_URbnew01'], 'sm_vec', 'inca_pct', 'mrta_pct', 'noti_pct', 'notq_pct', 'noti');
end

