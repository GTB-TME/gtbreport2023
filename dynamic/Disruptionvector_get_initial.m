% --- Code to construct an initial 'disruption vector', given
% monthly/quarterly notification data, and fitted parameters to
% pre-pandemic data. This output vector is 'initial' in the sense that it 
% can be subsequently smoothed, and individual elements adjusted to refine
% the agreement with the notification data (see Disruptionvector_adjust)

warning off;

looping_sd = 0;
if ~looping_sd
    clearvars -except looping_sd loading;
    iso3 = 'KGZ';
end
load([iso3,'/model_fits']);
opts.const = 0;

tend = 2025;                                                               % End date for simulation

% Draw a 'thinned' posterior distribution
ix0 = round(size(xsto,1)/2); nx = 50; dx = round((size(xsto,1)-ix0)/nx);
xs = xsto(ix0:dx:end,:,1);

% --- Get the disruption notifications
if contains(iso3,'region')
    load regional_data;
    reg = erase(iso3,'region_');
    notif_rate = regdata.(reg).disruption_notifs;
    notif_smth = notif_rate;
    datafreq = 'quarterly';
    dt = 1/4;
else
    load('Data/Disruptions/disruption_data_2023_04_24.mat');
    ico = find(strcmp(iso3_disrp,iso3));
    
    % Classify whether monthly, quarterly or annual
    if isempty(ico) 
        datafreq = 'annual';
    else
        if freq(ico,1)==70
            datafreq = 'monthly';
        elseif freq(ico,1)==71
            datafreq = 'quarterly';
        elseif isnan(freq(ico,1))
            datafreq = 'annual';
        end
    end
    
    if ~looping_sd && exist('freq')
        disp([num2str(freq(ico,:)), ' ', datafreq]);
    end

    % Find the time interval and relevant data source
    if strcmp(datafreq,'monthly')
        dat = mdata(ico,:,:);
        dt  = 1/12;
                
    elseif strcmp(datafreq,'quarterly')
        dat = qdata(ico,:,:);
        dt  = 1/4;
        
    elseif strcmp(datafreq,'annual')
        % Pull annual data
        load Data/'TB Notifications'/preCOVID_notifs.mat;
        row = find(strcmp(mat.iso3,iso3));
        dat = mat{row,end-2:end};
        dt = 1;
    end
    
    % Get notifications as rates, and smooth in the case of monthly
    notif_rate = dat(:)'/popn*1e5;
    notif_smth = notif_rate;
    if strcmp(datafreq,'monthly')
        tmp = notif_rate;
        tmp(6:end) = smooth(tmp(6:end));
        tmp(isnan(notif_rate)) = nan;
        notif_smth = tmp;
    end
end

% --- Lockdown dates
betared = 0.25 + 0.5*rand(1,size(xs,1));                                   % Reduction in beta during lockdown

% --- Construct data adjusted for under-reporting 
notif_dat = notif_smth;

% --- Do the simulations

if strcmp(datafreq,'monthly')
    vec0 = [1 1 1];
    if strcmp(iso3,'CHN')
        vec0 = [1];
    end
elseif strcmp(datafreq,'quarterly') 
    vec0 = [1];
else
    vec0 = [];
end
vecsto = vec0;

tmp = fillmissing(notif_dat,'next');
nsteps = length(find(~isnan(tmp))) - length(vec0);

for inst = 1:nsteps
    fprintf('Step %0.5g ',inst);
    
    prop = 1; hi = 1.5; lo = 0;
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
noti_pct = permute(prctile(aux.noti,[2.5,50,97.5],3)*1e5,[3,1,2,4]);           % Dims: 1.Lo/Md/Hi, 2.Month, 3.Pu/Pr, 4.Scenario
notq_pct = permute(prctile(aux.notq,[2.5,50,97.5],3)*1e5,[3,1,2,4]);           % Dims: 1.Lo/Md/Hi, 2.Month, 3.Pu/Pr, 4.Scenario

save([iso3,'/disruption_vector3'],'vec0');
save([iso3,'/projections_raw3']);



if ~looping_sd
    
    vec1 = vec0;
    
    % ---------------------------------------------------------------------
    % --- Plot the fit with notifications ---------------------------------
        
    ff=figure; set(ff,'Position',[283 336 1008 499]);
    subplot(1,2,1);
    hold on;
    lw = 1.5; fs = 14; ts = 0.1; ms = 24;
    cols = linspecer(2);
    
    if strcmp(datafreq,'monthly')                                        % <--- Update this to account for annual data
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
    skip = 2; xinds = 1:skip:1/dt+length(notif_dat);
    
    xhi = find(~isnan(notif_dat),1,'last');
    xlim([1, 1/dt+xhi]);
%     ym = [0,max(notif_dat)*1.2]; ylim(ym);
    %title([iso3, ' Notifications']);
    title(['EUR Region', ' Notifications']);
    legend(pl2([3,1,2],:),'Data','Modelled baseline','Modelled disruption','location','SouthWest','autoupdate','off');
    set(gca,'fontsize',fs);
    xlabel('Quarters since Q1 2019');
    
    subplot(1,2,2);
    plot([ones(1,4), vec0],'linewidth',lw);
    title('Disruption vector');
    xlabel('Quarters since Q1 2019');
    set(gca,'fontsize',fs);
    xlim([1 length(vec0)+4]);
    yl = ylim; yl(1) = 0; ylim(yl);
end
