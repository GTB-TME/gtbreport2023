function [out, aux] = get_objective2D(x, prm, ref, sel, agg, gps, calfn, opts)

r = prm.r; p = prm.p; i = ref.i; s = ref.s; xi = ref.xi;
mat = [prm.bounds(2,1:length(x))-x; x-prm.bounds(1,1:length(x))];

% if min(mat(:)) < 0
if 1<0
    out = Inf*calfn.sgn;
    aux = [];
else
    
    [r,p,prm] = alloc_parameters(x,r,p,prm,xi,opts);
    
    % --- Set up the necessary models -------------------------------------
    
    if opts.hiv || opts.provs || opts.const
        
        % Pre-ART, pre-pu conditions
        p0 = p; r0 = r;
        r0.ART_init = 0;
        r0.Tx_init(1) = r0.Tx_init(1)*(1-opts.provs);
        M0 = make_model(p0, r0, i, s, gps, opts);
        
        % Public sector, no ART
        p1 = p; r1 = r;
        r1.ART_init = 0;
        M1 = make_model(p1, r1, i, s, gps, opts);
        
        % ART, no public sector
        p2 = p; r2 = r;
        r2.Tx_init(1) = r2.Tx_init(1)*(1-opts.provs);
        M2 = make_model(p2, r2, i, s, gps, opts);
        
        %keyboard;
        
        % --- Simulate the models
        
        % Equilibrium model
        init = zeros(1,i.nx); seed = 1e-6;
        init(i.U.h0) = (1-seed); init(i.I.h0) = seed;
        geq = @(t,in)goveqs_basis2(t, in, M0, i, prm, sel, agg);
        [t0, soln0] = ode15s(geq, [0:2e3], init, odeset('NonNegative',[1:i.nstates]));
        
        % Model programmatic changes after 2000: growth of the public sector
        % from 2000 - 2009 (for public/private models), and implementation of ART
        % (for TB/HIV models)
        init  = soln0(end,:);
        tinit = min([prm.ART_start, 2000]);
        
        geq = @(t,in) goveqs_scaleup2D(t, in, M0, M1, M2, [2000 2009; prm.ART_start 2019], i, prm, sel, agg);
        [t, soln] = ode15s(geq, [tinit:2019], init, odeset('NonNegative',[1:i.nstates]));

%         tinit = 1997;
%         geq = @(t,in) goveqs_scaleup2D(t, in, M0, M1, M2, [1997 2007; prm.ART_start 2019], i, prm, sel, agg);
%         [t, soln] = ode15s(geq, [tinit:2020], init, odeset('NonNegative',[1:i.nstates]));
        
        allin = -M0.lin + M1.lin + M2.lin;
    else
        
        % Pre ramp-up in casefinding
        p0 = p; r0 = r;
        r0.casefinding = 0;
        M0 = make_model(p0, r0, i, s, gps, opts);
        
        % Post ramp-up in case-finding
        p1 = p; r1 = r;        
        M1 = make_model(p1, r1, i, s, gps, opts);
        
        % Equilibrium model
        init = zeros(1,i.nx); seed = 1e-6;
        init(i.U.h0) = (1-seed); init(i.I.h0) = seed;
        geq = @(t,in)goveqs_basis2(t, in, M0, i, prm, sel, agg);
        [t0, soln0] = ode15s(geq, [0:2e3], init, odeset('NonNegative',[1:i.nstates]));
        
        % Model any increase in case-finding after 2014
        init  = soln0(end,:);
        geq = @(t,in) goveqs_scaleup(t, in, M0, M1, [2014 2019], i, prm, sel, agg);
        [t, soln] = ode15s(geq, [2010:2019], init, odeset('NonNegative',[1:i.nstates]));

        allin = M1.lin;
    end
    
    
    sfin  = soln(end,:);
    sdiff = diff(soln,1);
    
    % --- Get the objectives ----------------------------------------------
    
    inc_all_2019 = sum(sdiff(end,i.aux.inc))*1e5;
    inc_all_2014 = sum(sdiff(end-5,i.aux.inc))*1e5;
    inc_h1       = sum(sdiff(end,i.aux.inc([2,3])))*1e5;
    noti         = sdiff(end,i.aux.noti)*1e5;
    noti_pu      = noti(1);
    prop_pu      = noti(1)/sum(noti);
    
    ART_covg = sum(sfin(s.hart)/sum(sfin([s.h1,s.hart])));
    HIV_prev = sum(sfin([s.h1, s.hart]))/sum(sfin(1:i.nstates));
    mort_H0  = sdiff(end,i.aux.mort(1))*1e5;
    mort_H1  = sdiff(end,i.aux.mort(2))*1e5;
    
    if inc_all_2019 < 1
        out = Inf*calfn.sgn;
        aux = [];
    else
        
        if opts.hiv
            out = calfn.fn(inc_all_2019, mort_H0, noti_pu, inc_h1, ART_covg, HIV_prev, mort_H1);
        elseif opts.provs
            out = calfn.fn(inc_all_2019, mort_H0, noti_pu);
        else
            out = calfn.fn(inc_all_2019, inc_all_2014, mort_H0, noti_pu);
        end
        if opts.provs 
            out = out + calfn.fn_pu(prop_pu);
        end

        % --- Get additional outputs and package --------------------------
        aux.soln         = [soln, t];
        aux.allsol       = [soln0; soln(2:end,:)];
        aux.soln0        = soln0;
        aux.inc_all_2019 = inc_all_2019;
        aux.inc_all_2014 = inc_all_2014;
        aux.inct         = sum(sdiff(:,i.aux.inc),2)*1e5;
        aux.inc_h1       = inc_h1;
        aux.noti         = noti;
        aux.noti_pu      = noti_pu;
        aux.prop_pu      = prop_pu;
        aux.ART_covg     = ART_covg;
        aux.HIV_prev     = HIV_prev;
        aux.mort_H0      = mort_H0;
        aux.mort_H1      = mort_H1;
        %aux.M1       = M1;
        aux.prev         = sum(sfin(s.prevalent))*1e5;
        aux.allin        = allin;
        aux.M0           = M0;
    end        
    %keyboard;
    
    if opts.provs || opts.hiv
       aux.M1 = M1;
       aux.M2 = M2;
    else
        aux.M1 = M1;
    end
    
    
end
