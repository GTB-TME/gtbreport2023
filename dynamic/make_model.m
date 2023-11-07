function M = make_model(p, r, i, s, gps, opts)

% --- Get the linear rates ------------------------------------------------
m  = zeros(i.nstates);                                                     % Matrix for all linear transitions in the model (i.e. except infection)
m2 = zeros(i.nstates);                                                     % Separate linear matrix for linkage to treatment - this will be multiplied by a time-dependent factor in 'goveqs_basis', to reflect COVID-related disruptions

for ih = 1:length(gps.hiv)
    
    hiv = gps.hiv{ih};
    getst = @(st) i.(st).(hiv);
    
    Lf   = getst('Lf');
    Ls   = getst('Ls');
    I    = getst('I');
    Txs  = getst('Tx');
    Rlo  = getst('Rlo');
    Rhi  = getst('Rhi');
    R    = getst('R');
    
    % --- Fast progression and LTBI stabilisation
    source  = Lf;
    destins = [I,                 Ls];
    rates   = [r.progression(ih), r.LTBI_stabil(ih)];
    m(destins, source) = m(destins, source) + rates';
    
    % --- Reactivation
    source = Ls; destin = I; rate = r.reactivation(ih);
    m(destin, source) = m(destin, source) + rate;
    
    % --- Primary careseeking
    source = I;
    destin = Txs.pu;
    rate   = r.Tx_init(1);
    m2(destin, source) = m2(destin, source) + rate;
    
    source = I;
    destin = Txs.pr;
    rate   = r.Tx_init(2);
    m2(destin, source) = m2(destin, source) + rate;

    % --- Suppleemntary case-finding (assumed from 2014 onwards)
    source = I;
    destin = Txs.pu;
    rate   = r.casefinding;
    m2(destin, source) = m2(destin, source) + rate;
    
    % --- Treatment outcomes
    source  = Txs.pu;
    destins = [Rlo    Rhi];
    rates   = [r.Tx,  r.default];
    m(destins, source) = m(destins, source) + rates';

    source  = Txs.pr;
    destins = [Rlo    Rhi];
    rates   = [r.Tx,  r.default];
    m(destins, source) = m(destins, source) + rates';

    % --- Relapse
    sources = [Rlo, Rhi, R];
    destin  = I;
    rates   = r.relapse;
    m(destin, sources) = m(destin, sources) + rates;
    
    sources = [Rlo, Rhi];
    destin  = R;
    rates   = 0.5;
    m(destin, sources) = m(destin, sources) + rates;
    
    % --- Self cure
    sources = intersect(s.infectious,s.(hiv));
    destin  = Rhi;
    rates   = r.self_cure(ih);
    m(destin, sources) = m(destin, sources) + rates;
    
end

m3 = zeros(i.nstates);                                                     % Separate linear matrix for HIV incidence - this will be multiplied by a time-dependent factor in 'goveqs_basis', to reflect HIV acquisition
if opts.hiv
    % --- HIV acquisition                                                  
    sources = s.h0;
    destins = s.h1;
    inds    = sub2ind(size(m), destins, sources);
    rates   = 1;
    m3(inds) = m3(inds) + rates;
    
    % --- ART initiation
    sources = s.h1;
    destins = s.hart;
    inds    = sub2ind(size(m), destins, sources);
    rates   = r.ART_init;
    m(inds) = m(inds) + rates;
end

% --- Bring them together
M.lin    = sparse(m - diag(sum(m,1)));
M.Dxlin  = sparse(m2 - diag(sum(m2,1)));
M.linHIV = sparse(m3 - diag(sum(m3,1)));                       


% --- Get the nonlinear rates ---------------------------------------------

% --- Allocating transitions
m = zeros(i.nstates);
for ih = 1:length(gps.hiv)
    hiv = gps.hiv{ih};
    sources = intersect([s.U, s.Lf, s.Ls, s.Rlo, s.Rhi, s.R],s.(hiv));
    destin  = i.Lf.(hiv);
    m(destin, sources) = m(destin, sources) + 1;
    % Adjust for any immune protection
    cols = intersect([s.Lf, s.Ls, s.Rlo, s.Rhi, s.R],s.(hiv));
    m(:,cols) = m(:,cols)*(1-p.imm(ih));   
end
% Relative acquisition for HIV
cols = [s.h1, s.hart];
m(:,cols) = m(:,cols)*p.HIVlam;
M.nlin = sparse(m - diag(sum(m,1)));


% --- Getting force-of-infection
m = zeros(1,i.nstates);
m(intersect(s.infectious,s.h0)) = r.beta(1);
m(intersect(s.infectious,[s.h1,s.hart])) = r.beta(2);
M.lambda = sparse(m);


% --- Get the mortality rates
m         = zeros(i.nstates,3);
m(:,1)    = r.mort;
m(s.h1,1) = r.HIV_mort;                                                    % HIV+ve, no TB

% HIV-ve TB
inds = intersect(s.h0, s.infectious);
m(inds,2) = r.mort_TB(1);                                          

% HIV/TB coinfection
inds = intersect([s.h1,s.hart], s.infectious);
m(inds,3) = r.mort_TB(2);                                                  
M.mortvec = m;