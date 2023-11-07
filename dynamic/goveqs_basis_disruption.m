function [out, lam] = goveqs_basis(t, in, M, notif_fac, i, prm, sel, agg)  % <--- Arguments now incorporating prm 

invec = in(1:i.nstates);

rHIV = interp1(0:length(prm.rHIV)-1, prm.rHIV, (t-1980));                   % <--- NEW

% Normalise by populations
lam = M.lambda*invec/sum(invec);
fac = interp1(2019+(0:length(notif_fac))/12, [1, notif_fac], t);
if isnan(fac)
    keyboard;
end
% Pull them together
allmat = M.lin + M.Dxlin*fac + rHIV*M.linHIV + lam*M.nlin;
out = allmat*invec;

% Implement deaths
morts = sum(M.mortvec,2).*invec;
out = out - morts;

% Implement births
births = sum(morts);
out(i.U.h0) = out(i.U.h0)+births;

% Get the auxiliaries
out(i.aux.inc)  = agg.inc*(sel.inc.*allmat)*invec;
out(i.aux.noti) = agg.noti*(sel.noti.*allmat)*invec;
out(i.aux.mort(1)) = sum(M.mortvec(:,2).*invec);
out(i.aux.mort(2)) = sum(M.mortvec(:,3).*invec);