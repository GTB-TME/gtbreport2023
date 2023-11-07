function [out, lam] = goveqs_basis(t, in, M, i, prm, sel, agg)             

invec = in(1:i.nstates);

if t<1980                                                                  
    rHIV = 0;
else
    rHIV = interp1(0:length(prm.rHIV)-1, prm.rHIV, (t-1980)); 
end
    
try
% Normalise by populations
lam = M.lambda*invec/sum(invec);
allmat = M.lin + M.Dxlin + rHIV*M.linHIV + lam*M.nlin;                     
out = allmat*invec;
catch
   keyboard; 
end

% Implement deaths
morts = sum(M.mortvec,2).*invec;
out = out - morts;

% Implement births
births = sum(morts)*prm.popn_turnover;
out(i.U.h0) = out(i.U.h0)+births;

% Get the auxiliaries
out(i.aux.inc)  = agg.inc*(sel.inc.*allmat)*invec;
out(i.aux.noti) = agg.noti*(sel.noti.*allmat)*invec;
out(i.aux.mort(1)) = sum(M.mortvec(:,2).*invec);
out(i.aux.mort(2)) = sum(M.mortvec(:,3).*invec);