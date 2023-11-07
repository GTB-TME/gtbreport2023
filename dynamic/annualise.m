function [out, out_pct] = annualise(inmat,annudim,take_pct,pctdim)

dims = size(inmat);
if annudim == 1
    shape = [12, dims(1)/12, dims(2:end)];
elseif annudim == length(dims)
    shape = [dims(1:annudim-1), 12, dims(end)/12];
else
    shape = [dims(1:annudim-1), 12, dims(annudim)/12, dims(annudim+1:end)];
end

tmp1 = reshape(inmat, shape);
tmp2 = sum(tmp1,annudim);
out  = squeeze(tmp2);

% keyboard;

% Also take percentiles if needed
if take_pct
    out_pct = squeeze(prctile(out,[2.5,50,97.5],pctdim));
else
    out_pct = [];
end