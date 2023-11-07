function out = goveqs_scaleup(t, in, M0, M1, times, i, prm, sel, agg)

scale = min(max((t-times(1))/(times(2)-times(1)),0),1);                           % <--- NEW: removed min(,1), so that model can continue extrapolating ART coverage into future
Mt = M1; Mt.lin = M0.lin + scale*(M1.lin-M0.lin);
out = goveqs_basis2(t, in, Mt, i, prm, sel, agg);
