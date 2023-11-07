function out = goveqs_scaleup_disruption(t, in, M0, M1, times, notif_fac, i, prm, sel, agg)

scale = max((t-times(1))/(times(2)-times(1)),0);                           % <--- NEW: removed min(,1), so that model can continue extrapolating ART coverage into future
Mt = M1; Mt.lin = M0.lin + scale*(M1.lin-M0.lin);
% out = goveqs_basis2(t, in, Mt, i, s, p, sel, agg);
out = goveqs_basis_disruption(t, in, Mt, notif_fac, i, prm, sel, agg);