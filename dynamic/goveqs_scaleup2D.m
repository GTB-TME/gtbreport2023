function out = goveqs_scaleup2D(t, in, M0, M1, M2, times, i, prm, sel, agg)

scale = max((t-times(:,1))./(times(:,2)-times(:,1)),0);                    % <--- NEW: removed min(,1), so that model can continue extrapolating ART coverage into future
scale(1) = min(scale(1),1);

Mt = M1; Mt.lin = M0.lin + scale(1)*(M1.lin-M0.lin) + scale(2)*(M2.lin-M0.lin);
out = goveqs_basis2(t, in, Mt, i, prm, sel, agg);
