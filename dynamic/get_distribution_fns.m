% Function to find log-density functions matching given values for the 2.5, 50 and 97.5th percentiles
% 'prctiles' is a vector of the [2.5, 50, 97.5]th percentiles
% 'distribution' specifies whether log normal or beta 
% 'visualising' should be set to 'true' if wanting to see the estimated pdf

function [logfn, out, aux] = get_distribution_fns(prctiles, distribution, visualising)

try
dat  = sort(prctiles);
opts = optimset('TolX',1e-12,'TolFun',1e-12,'MaxIter',1e6,'MaxFunEvals',1e6);

% Estimators to help setting up initial guesses
mn = dat(2); var = (dat(3)-dat(1))^2/4;

if strcmp(distribution, 'lognorm')
    ff  = @(x) [logncdf(dat(1),x(1),x(2)), logncdf(dat(2),x(1),x(2)), logncdf(dat(3),x(1),x(2))];
    % Set up initial guess
    sig = sqrt(log(var/mn^2+1)); mu  = log(mn)-sig^2/2; init = [mu sig];
    % Do the optimisation
    obj = @(x) sum((ff(x)./([2.5 50 97.5]/100) - 1).^2);
    [out, val] = fminsearch(obj,init,opts);
    %keyboard;
    if val > 0.1
        error('Calibration setup not converged');
    end
    % Get the log-pdf
    mu = out(1); sig = out(2);
    logfn = @(x) -(log(x)-mu)^2/(2*sig^2) - log(x*sig*sqrt(2*pi));
    
elseif strcmp(distribution, 'beta')
    ff  = @(x) [betacdf(dat(1),x(1),x(2)), betacdf(dat(2),x(1),x(2)), betacdf(dat(3),x(1),x(2))];
    % Set up initial guess
    tmp = mn*(1-mn)/var-1; a = tmp*mn; b = tmp*(1-mn); init = [a b];
    % Do the optimisation
    obj = @(x) sum((ff(x)./([2.5 50 97.5]/100) - 1).^2);
    [out, val] = fminsearch(obj,init,opts);
    if val > 1e-2
        error('Calibration setup not converged');
    end
    % Get the log-pdf
    a = out(1); b = out(2);
    logfn = @(x) (a-1)*log(x) + (b-1)*log(1-x) - betaln(a,b);
end

aux.sim = ff(out);                                                         % Simulated values of CDF at given percentile points
aux.val = val;                                                             % The final objective function

if nargin == 3 && visualising
    figure;
    x = linspace(dat(1)*.8, dat(3)*1.2);
    if strcmp(distribution,'lognorm')
        y = lognpdf(x,out(1),out(2));
    elseif strcmp(distribution,'beta')
        y = betapdf(x,out(1),out(2));
    end
    plot(x,y); hold on;
    % Show the percentiles
    for id = 1:3
        line(dat(id)*[1 1],ylim,'linestyle','--');
    end
end

catch
    keyboard;
end