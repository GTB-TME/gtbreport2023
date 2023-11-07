clear all;
% warning off;

% -------------------------------------------------------------------------
% --- New infections ------------------------------------------------------

C = readtable('New_infections.xlsx');

countries1 = C.Country;
dat = C{:,2:end};

% Organise into hi, mid, lo
for ico = 1:size(dat,1)
    mat = zeros(size(dat,2),3);
    for iyr = 1:size(dat,2)
        out = get_numbers(dat{ico,iyr});
        out(isnan(out)) = 0;
        if ~isempty(out)
            mat(iyr,:) = out;
        end
    end

    % Extrapolate linearly from 1980 to 1990
    yr0  = 1980;
    Ys   = [zeros(1,3); mat(1,:)];
    Xs   = [1980 1990];
    ms   = diff(Ys,1)/diff(Xs);
    cs   = Ys(1)-ms*Xs(1);
    xpts = [Xs(1):1:Xs(2)]';
    ypts = xpts.*ms + cs;
    mat2 = [ypts; mat];

    % Extrapolate forwards by 15 years - NB: this isn't quite right, since
    % for some countries (e.g. Ukraine) it gives different trends for percentiles
    % and central estimates. OK for now, since we're only using the central
    % estimates
    xs1  = 1:size(mat2,1);
    xs2  = 1:size(mat2,1)+15;
    mat3 = max(interp1(xs1,mat2,xs2,'linear','extrap'),0);

    HIV_incd(:,:,ico) = mat3;

end

% -------
% -------
% -------
% -------------------------------------------------------------------------
% --- Estimates on ART coverage -------------------------------------------

C = readtable('ART coverage.xlsx');
countries2 = C.Country;

% Get 2019 data
dat = [C.x2010, C.x2019];

% Organise into hi, mid, lo
for ico = 1:size(dat,1)
    mat = zeros(2,3);
    for iyr = [1:2]
        out = get_numbers(dat{ico,iyr});
        if ~isempty(out)
            mat(iyr,:) = out;
        end
    end
    ARTcovg_2019(ico,:) = mat(2,:);

    % Do extrapolation backwards to figure out when ART started
    Ys     = mat(:,2);
    Xs     = [2010 2019];
    ms     = diff(Ys)/diff(Xs);
    cs     = Ys(1)-ms*Xs(1);
    ART_start(ico) = floor(-cs/ms);
    
end

% -------
% -------
% -------
% -------------------------------------------------------------------------
% --- Estimates on HIV prevalence -----------------------------------------

C = readtable('HIV prevalence.xlsx');
countries3 = C.Country;

% Get 2019 data
dat = C.x2019;

% Organise into hi, mid, lo
for ico = 1:size(dat,1)
    out = get_numbers(dat{ico});
    if ~isempty(out)
        HIVprev_2019(ico,:) = out;
    end
end



% --- Bring them all together and save
save HIV_estims countries1 countries2 countries3 HIV_incd ARTcovg_2019 ART_start HIVprev_2019

