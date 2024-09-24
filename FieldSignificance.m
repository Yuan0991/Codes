% ----------------------------------------------------------------------- %
%       Benjamini-Hochberg correction of the False Discovery Rate         %
%                                                                         %
% Benjamini, Y., & Hochberg, Y. (1995). Controlling the false discovery   %
% rate : a practical and powerful approach to multiple testing. Journal   %         
% of the Royal statistical society: series B(Methodological), 57(1),      %
% 289-300.                                                                %
% ----------------------------------------------------------------------- %
%% ESM
clearvars
root_path = 'H:\Publications\Cal';
data = 'ESM';
model = {'CMCC_CM', 'CMCC_ES', 'Nor_LM', 'Nor_MM'};
for m=1:size(model,2)
stat= 'Partial correlation';
path = fullfile(root_path,'\','Partial correlation', '\', data,'\',char(model(m)),'\');
Dir=dir(path);
for filename=[3,4,7,8,11,12]
    file=[Dir(filename).name ];
    pp=readmatrix([path file]);
    for c=1:size(pp,2)
    [c_pvalues, c_alpha, h] = fdr_BH(pp(:,c), 0.05, false);
    c_pval (:,c)=c_pvalues';
    end
    c_pval(c_pval>1)=0.99 + (1-0.99) .* rand; % setting an upper limit for p-values
   
% Check if folder exist.
% if not, create a new folder
folder_dir = fullfile(root_path,'FieldSig',string(data),string(model(m)));
if not(isfolder(folder_dir))
    mkdir(folder_dir)
end

new_file_str = regexp(file, '\.', 'split');
output_name = strcat(char(new_file_str(1)),'_corrected','.', 'txt');
writematrix(c_pval,fullfile(folder_dir,output_name),'delimiter', '\t')   
end
clearvars -except root_path data model
end
%% FLUXCOM
clearvars
root_path = 'H:\Publications\Cal';
data = 'FLUXCOM';
model = {'ANN', 'MAR', 'RF'};
for m=1:size(model,2)
stat= 'Partial correlation';
path = fullfile(root_path,'\','Partial correlation', '\', data,'\',char(model(m)),'\');
Dir=dir(path);
for filename=[3,4,7,8,11,12]
    file=[Dir(filename).name ];
    pp=readmatrix([path file]);
    for c=1:size(pp,2)
    [c_pvalues, c_alpha, h] = fdr_BH(pp(:,c), 0.05, false);
    c_pval (:,c)=c_pvalues';
    end
    c_pval(c_pval>1)=0.99 + (1-0.99) .* rand; % setting an upper limit for p-values
    
% Check if folder exist.
% if not, create a new folder
folder_dir = fullfile(root_path,'FieldSig',string(data),string(model(m)));
if not(isfolder(folder_dir))
    mkdir(folder_dir)
end

new_file_str = regexp(file, '\.', 'split');
output_name = strcat(char(new_file_str(1)),'_corrected','.', 'txt');
writematrix(c_pval,fullfile(folder_dir,output_name),'delimiter', '\t')   
end
clearvars -except root_path data model
end
%% FDR function
function [c_pvalues, c_alpha, h, extra] = fdr_BH(pvalues, alpha, plotting)
% ----------------------------------------------------------------------- %
%   Input parameters:                                                     %
%       - pvalues:      List of p-values to correct.                      %
%       - alpha:        Significance level (commonly, alpha=0.05).        %
%       - plotting:     (Optional, default=false) Plotting boolean.       %
%                                                                         %
%   Output variables:                                                     %
%       - c_pvalues:    Corrected p-values (that should be compared with  %
%                       the given alpha value to test the hypotheses).    %
%       - c_alpha:      Corrected significance levels (that should be     %
%                       compared with the given pvalues to test the       %
%                       hypotheses).                                      %
%       - h:            Hypothesis rejection. If h=1, H0 is rejected; if  %
%                       h=0, H0 is accepted.                              %
%       - extra:        Struct that contains additional information.      %
% ----------------------------------------------------------------------- %
    
    % Error detection
    if nargin < 3, plotting = false; end
    if nargin < 2, error('Not enough parameters.'); end
    if ~isnumeric(pvalues) && ~isnumeric(alpha)
        error('Parameters pvalues and alpha must be numeric.');
    end
    pvalues = pvalues(:);
    if length(pvalues) < 2, error('Not enough tests to perform the correction.'); end
    if ~islogical(plotting), error('Plotting parameter must be a boolean'); end
        
    % Parameters
    m = length(pvalues);    % No. tests
    
    % Compute the adjusted p-values
    k = (1:1:m)';
    [s_pvalues, idx] = sort(pvalues,'ascend');
    s_c_pvalues = cummin(s_pvalues.*(m./k), 'reverse');
    c_pvalues(idx) = s_c_pvalues;
    
    % Compute the corrected significance levels
    s_c_alpha = alpha.*k./m;
    c_alpha(idx) = s_c_alpha;
    
    % Rejected H0
    h = pvalues(:) < c_alpha(:);

    % Extra information
    extra.s_pvalues = s_pvalues;
    extra.s_c_pvalues = s_c_pvalues;
    extra.s_c_alpha = s_c_alpha;
    extra.alpha = alpha;
    extra.pvalues = pvalues;
    
    % Plotting
    if plotting
        figure;
        subplot(1,2,1);
        l1 = plot(0:0.01:m, (0:0.01:m).*alpha/m,'r','linewidth',2); hold on;
        for i = 1:m
            plot(i, s_pvalues(i),'ob','linewidth',1.5); hold on;
            plot([i i], [0 s_pvalues(i)],'b','linewidth',1.5); hold on;
        end
        xlabel('k'); ylabel('p_k'); title('BH'); grid on;
        legend(l1,'y=\alphax/m');
        
        subplot(1,2,2);
        l2 = plot([0 m], [alpha alpha],'--r','linewidth',2); hold on;
        for i = 1:m
            plot(i, s_c_pvalues(i),'ob','linewidth',1.5); hold on;
            plot([i i], [0 s_c_pvalues(i)],'b','linewidth',1.5); hold on;
        end
        xlabel('k'); ylabel('adjusted p_k'); title('BH adjustment'); grid on;
        legend(l2,{'y=\alpha'});
        
        figure;
        subplot(2,2,1:2);
        plot(s_pvalues, s_c_pvalues, 'b', 'linewidth',2);
        ylabel('Adj. p-values'); xlabel('p-values');
        title('Benjamini-Hochberg');
        
        subplot(2,2,3);
        hist(pvalues); xlabel('p-values'); ylabel('Histogram');
        
        subplot(2,2,4);
        hist(c_pvalues); xlabel('Adj. p-values'); ylabel('Histogram');
    end
end