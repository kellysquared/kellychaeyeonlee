data = readtable("/Users/kellychaeyeonlee/Downloads/compiled_chocolatenir.csv");

% Load the data
X = table2array(data(:, 3:end)); % Predictor variables
Y = table2array(data(:, 2)); % Response variable

% Remove any rows with NaN values in Y
nonNanIndices = ~isnan(Y);
X = X(nonNanIndices, :);
Y = Y(nonNanIndices, :);

% Kennard-Stone Algorithm
function [trainInd, testInd] = kennardStone(X, numSamples)
    numData = size(X, 1);
    trainInd = zeros(numSamples, 1);
    testInd = (1:numData)';
    D = pdist2(X, X, 'euclidean');
    [~, maxIdx] = max(D(:));
    [i, j] = ind2sub([numData, numData], maxIdx);
    trainInd(1) = i;
    trainInd(2) = j;
    testInd([i, j]) = [];
    for k = 3:numSamples
        remainingSamples = X(trainInd(1:k-1), :);
        Dmin = min(pdist2(X(testInd, :), remainingSamples, 'euclidean'), [], 2);
        [~, nextIdx] = max(Dmin);
        trainInd(k) = testInd(nextIdx);
        testInd(nextIdx) = [];
    end
end

% Number of samples for the training set
num_train_samples = round(0.7 * size(X, 1));

% Split data
[trainInd, testInd] = kennardStone(X, num_train_samples);

% Create training and test sets
X_train = X(trainInd, :);
Y_train = Y(trainInd, :);
X_test = X(testInd, :);
Y_test = Y(testInd, :);

% Save to workspace
save('trainTestSplit.mat', 'X_train', 'Y_train', 'X_test', 'Y_test');

vip_scores = vip(plsmodel)
disp(vip_scores);
save('VIP_scores.mat', 'vip_scores');
% load the VIP scores from the .mat file
load('VIP_scores.mat');
% exclude the data before index 100
filtered_VIP_scores = vip_scores(100:length(vip_scores)); %
%plot
plot(100:length(vip_scores), filtered_VIP_scores); % plot from index 100 onward
xlabel('Wavelength Index');
ylabel('VIP Score');
title('Filtered VIP Scores for Each Wavelength');
grid on;
hold on;
yline(1, 'r--');  % add a horizontal line at VIP score ===1

% identify important wavelengths excluding  noisy region
importantWavelengths = find(vip_scores(100:end) > 1) + 99;  % adjust indices back to original scale
disp('Important Wavelengths excluding noisy data:');
disp(importantWavelengths);

% function to group consecutive indices
function ranges = groupConsecutive(indices)
   if isempty(indices)
       ranges = {};
       return;
   end
   d = diff(indices);
   gaps = find(d > 1);
   edges = [0, gaps, numel(indices)];
   ranges = arrayfun(@(i) indices(edges(i)+1):indices(edges(i+1)), 1:numel(edges)-1, 'UniformOutput', false);
end
importantRanges = groupConsecutive(importantIndices);
% Display the ranges
disp('Important Wavelength Ranges:');
for i = 1:length(importantRanges)
   fprintf('%d-%d\n', importantRanges{i}(1), importantRanges{i}(end));
end

% Save the important wavelengths to a file,
save('importantWavelengths_filtered.txt', 'importantWavelengths');
