clear, close, clc;

radFileName = "radar.dat";

fileID = fopen(radFileName, 'r');

if fileID == -1
    error('Cannot open file: %s', radFileName);
end

% Read data (assuming format: timestep Ez_value)
data = fscanf(fileID, '%f %f', [2, Inf]);
fclose(fileID);

% Transpose data for easier access
data = data';

% Extract timestep and Ez values
timestep = data(:, 1);
Ez_values = abs(data(:, 2));

% Plotting
figure;
plot(timestep, Ez_values, '-b', 'LineWidth', 1.5);
grid on;
title('Radar Received Signal (Ez vs. Timestep)');
xlabel('Timestep');
ylabel('Ez Magnitude');