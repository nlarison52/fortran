clear, close all, clc;

% File names
radFileName45 = "radar.dat";      % Steering angle 45 degrees
radFileName0 = "radar_zero.dat";  % Steering angle 0 degrees

% Read data for 45 degrees
fileID45 = fopen(radFileName45, 'r');
if fileID45 == -1
    error('Cannot open file: %s', radFileName45);
end
data45 = fscanf(fileID45, '%f %f', [2, Inf]);
fclose(fileID45);
data45 = data45';  % Transpose for easier access
timestep45 = data45(:, 1);
Ez_values45 = abs(data45(:, 2));

% Read data for 0 degrees
fileID0 = fopen(radFileName0, 'r');
if fileID0 == -1
    error('Cannot open file: %s', radFileName0);
end
data0 = fscanf(fileID0, '%f %f', [2, Inf]);
fclose(fileID0);
data0 = data0';  % Transpose for easier access
timestep0 = data0(:, 1);
Ez_values0 = abs(data0(:, 2));

% Calculate average power (proportional to Ez^2)
avg_power45 = mean(Ez_values45.^2);
avg_power0 = mean(Ez_values0.^2);
power_diff = avg_power0 - avg_power45;  % 0° minus 45° for "0° has more" logic

% Determine the power difference message
if power_diff >= 0
    power_msg = sprintf('The 0° incidence exhibits %.4f higher average power', power_diff);
else
    power_msg = sprintf('The 45° incidence exhibits %.4f higher average power', -power_diff);
end

% Plotting both in subplots
figure('Position', [100, 100, 800, 400]);

% Subplot 1: Steering angle 0 degrees (left)
subplot(1, 2, 1);
plot(timestep0, Ez_values0, '-r', 'LineWidth', 1.5);  % Red for 0°
grid on;
title('Radar Received Signal (0° Steering)');
xlabel('Timestep');
ylabel('Ez Magnitude');
ylim([0, max(max(Ez_values45), max(Ez_values0)) * 1.1]);

% Subplot 2: Steering angle 45 degrees (right)
subplot(1, 2, 2);
plot(timestep45, Ez_values45, '-b', 'LineWidth', 1.5);  % Blue for 45°
grid on;
title('Radar Received Signal (45° Steering)');
xlabel('Timestep');
ylabel('Ez Magnitude');
ylim([0, max(max(Ez_values45), max(Ez_values0)) * 1.1]);

% Link axes for synchronized zooming
linkaxes([subplot(1, 2, 1), subplot(1, 2, 2)], 'x');

% Display average power difference in a polished super-title
sgtitle(power_msg, 'FontSize', 12, 'FontWeight', 'bold');