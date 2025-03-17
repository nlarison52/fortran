% Combined MATLAB script with continuous slider updates, looping animation, and larger heatmap:
% - Animated heatmap of Ez field
% - Power over time at receiver point
clear, close all, clc

% Real-world unit conversions
c = 3e8;                  % Speed of light (m/s)
dx_real = 1e-3;           % Spatial step size (1 mm)
dt_real = dx_real / (2*c); % Time step based on CFL condition

% --- Read Heatmap Data (output.dat) ---
filename = 'output.dat';
fileID = fopen(filename, 'r');
if fileID == -1
    error('Cannot open file: %s', filename);
end

% Initialize variables for heatmap
nx = 1000;
ny = 1000;
Ez = zeros(nx, ny);
timesteps = [];
data_blocks = {};

fprintf('Reading heatmap data...');
count = 0;
while ~feof(fileID)
    line = fgetl(fileID);
    if ischar(line) && contains(line, 'Timestep:')
        line = strtrim(line);
        parts = split(line, ':');
        timestep = str2double(strtrim(parts{2}));
        if isnan(timestep)
            error('Failed to parse timestep from line: %s', line);
        end
        timesteps = [timesteps, timestep];
        
        block = zeros(nx, ny);
        for i = 1:nx
            data_line = fgetl(fileID);
            if ischar(data_line)
                values = sscanf(data_line, '%f');
                if length(values) ~= ny
                    error('Expected %d values for i=%d, got %d', ny, i, length(values));
                end
                block(i, :) = values(:)';
            else
                error('Missing data line for i=%d after timestep %d', i, timestep);
            end
        end
        data_blocks{end+1} = block;
        
        count = count + 1;
        if mod(count, 10) == 0
            fprintf('.');
        end
    end
end
fclose(fileID);
fprintf(' done!\n');
fprintf('Number of heatmap timesteps found: %d\n', length(timesteps));

if isempty(timesteps)
    error('No timesteps found in output.dat. Check file format.');
end

% --- Read Radar Data (radar.dat) ---
radFileName = 'radar.dat';
fileID = fopen(radFileName, 'r');
if fileID == -1
    error('Cannot open file: %s', radFileName);
end

data = fscanf(fileID, '%f %f', [2, Inf]);
fclose(fileID);
data = data';
radar_timesteps = data(:, 1);
Ez_values = abs(data(:, 2));

fprintf('Number of radar timesteps found: %d\n', length(radar_timesteps));

% --- Physical Axes ---
x_axis = (1:nx) * dx_real; % meters
y_axis = (1:ny) * dx_real; % meters
time_axis = radar_timesteps * dt_real * 1e9; % nanoseconds

% --- Visualization Setup ---
fig = figure('Name', 'FDTD Simulation - Heatmap and Radar Power', ...
    'NumberTitle', 'off', 'Position', [100, 100, 1000, 900]);

% Subplot 1: Heatmap (larger)
axes_heatmap = axes('Position', [0.1, 0.25, 0.8, 0.65]);
colormap('jet');
h_heatmap = imagesc(x_axis, y_axis, data_blocks{1});
colorbar;
caxis([-1.6, 1.6]);
title(sprintf('Ez Field at %.3f ns', timesteps(1) * dt_real * 1e9));
xlabel('X Position (m)');
ylabel('Y Position (m)');
axis equal tight;

% Subplot 2: Radar Power (smaller)
axes_power = axes('Position', [0.1, 0.1, 0.8, 0.13]);
h_power = plot(time_axis, Ez_values, '-b', 'LineWidth', 1.5);
hold on;
h_marker = plot(time_axis(1), Ez_values(1), 'ro', 'MarkerSize', 8, 'LineWidth', 2);
grid on;
title('Radar Received Signal (Ez Magnitude vs. Time)');
xlabel('Time (ns)');
ylabel('Ez Magnitude');
xlim([min(time_axis), max(time_axis)]);
% Handle zero Ez_values
max_val = max(Ez_values);
if max_val == 0
    ylim([0, 1]); % Default range if all zeros
else
    ylim([0, max_val * 1.1]);
end

% --- Add Slider and Play/Pause Button ---
% Slider
h_slider = uicontrol('Style', 'slider', 'Parent', fig, ...
    'Position', [150, 20, 500, 20], 'Min', 1, 'Max', length(timesteps), ...
    'Value', 1, 'SliderStep', [1/(length(timesteps)-1), 10/(length(timesteps)-1)]);

% Play/Pause Button
h_playButton = uicontrol('Style', 'pushbutton', 'Parent', fig, ...
    'Position', [50, 20, 80, 20], 'String', 'Play');

% Store data in figure for callbacks
data = struct('isPlaying', false, 'h_slider', h_slider, 'h_playButton', h_playButton, ...
    'h_heatmap', h_heatmap, 'h_power', h_power, 'h_marker', h_marker, ...
    'data_blocks', {data_blocks}, 'timesteps', timesteps, ...
    'radar_timesteps', radar_timesteps, 'time_axis', time_axis, 'Ez_values', Ez_values, ...
    'dt_real', dt_real, 'axes_heatmap', axes_heatmap, 'axes_power', axes_power);
guidata(fig, data);

% Set callbacks
set(h_playButton, 'Callback', @(src, event) playPauseCallback(src, fig));
addlistener(h_slider, 'Value', 'PostSet', @(src, event) sliderCallback(fig));

% Initial plot update
updatePlots(1, data, fig);

disp('Use the slider to step through the simulation or press Play to animate (loops until paused).');

% --- Callback Functions ---
function sliderCallback(fig)
    data = guidata(fig);
    t = round(get(data.h_slider, 'Value'));
    updatePlots(t, data, fig);
end

function playPauseCallback(~, fig)
    data = guidata(fig);
    if data.isPlaying
        data.isPlaying = false;
        set(data.h_playButton, 'String', 'Play');
    else
        data.isPlaying = true;
        set(data.h_playButton, 'String', 'Pause');
        guidata(fig, data);
        playAnimation(fig);
    end
    guidata(fig, data);
end

function playAnimation(fig)
    data = guidata(fig);
    while data.isPlaying
        t = round(get(data.h_slider, 'Value'));
        updatePlots(t, data, fig);
        t = t + 1;
        if t > length(data.timesteps)
            t = 1;
        end
        set(data.h_slider, 'Value', t);
        pause(0.05);
        drawnow;
        data = guidata(fig);
    end
    guidata(fig, data);
end

function updatePlots(t, data, fig)
    % Update heatmap
    set(data.axes_heatmap, 'NextPlot', 'replacechildren');
    set(data.h_heatmap, 'CData', data.data_blocks{t});
    title(data.axes_heatmap, sprintf('Ez Field at %.3f ns', data.timesteps(t) * data.dt_real * 1e9));
    
    % Find closest radar timestep
    [~, idx] = min(abs(data.radar_timesteps - data.timesteps(t)));
    
    % Update power plot
    set(data.axes_power, 'NextPlot', 'replacechildren');
    set(data.h_power, 'XData', data.time_axis, 'YData', data.Ez_values);
    set(data.h_marker, 'XData', data.time_axis(idx), 'YData', data.Ez_values(idx));
end