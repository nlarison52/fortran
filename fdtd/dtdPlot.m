% MATLAB script to visualize FDTD simulation as an animated heatmap with realistic units
clear, close all, clc

% Real-world unit conversions
c = 3e8;                  % Speed of light (m/s)
dx_real = 1e-3;          % Spatial step size (1 mm)
dt_real = dx_real / (2*c); % Time step based on CFL condition

% Open the output file
filename = 'output.dat';
fileID = fopen(filename, 'r');
if fileID == -1
    error('Cannot open file: %s', filename);
end

% Initialize variables
nx = 200;
ny = 200;
Ez = zeros(nx, ny);
timesteps = [];
data_blocks = {};

% Read the file
fprintf('Reading simulation data...');
count = 0;
while ~feof(fileID)
    line = fgetl(fileID);
    if ischar(line) && contains(line, 'Timestep:')
        % Extract timestep number
        line = strtrim(line);
        parts = split(line, ':');
        timestep = str2double(strtrim(parts{2}));
        if isnan(timestep)
            error('Failed to parse timestep from line: %s', line);
        end
        timesteps = [timesteps, timestep];
        
        % Read nx lines for Ez data
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
        
        % Simple progress indicator
        count = count + 1;
        if mod(count,10)==0
            fprintf('.');
        end
    end
end
fclose(fileID);
fprintf(' done!\n');
fprintf('Number of timesteps found: %d\n', length(timesteps));

% Check if data was found
if isempty(timesteps)
    error('No timesteps found in output.dat. Check file format.');
end

% Physical axes (converted to meters)
x_axis = (1:nx) * dx_real;
y_axis = (1:ny) * dx_real;

% Visualization setup
figure('Name', 'FDTD Simulation - Ez Field Heatmap', 'NumberTitle', 'off');
colormap('jet');
h = imagesc(x_axis, y_axis, data_blocks{1});
colorbar;
caxis([-1.6, 1.6]);
title(sprintf('Ez Field at %.3f ns', timesteps(1)*dt_real*1e9));
xlabel('X Position (m)');
ylabel('Y Position (m)');
axis equal tight;

% Infinite animation loop
disp('Starting animation... Press Ctrl+C to stop.');
while true
    for t = 1:length(timesteps)
        set(h, 'CData', data_blocks{t});
        title(sprintf('Ez Field at %.3f ns', timesteps(t)*dt_real*1e9));
        pause(0.05);
        drawnow;
    end
end