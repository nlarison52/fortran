% MATLAB script to visualize FDTD simulation as an animated heatmap with continuous looping
clear, close all, clc

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

% Hardcoded color limits for visualization
caxis_limit = [-1.6, 1.6];

% Visualization setup
figure('Name', 'FDTD Simulation - Ez Field Heatmap', 'NumberTitle', 'off');
colormap('jet');
h = imagesc(data_blocks{1});
colorbar;
caxis(caxis_limit);
title(sprintf('Ez Field (Timestep %d)', timesteps(1)));
xlabel('X Grid');
ylabel('Y Grid');
axis equal tight;

% Infinite animation loop
disp('Starting animation... Press Ctrl+C to stop.');
while true
    for t = 1:length(timesteps)
        set(h, 'CData', data_blocks{t});
        title(sprintf('Ez Field (Timestep %d)', timesteps(t)));
        pause(0.05);
        drawnow;
    end
end