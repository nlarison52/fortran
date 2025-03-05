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
disp('Reading simulation data...');
while ~feof(fileID)
    line = fgetl(fileID);
    if ischar(line) && contains(line, 'Timestep:')
        % Extract timestep number, handling extra spaces
        line = strtrim(line); % Remove leading/trailing spaces
        parts = split(line, ':'); % Split at colon
        timestep = str2double(strtrim(parts{2})); % Convert second part to number
        if isnan(timestep)
            error('Failed to parse timestep from line: %s', line);
        end
        fprintf('Found timestep: %d\n', timestep); % Debug output
        timesteps = [timesteps, timestep];
        
        % Read the next nx lines for Ez data (200 values per line)
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
    end
end
fclose(fileID);
disp('Data reading complete.');
fprintf('Number of timesteps found: %d\n', length(timesteps));

% Check if data was found
if isempty(timesteps)
    error('No timesteps found in output.dat. Check file format.');
end

% Hardcode the range for Ez color scaling
caxis_limit = [-1.6, 1.6];
fprintf('Color axis limits hardcoded to: [%f, %f]\n', caxis_limit(1), caxis_limit(2));

% Set up the figure for animation
figure('Name', 'FDTD Simulation - Ez Field Heatmap', 'NumberTitle', 'off');
colormap('jet');
h = imagesc(data_blocks{1}); % Use first block as initial heatmap
colorbar;
caxis(caxis_limit); % Apply hardcoded color limits
title(sprintf('Ez Field (Timestep %d)', timesteps(1)));
xlabel('X Grid');
ylabel('Y Grid');
axis equal tight;

% Animation loop with continuous cycling
disp('Starting animation... Press Ctrl+C to stop.');
while true
    for t = 1:length(timesteps)
        set(h, 'CData', data_blocks{t});
        title(sprintf('Ez Field (Timestep %d)', timesteps(t)));
        pause(0.05);
        drawnow;
    end
end