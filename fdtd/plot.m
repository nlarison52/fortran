clc; clear; close all;

% Open the data file
filename = 'output.dat';
fid = fopen(filename, 'r');

if fid == -1
    error('Failed to open file: %s\nCheck if the file exists and that you are in the correct directory.', filename);
end

% Read file line by line
data = [];
timestep = [];
nx = 100; ny = 100; % Make sure this matches your Fortran grid size
frame = 0;
object_rect = [];

while ~feof(fid)
    line = fgetl(fid);
    
    if contains(line, 'Object')  % Read object coordinates
        object_rect = sscanf(line, 'Object: %d %d %d %d');  % Extract rectangle bounds
    elseif contains(line, 'Timestep')  % Read timestep information
        frame = frame + 1;
        timestep(frame) = str2double(extractAfter(line, ':'));
        data(:,:,frame) = zeros(nx, ny); % Initialize frame data
        row = 1;
    else
        nums = sscanf(line, '%f');
        if ~isempty(nums)
            data(row,:,frame) = nums'; % Read row data into the frame
            row = row + 1;
        end
    end
end

fclose(fid);

% Create a figure and colormap
figure;
colormap('jet');
colorbar;
caxis([-1 1]);  % Adjust color scale to match Ez values

% Set up the heat map
h = imagesc(data(:,:,1));  % Initialize with the first frame
title(['Timestep: ', num2str(timestep(1))]);
xlabel('Grid X');
ylabel('Grid Y');

% Infinite animation loop
while true
    for frame = 1:length(timestep)
        set(h, 'CData', data(:,:,frame));  % Update heatmap data
        title(['Timestep: ', num2str(timestep(frame))]);

        % Draw the object as a rectangle
        hold on;
        if ~isempty(object_rect)
            rectangle('Position', [object_rect(1), object_rect(3), object_rect(2)-object_rect(1), object_rect(4)-object_rect(3)], ...
                      'EdgeColor', 'k', 'LineWidth', 2);  % Black rectangle
        end
        hold off;
        
        pause(0.1);  % Control animation speed
    end
end