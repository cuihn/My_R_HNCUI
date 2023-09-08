%% visualize time series data
% load grand averaged .erp format file 
event = ft_read_event('/Users/hainingcui/Dropbox/Trying/EEG_KNOC_N400/grand_averaged_RIDE_20221116.erp')

% Create an FT-compatible structure
ft_data = struct();
ft_data.label = {event.type};
ft_data.sampleinfo = [event.latency]';
ft_data.time = [event.latency] / event(1).fs;