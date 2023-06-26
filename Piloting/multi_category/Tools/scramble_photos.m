

% code to scramble images 

cats = ["faces", "outdoor_scenes", "indoor_scenes", "objects", "body_parts"];
file_format = "jpg";
showOption = false;
scramble_parameter = 75;

for c=1:length(cats)
    curr_files = dir(sprintf('../Stimuli/Experimental_stims/%s/*.%s',cats(c),file_format));
    for img_i = 1:length(curr_files)
        % read file
        img = imread(sprintf('../Stimuli/Experimental_stims/%s/%s', cats(c), curr_files(img_i).name));
        % scramble
        [img_scrambled, permuteParameter] = hb_imageScramble( img, scramble_parameter, showOption ); 
        % save
        imwrite(img_scrambled, sprintf('../Stimuli/Experimental_stims/scramble/%s/scrambled_%s',cats(c),curr_files(img_i).name))

    end
end


% scramble practice images 

prac_files = dir(sprintf('../Stimuli/Practice_stims/*.%s',file_format));
for img_i = 1:length(prac_files)
    % read file
    img = imread(sprintf('../Stimuli/Practice_stims/%s', prac_files(img_i).name));
    % scramble
    [img_scrambled, permuteParameter] = hb_imageScramble( img, scramble_parameter, showOption ); 
    % save
    imwrite(img_scrambled, sprintf('../Stimuli/Practice_stims/scramble/scrambled_%s',prac_files(img_i).name))

end

% scramble reward stim

reward_stim = dir(sprintf('../Stimuli/Reward_stim/*.%s',"jpeg"));
img = imread(sprintf('../Stimuli/Reward_stim/%s', reward_stim(1).name));
[img_scrambled, permuteParameter] = hb_imageScramble( img, scramble_parameter, showOption ); 
imwrite(img_scrambled, sprintf('../Stimuli/Reward_stim/scrambled_%s',reward_stim(1).name))

% cite scramble function: Hio-Been Han (2019). Scrambling image (hb_imageScramble.m) (https://www.mathworks.com/matlabcentral/fileexchange/<...>), MATLAB Central File Exchange. Retrieved March 6, 2019.

