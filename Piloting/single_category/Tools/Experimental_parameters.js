// =========== Experiment parameters  ===========

var curr_category         = "Scenes"
var image_folder          = "Stimuli/Experimental_stims/" + curr_category + "/";
var n_ratings             = 50;
var n_pairs               = 12;//10;
var n_rep_deliberation    = 2; // number of repetitions before starting the actual block
var n_explained_per_rep   = n_pairs/n_rep_deliberation; // number of trials explained by subjects per block
var n_deliberation_trials = n_pairs*(n_rep_deliberation+1); // number of total deliberaiton trials
var reward_types          = [1,-1];
var n_reward_blocks       = 3;
var n_reward_rep          = 2; // each block includes two repeatitions
var n_reward_trials       = n_reward_rep*n_pairs*n_reward_blocks;
var percent_Del_bonus     = 0.01; // what percentage of randomnly chosen trial will we give subjects
var n_blocks_FD           = 3;//4;
var n_trials_FD           = Math.pow(n_pairs/2,2)*2*n_blocks_FD; // 25 chosen and 25 unchosen per block
var mu_reward             = 150;
var std_reward            = 10;
var n_memory_new          = n_pairs;
var n_memory_old          = n_pairs;
var n_memory_trials       = n_memory_new + n_memory_old;
var n_outcome_evaluation_trials = n_pairs * 2;
var max_extra             = 3.5; // how much extra money we can give participants for their performance
var reward_per_correct_reponse = (max_extra-(mu_reward*percent_Del_bonus))/(n_trials_FD/2); // add $ 0.02 per correct response only for chosen pairs in the final decisions
var keys_see_reward       = [' '];
var keys_register_reward  = ['uparrow', 'downarrow']; // up - gain, down - loss
var keys_decision         = ['d', 'k']; // d - left, k - right
var keys_go_next          = ['n']; // next
var keys_go_back          = ['b']; // back
var keys_instructions     = [' '];
var keys_memory_old_new   = ['uparrow', 'downarrow']; // up - old, down - new
var keys_outcome_eval     = ['uparrow', 'downarrow']; // up - gain, down - loss
var min_deliberation_time = 300; // minimum of 300 ms to deliberate (to prevent pressing without thinking)
var max_deliberation_time = 10000; // 10 secs
var timing_FD             = 2500; // 2.5 sec
var timing_ITI            = 1000; // 1 sec ITI
var timing_reward_outcome = 2000; // see outcome on the screen
var timing_confirmation   = 500;
var timing_warning        = 1500;
var timeline              = [];
var quiz_qs               = {rating:
                                [{prompt: "True or False: You will rate the following photographs according to how much you like them",
                                 options: ["True", "False"]},
                                {prompt: "You will rate photographs by using: ",
                                 options: ["Mouse","Two optional keys"]}],
                             deliberation:
                                [{prompt: "True or False: You will now choose photographs for an auction, and your goal is to make decisions that will maximize your profits from the auction",
                                 options: ['True','False']},
                                {prompt: "True or False: You will practice the decisions a couple of times before the final round of decisions",
                                 options: ["True", "False"]},
                                {prompt: "How should you make this decision?",
                                 options: ["Consider carefully and take your time (up to 10 seconds)","Do it as quickly as possible"]},
                                {prompt: "True or False: When asked why did you choose a specific photograph, you may refer to any internal thoughts or external features that led to your decision",
                                  options: ["True", "False"]},
                                {prompt: "What button should you press to choose the photograph on the left?",
                                 options: ["K","D"]}],
                             reward:
                                [{prompt: "True or False: You will now find out the results of the auction.",
                                 options: ['True','False']},
                                {prompt: "True or False: Your chosen photographs could either result in a gain or in no gain",
                                 options: ["True", "False"]},
                                {prompt: "What should you do when shown a photograph?",
                                 options: ["Just look at it","Press the space bar to see its outcome"]},
                                {prompt: "What should you do when shown the outcome of the photograph?",
                                 options: ["Just look at it","Press the relevant key while the outcome is presented"]}],
                             final_decisions:
                                [{prompt: "True or False: You will now make more decisions between photographs to prepare a portfolio of high-valued photographs",
                                 options: ['True','False']},
                                {prompt: "To choose the photograph on the right you should press what key with the right index finger?",
                                 options: ["K","D"]}],
                             memory:
                                [{prompt: "You will now be asked what you remember from which phase of the experiment?",
                                 options: ["Decisions made in the beginning of the experiment","Decisions made in the end of the experiment"]},
                                {prompt: "Recombined photographs are:",
                                 options: ["Random photographs you haven't seen before", "Photographs you have seen before but they were not paired together"]}],
                             final_ratings:
                                [{prompt: "True or False: You will now rate again how much you like the following photographs",
                                 options: ["True","False"]},
                                 {prompt: "Your responses should be made using:",
                                  options: ["Mouse","Two optional keys"]}],
                             outcome_evaluation:
                                 [{prompt: "True or False: After you made decisions about which photographs should go on auction and learned their profits, the remaining photographs (those you did not choose) also went on auction.",
                                  options: ["True","False"]},
                                  {prompt: "True or False: You will now be asked what the auction outcomes were for the photographs you previously chose and those you did not choose to go on auction.",
                                   options: ["True","False"]},
                                  {prompt: "If the outcome was a GAIN, which button should you press?",
                                   options: ["Up arrow", "Down arrow"]},
                                  {prompt: "True or False: You will also be asked to indicate your confidence in your response",
                                    options: ["True","False"]}]};
quiz_answers                = {rating: ["True", "Mouse"],
                              deliberation: ["True", "True", "Consider carefully and take your time (up to 10 seconds)", "True", "D"],
                              reward: ["True", "True", "Press the space bar to see its outcome", "Press the relevant key while the outcome is presented"],
                              final_decisions: ["True", "K"],
                              memory: ["Decisions made in the beginning of the experiment", "Photographs you have seen before but they were not paired together"],
                              final_ratings: ["True", "Mouse"],
                              outcome_evaluation: ["True", "True", "Up arrow", "True"]};


// these are css for stimuli presentation. I insert the stimuli within a table,
// such that the first rows includes some prompt, and the second row includes
// the actual stimuli: two objects from both sides of a fixation place holder
var decisions_stims       = '<table class="table">\
                            <tr><p>{0}</p></tr>\
                            <tr><td> <img class="object" src="{1}"</img></td>\
                            <td class="fixation"></td>\
                            <td><img class="object" src="{2}"></img></td></tr></table>';
var choice_confirm_stims  = '<table class="table"><tr>\
                            <tr><p>{0}</p></tr>\
                            <td> <img class="{1}" src="{2}"</img> </td>\
                            <td class="fixation"></td>\
                            <td> <img class="{3}" src="{4}"></img> </td></tr></table>';
var fixation_stim         = '<table class="table"><tr>\
                            <td class="object"> </td>\
                            <td class="fixation">+</td>\
                            <td class="object"> </td></tr></table>';
var see_reward_stim       = '<table class="table"><tr>\
                            <tr><p style="height:30px">{0}</p></tr>\
                            <tr><div style="height:30px"></div></tr>\
                            <td class="object"> </td>\
                            <td class="object"><img class="object" src="{1}"/></td>\
                            <td class="object"> </td></tr></table>';
var reward_stim           = '<table class="table"><tr>\
                            <tr><div style="height:14px"><p style="font-size: 10pt">{0}</p></div></tr>\
                            <tr><p class="{1}" style="font-size:15.5pt">{2}{3}</p></tr>\
                            <td class="object"> </td>\
                            <td class="object"><img class="{4}" src="{5}"/></td>\
                            <td class="object"> </td></tr></table>';
var propmt_decision        = 'Which photograph do you choose? <br> left (D) / right (K) </br>';
var prompt_see_reward      = 'Press the SPACE bar to see the outcome of this photograph'
var prompt_reward          = 'Gain (up) / No gain (down)';
var prompt_pair_memory     = '<p>Is this pair INTACT (up) or RECOMBINED (down)?</p>';
var prompt_decision_memory = 'Which photographs did you previously choose? <br> left (D) / right (K) </br>';
var prompt_outcome_eval    = 'What was the auction profit of this photograph?<br> Gain (up) / No gain (down) <br>';
