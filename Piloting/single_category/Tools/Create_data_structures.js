// =========== Create data structures  ===========

// ---------------------------------- Rating -----------------------------------

var Ratings = [];
var all_stims = JSON.parse(JSON.stringify(Stimuli));
// choose specific categories
var curr_stims = all_stims.filter(a => a[0]==curr_category);
var curr_stims = jsPsych.randomization.shuffle(curr_stims); // shuffle order of stims
for (i = 0; i < n_ratings; i++) {
    Ratings[i] = {category: 'rating',
                  type: curr_stims[i][0],
                  stimulus: curr_stims[i][1], // painting
                  index: curr_stims[i][2], // index of painting
                  response: NaN,
                  rt: NaN};
  }
Ratings = jsPsych.randomization.shuffle(Ratings); // randomize order of Ratings

// ------------------------------- Deliberation --------------------------------

function create_deliberation_mat(Ratings_mat){

  // we create deliberation stimuli by first sorting the rating matrix by ratings,
  // and then using the middle 20 rated paintings. We next shuffle
  // the stimuli and divide them into left and right stimuli. Finally, we
  // randomly assign half of deliberation trials to include the highest rated painting
  // out of the pair to be located on the left, and make sure this is the case,
  // if not, we reverse the order of the left and right stimuli.

  // assign columns
  var col_left = 0;
  var col_right = 1;
  var col_rewards = 2;
  var col_explain = 3;
  var col_block = 4;

  // find middle-rated ratings trials and shuffle
  var sorted_ratings = Ratings_mat.sort(function(a,b){return a.response - b.response});
  var middle = [n_ratings/2-n_pairs,n_ratings/2+n_pairs];
  var deliberation_stims = jsPsych.randomization.shuffle(sorted_ratings.slice(middle[0],middle[1])); // take the 20 middle-rated rows from the sorted ratings matrix. and shuffle their order.
  var deliberation_inds = deliberation_stims.map(a => a.index)

  // assign stims for left and right
  var pairs = [];
  for (i=0; i < n_pairs; i++){
    pairs[i] = [deliberation_inds[i*2],deliberation_inds[i*2+1]]
  }

  // randomly choose half of pairs to assign the left painting as higher
  var left_high = jsPsych.randomization.sampleWithoutReplacement(range(0,pairs.length-1,1),pairs.length/2)
  // for each pair, check if the index of the pair is within the left_high vector,
  // if so, make sure the left is indeed high, if not, make sure the right is
  // high. If either of these isn't true, swap position of stims in the pair.
  var ratings = [];
  for (var i = 0; i < pairs.length; i++){
    // find ratings of both stims in a pair
    var rating_left = Number(deliberation_stims[deliberation_stims.findIndex(function(a){return a.index==pairs[i][col_left]})].response);
    var rating_right = Number(deliberation_stims[deliberation_stims.findIndex(function(a){return a.index==pairs[i][col_right]})].response);
    ratings[i] = [rating_left, rating_right];
    if (left_high.includes(i)){ // left painting is supposed to be higher
      if (rating_left < rating_right) { // but as of now, the right is rated higher than the left
        swap(pairs[i],col_left,col_right);
        swap(ratings[i], 0, 1)
      } // if rates
    } else if (!left_high.includes(i)) { // right painting is supposed to be higher
      if (rating_left > rating_right) { // but as of now, the left is rated higher than the right
        swap(pairs[i],col_left,col_right);
        swap(ratings[i], 0, 1)
      } // if
    } // else
  } // for i

  // assign rewards
  var rewards = repmat(reward_types,n_pairs/reward_types.length);
  rewards = jsPsych.randomization.shuffle(rewards);
  for (i=0; i < n_pairs; i++){
    pairs[i][col_rewards] = rewards[i];
  }

  // repeat deliberation pairs
  var all_pairs = [];
  all_pairs = repmat(pairs, n_rep_deliberation+1)

  // add explaination trials
  var n_explained_per_rep = Math.ceil(n_pairs/n_rep_deliberation);
  var explain_blocks = repmat(range(1,n_rep_deliberation),n_explained_per_rep).sort();
  var explain_trials_practice_blocks = [];
  for (b=1; b<n_rep_deliberation+1; b++){
    var curr_explain_trials = [];
    for (i=0; i<n_pairs; i++){
      if (explain_blocks[i] == b){
        curr_explain_trials[i] = 1;
      } else {
        curr_explain_trials[i] = 0;
      }
    }
    explain_trials_practice_blocks = explain_trials_practice_blocks.concat(curr_explain_trials);
  }
  explain_trials = explain_trials_practice_blocks.concat(repmat(0,n_pairs)); // last block includes no explanation trials.

  // block
  blocks = repmat(range(1,n_rep_deliberation+1), n_pairs).sort()

  // add to pairs
  for (t=0; t<all_pairs.length; t++){
    all_pairs[t] = all_pairs[t].concat([explain_trials[t],blocks[t]]) ;
  }

  // shuffle order within blocks
  all_pairs_tmp = [];
  for (b=1; b<n_rep_deliberation+2; b++){
    curr_block = all_pairs.filter(a => a[col_block]==b)
    curr_block = jsPsych.randomization.shuffle(curr_block);
    all_pairs_tmp = all_pairs_tmp.concat(curr_block)
  }
  all_trials = all_pairs_tmp;

  // add to Deliberation matrix
  Deliberation = [];
  for (t=0; t<all_trials.length; t++){
    stim_left = deliberation_stims.filter(function(a){return a.index==all_trials[t][col_left]})[0]
    stim_right = deliberation_stims.filter(function(a){return a.index==all_trials[t][col_right]})[0]
    Deliberation[t] = {category: 'deliberation',
                       index_trial: t,
                       block: all_trials[t][col_block],
                       reward_type: all_trials[t][col_rewards],
                       explain_trial: all_trials[t][col_explain],
                       stimulus_left: stim_left.index,
                       painting_left: stim_left.stimulus,
                       rating_left: Number(stim_left.response),
                       stimulus_right: stim_right.index,
                       painting_right: stim_right.stimulus,
                       rating_right: Number(stim_right.response),
                       left_chosen: NaN,
                       chosen_obj: NaN,
                       unchosen_obj: NaN}
  }

   return Deliberation

} // function

// ----------------------------- Reward learning -------------------------------

function create_reward_learning_mat(Deliberation_matrix) {

// assign reward value for gain items and access chosen stim from deliberation
var chosen_stim = []; var reward_type = []; var reward_amount = [];
for (i = 0; i < Deliberation_matrix.length; i++){
  chosen_stim[i] = Deliberation_matrix[i].chosen_obj;
  reward_type[i] = Deliberation_matrix[i].reward_type;
  if (reward_type[i] == 1){
    reward_amount[i] = Math.round(normal(mu_reward, std_reward, 1));
  } else {
    reward_amount[i] = 0;
  }
}
// check if one of the paintings is exactly mu_reward. if not, change the first amount to this number so we could give this bonus for the deliberation choices
if(!reward_amount.includes(mu_reward)){
  var found = reward_amount.findIndex(function(element) {
  return element > 0;
  });
  reward_amount[found] = mu_reward;
}

// Create the full reward learning matrix
var Reward_learning_reps = []; var Reward_learning_trials = []; var Reward_learning = [];
for (b = 0; b < n_reward_blocks; b++){
  for (r = 0; r < n_reward_rep; r++){
    for (t = 0; t < n_pairs; t++){
      Reward_learning_trials[t] = {category: 'reward_learning',
                                 block: b+1,
                                 stimulus: chosen_stim[t],
                                 reward_type: reward_type[t],
                                 reward_amount: reward_amount[t]}
    } // t
    Reward_learning_reps = Reward_learning_reps.concat(Reward_learning_trials)
    Reward_learning_trials = [];
  } // r
  Reward_learning = Reward_learning.concat(jsPsych.randomization.shuffle(Reward_learning_reps))
  Reward_learning_reps = [];
} // b

return Reward_learning
} // function

// ----------------------------- Final decisions -------------------------------

function create_final_decisions_mat(Deliberation_matrix, Ratings_matrix) {

var Final_decisions = [];

// seperate deliberation trials according to outcome types
var pos = []; var nonpos = [];
for (t = 0; t < n_pairs; t++){
    if (Deliberation_matrix[t].reward_type == reward_types[0]) {
          pos = pos.concat(Deliberation_matrix[t].index_trial)}
    else if (Deliberation_matrix[t].reward_type == reward_types[1]) {
          nonpos = nonpos.concat(Deliberation_matrix[t].index_trial)}
  } // for

// run over blocks, and then over chosen/unchosen pairs, and create pairs
// combining positive and non-positive outcome trials
var block_Final_decisions=[];
for (var b=0; b < n_blocks_FD; b++){
  // run over chosen or unchosen pairs
  for (var ch=0; ch < 2; ch++){
    // assign empty variables
    var pairs_array = []; var outcome_array = []; var swap_inds = []; var check_outcome = []; var tmp_Final_decisions = [];
    // create all possible pairs of positive and non-positive outcomes, by
    // replicating one outcome type n_pairs/2, and the other the same, but also
    // sorting it to get, e.g., [[1,2,3,1,2,3,1,2,3][4,4,4,5,5,5,6,6,6]]
    pairs_array = [repmat(pos,n_pairs/2),repmat(nonpos,n_pairs/2).sort()];
    pairs_array = transpose(pairs_array); // transpose to columns
    // create outcome arrays
    outcome_array = transpose([repmat(reward_types[0],pairs_array.length),repmat(reward_types[1],pairs_array.length)])
    // randomly choose half of these trials and reverse the location of the
    // positive outcome from the left side to the right, to make sure outcome type
    // is counterbalanaced across trials
    swap_inds = jsPsych.randomization.shuffle(range(0,pairs_array.length-1,1)).splice(0,pairs_array.length/2)
    for (var i = 0; i < pairs_array.length; i++){
      if (swap_inds.includes(i)){
        pairs_array[i] = pairs_array[i].reverse();
        outcome_array[i] = outcome_array[i].reverse();
      } // if
    } // for i
    // put it all together inside Final_decisios array of objects
    var tmp_Final_decisions=[];
    for (t = 0; t < pairs_array.length; t++) {
      tmp_Final_decisions[t] = {category: 'final_decisions',
                                block: b+1,
                                stimulus_left_ind: pairs_array[t][0], // index from deliberation matrix. The stimulus itself will appear in this index.
                                stimulus_right_ind: pairs_array[t][1],
                                stimulus_left: NaN,
                                stimulus_right: NaN,
                                index_trial: NaN,
                                chosen_trial: ch,
                                reward_type_left: outcome_array[t][0],
                                reward_type_right: outcome_array[t][1],
                                left_chosen: NaN,
                                higher_outcome_chosen: NaN}
    } // trial
    // add temp matrix to create a block matrix
    block_Final_decisions = block_Final_decisions.concat(tmp_Final_decisions);
  } // for chosen/unchosen
  // jsPsych.randomization.shuffle order of trials in each block
  block_Final_decisions = jsPsych.randomization.shuffle(block_Final_decisions);
  // add the block to general matrix
  Final_decisions = Final_decisions.concat(block_Final_decisions);
  block_Final_decisions = [];
} // for block

// After building the structure of the matrix, add right and left stimuli
// according to choices in the deliberation phase
for (t=0; t < Final_decisions.length; t++) {
  var left_stim = Deliberation_matrix.filter(function(a){return a.index_trial==Final_decisions[t].stimulus_left_ind})[0]
  var right_stim = Deliberation_matrix.filter(function(a){return a.index_trial==Final_decisions[t].stimulus_right_ind})[0]
    if (Final_decisions[t].chosen_trial==1) { // this is a chosen trial
        Final_decisions[t].stimulus_left = left_stim.chosen_obj;
        Final_decisions[t].stimulus_right = right_stim.chosen_obj;
    } else if (Final_decisions[t].chosen_trial==0) {
      Final_decisions[t].stimulus_left = left_stim.unchosen_obj;
      Final_decisions[t].stimulus_right = right_stim.unchosen_obj;
    }
    Final_decisions[t].index_trial = t;
    // add logical for whether the gain reward is on the left
    if (left_stim.reward_type == 1){ // gain on the left
      Final_decisions[t].gain_left = 1;
    } else {
      Final_decisions[t].gain_left = 0;
    }
    // add additional information from rating matrix
    Final_decisions[t].painting_left = Ratings_matrix.filter(function(a){return a.index==Final_decisions[t].stimulus_left})[0].stimulus;
    Final_decisions[t].rating_left = Number(Ratings_matrix.filter(function(a){return a.index==Final_decisions[t].stimulus_left})[0].response);
    Final_decisions[t].painting_right = Ratings_matrix.filter(function(a){return a.index==Final_decisions[t].stimulus_right})[0].stimulus;
    Final_decisions[t].rating_right = Number(Ratings_matrix.filter(function(a){return a.index==Final_decisions[t].stimulus_right})[0].response);
  }; // trial final decisions

return Final_decisions

} // function

// ----------------------------- Memory test -------------------------------

// we present 10 trials of intact pairs, and 10 of recombined pairs.

function create_memory_mat(Deliberation_matrix){

var col_left = 0;
var col_right = 1;
var col_chosen_obj = 2;
var col_old_pair = 3;

// randomly shuffle the order of deliberation matrix
Deliberation_matrix = jsPsych.randomization.repeat(Deliberation_matrix,1)

// assign pairs for old and new trials
var old_pairs = []; var new_pairs = []; var random_swap = []; var all_pairs = [];
for (i = 0; i < n_pairs; i++){
  // take the left and right stimuli for old pairs, as well as the chosen object in that trial.
  old_pairs[i] = [Deliberation_matrix[i].stimulus_left,Deliberation_matrix[i].stimulus_right,Deliberation_matrix[i].chosen_obj, 1];
  // create pairs of new items, and seperate chosen and unchosen objects for the first and second column respectively.
  new_pairs[i] = [Deliberation_matrix[i].chosen_obj,Deliberation_matrix[i].unchosen_obj, NaN, 0];
}

// move chosen objects in new pairs one row down (to make sure the shuffling of objects will not result in an old pair)
var new_pairs_tmp = shiftRows(new_pairs,col_right,1);
for (i=0; i<new_pairs_tmp.length; i++){new_pairs_tmp[i][col_chosen_obj]=NaN;}

// choose randomly half of rows from new_pairs to switch the left/right side of chosen and unchosen objects (so chosen won't always appear on the left)
random_swap = jsPsych.randomization.shuffle(range(0,n_pairs-1,1)).splice(0,n_pairs/2);
for (i = 0; i < random_swap.length; i++){
  swap(new_pairs_tmp[random_swap[i]],col_left,col_right)
};

// combine both types of pairs, and shuffle
all_pairs = jsPsych.randomization.shuffle(old_pairs.concat(new_pairs_tmp));

// insert it all in memory matrix
var Memory_test = [];
for (t = 0; t < n_memory_trials; t++) {
  Memory_test[t] = {category: 'memory_test',
                    stimulus_left: all_pairs[t][col_left],
                    stimulus_right: all_pairs[t][col_right],
                    index_trial: t,
                    old_pair: all_pairs[t][col_old_pair],
                    old_response: NaN,
                    rt_pairs: NaN,
                    chosen_object: all_pairs[t][col_chosen_obj],
                    left_object_chosen: NaN,
                    rt_object: NaN};
                 }; // for t
return Memory_test;
} // function

// ---------------------------------- Final Rating -----------------------------------

function create_final_ratings_mat(Deliberation_matrix, Ratings_matrix){

// here we create a final ratings matrix, where all paintings will be rated again,
// included those that did not participate in the experiment

var col_stim = 0;
var col_painting = 1;
var col_initial_rating = 2;
var col_deliberated_stim = 3;
var col_is_chosen = 4;
var col_outcome = 5;

all_stims = jsPsych.randomization.shuffle(Ratings_matrix.map(a => a.index));
del_stims = Deliberation_matrix.map(a => a.stimulus_left).concat(Deliberation_matrix.map(a => a.stimulus_right));
all_trials = [];
for (i=0; i < all_stims.length; i++){

  // include info about the painting
  curr_rating_trial = Ratings_matrix.filter(a => a.index == all_stims[i])[0];
  all_trials[i] = [curr_rating_trial.index, curr_rating_trial.stimulus, Number(curr_rating_trial.response)];

  // find whether the stim was deliberated
  if (del_stims.includes(all_stims[i])){
    all_trials[i][col_deliberated_stim] = 1;

    // find out whether this was a chosen item
    curr_del_trial = Deliberation_matrix.filter(a => a.stimulus_left == all_stims[i] | a.stimulus_right == all_stims[i])[0];
    if (all_stims[i] == curr_del_trial.chosen_obj){
      all_trials[i][col_is_chosen] = 1;
    } else if (all_stims[i] == curr_del_trial.unchosen_obj){
      all_trials[i][col_is_chosen] = 0;
    }
    // find out the item's outcome
    all_trials[i][col_outcome] = curr_del_trial.reward_type;

    // new item
  } else {
    all_trials[i] = all_trials[i].concat([0, NaN, NaN]);
  }
}

var Final_ratings_mat = [];
for (t = 0; t < all_trials.length; t++) {
  Final_ratings_mat[t] = {category: 'final_ratings',
                      index_trial: t,
                      stimulus: all_trials[t][col_stim],
                      painting: all_trials[t][col_painting],
                      initial_rating: all_trials[t][col_initial_rating],
                      new_rating: NaN,
                      deliberated_stim: all_trials[t][col_deliberated_stim],
                      chosen_obj: all_trials[t][col_is_chosen],
                      reward_type: all_trials[t][col_outcome],
                    };
                 }; // for t

return Final_ratings_mat;

}

// ---------------------------------- Outcome evaluation -----------------------------------

function create_outcome_evaluation_mat(Final_ratings_mat){

  // ask only about deliberated stims
  Outcome_evaluation_mat = Final_ratings_mat.filter(a => a.deliberated_stim == 1)

  // randomize order of trials
  Outcome_evaluation_mat = jsPsych.randomization.shuffle(Outcome_evaluation_mat);

  // add outcome_evaluation and change category and index
  for (t=0; t<Outcome_evaluation_mat.length; t++){
    Outcome_evaluation_mat[t].index_trial = t;
    Outcome_evaluation_mat[t].category = 'outcome_evaluation';
    Outcome_evaluation_mat[t].outcome_eval_response = NaN;
  }

  return Outcome_evaluation_mat;

}
