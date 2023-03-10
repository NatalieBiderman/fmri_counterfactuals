
// =========== Functions ===========

// This script includes function we use in the experiment.

// sprintf
if (!String.format) {
    String.format = function(format) {
      var args = Array.prototype.slice.call(arguments, 1);
      return format.replace(/{(\d+)}/g, function(match, number) {
        return typeof args[number] != 'undefined'
          ? args[number]
          : match
        ;
      });
    };
  }

  // repmat
  var repmat = function(elem, n){
      // returns an array with element repeated n times.
      var arr = [];
      for (var i = 0; i < n; i++) {
          arr = arr.concat(elem);
      };
      return arr;
  };

  // Range
  function range(start, stop, step) {
  var a = [start], b = start;
  while (b < stop) {
      a.push(b += step || 1);
  }
  return a;
}

// shuffle
function shuffle(a) {
  for (let i = a.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [a[i], a[j]] = [a[j], a[i]];
  }
  return a;
}

// normal distribution
function normal(mu, sigma, nsamples){
  if(!nsamples) nsamples = 6
  if(!sigma) sigma = 1
  if(!mu) mu=0

  var run_total = 0
  for(var i=0 ; i<nsamples ; i++){
     run_total += Math.random()
  }

  return sigma*(run_total - nsamples/2)/(nsamples/2) + mu
}

// create all possible combination of pairs (for final decisions phase)
function pairwise(list) {
  if (list.length < 2) { return []; }
  var first = list[0],
      rest  = list.slice(1),
      pairs = rest.map(function (x) { return [first, x]; });
  return pairs.concat(pairwise(rest));
}

// Shift order of rows in a column (for lure items in the memory test)
// and so, instead of an array [[1,2],[3,4],[5,6]] we get [[5,2],[1,4],[3,6]]
function shiftCol(arr, col) {
  var prev = arr[arr.length - 1][col-1];
  arr.forEach(function(v) {
    var t = v[col - 1];
    v[col - 1] = prev;
    prev = t;
  })
  return arr;
}

// Shift order of columns by deciding how many rows we need to move down
function shiftRows(arr, col, n_rows) {
  var original_arr = JSON.parse(JSON.stringify(arr));
  var arr = JSON.parse(JSON.stringify(arr));
  for (i=0; i<arr.length; i++){
    var new_row_ind = i+n_rows;
    if (new_row_ind > arr.length-1){
      new_row_ind = i - arr.length + n_rows;
    }
    arr[i][col] = original_arr[new_row_ind][col]
  }
  return arr;
}

// Swap order of columns according to specific index (for lures in the memory test).
// instead of [[1,2], [3,4], [5,6]] we switch the rows for index 1 for example,
// to get [[1,2], [4,3], [5,6]]
function swap(input, index_A, index_B) {
    var temp = input[index_A];
    input[index_A] = input[index_B];
    input[index_B] = temp;
}

// transpose
function transpose(a) {
    return Object.keys(a[0]).map(function(c) {
        return a.map(function(r) { return r[c]; });
    });
}
// Present png instructions screen
function present_instructions(img, ttype, keys){
  var instructions = {
    type: "html-keyboard-response",
    data: {ttype: ttype},
    stimulus: '<img class="instructions" src="Stimuli/Instructions/' + img + '"</img>',
    choices: keys_instructions,
    response_ends_trial: true
  }
  return instructions
}

// Save data to file functions
function saveData(name, data) {
  var xhr = new XMLHttpRequest();
  //xhr.addEventListener("load", onComplete);
  xhr.open('POST', 'Tools/write_data.php'); // 'write_data.php' is the path to the php file described above.
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.send(JSON.stringify({
    filename: name,
    filedata: data
  }));
}

// Time stamp for files
function timeStamp() {
  function s(x) {
    return x.length == 1 ? '0' + x : x
  }
  var a = new Date(),
    MM = s((a.getUTCMonth() + 1).toFixed()),
    dd = s(a.getUTCDate().toFixed()),
    hh = s(a.getUTCHours().toFixed()),
    mm = s(a.getUTCMinutes().toFixed()),
    year = s(a.getFullYear().toFixed());
  return  "d" + MM + dd +  year + "hr" + hh + mm
}

// Generate a random subject ID
function makeid(length) {
   var result           = '';
   var characters       = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
   var charactersLength = characters.length;
   for ( var i = 0; i < length; i++ ) {
      result += characters.charAt(Math.floor(Math.random() * charactersLength));
   }
   return result;
}

// Present quiz instructions using a loop function (until they get all qs right)
function present_quiz_instructions(instructions_screen, ttype, quiz_qs, quiz_answers){

    // Make sure participants understood the instructions, by giving them a short quiz.
    // if they got it wrong, present the instructions again, until they get it right.
    var comprehension_check = {
              type: 'survey-multi-choice',
              questions: quiz_qs,
              data: {ttype: ttype},
              on_finish: function(data) {
                  var responses = jsPsych.data.get().filter({ttype: ttype}).last(1).values()[0].responses;
                      responses = responses.split(","); // seperate responses by comma
                  var correct_answers = quiz_answers;
                  var repeat = 0;
                  for (i = 0 ; i < correct_answers.length; i++){
                    if (!responses[i].includes(correct_answers[i])){
                      repeat = 1;
                    } // if
                  } // for
                  data.repeat_instructions = repeat;
              } // on_finish
          }; // rating_comprehension_check

    var if_missed_instructions = {
      timeline: [missed_instruction_checkup],
      conditional_function: function(data){
          if (jsPsych.data.get().filter({ttype: ttype}).last(1).values()[0].repeat_instructions){
            return true;
          } else {
          return false;
        } // else
      } // conditional function
    } // if_confirmation

    var repeat_instructions = {
      timeline: [instructions_screen,comprehension_check,if_missed_instructions],
      loop_function: function(){
        if (jsPsych.data.get().filter({ttype: ttype}).last(1).values()[0].repeat_instructions){
          return true;
        } else {
          return false;
        } // else
      } // loop_function function
    } // repeat_rating_instructions
    return repeat_instructions
}

// Find trials of block change
function find_block_change(blocks_array){
  var transition_trials = [];
  for (i=0; i<blocks_array.length-1; i++){
    if (blocks_array[i] != blocks_array[i+1]){
      transition_trials = transition_trials.concat(i)
    }
  }
  return transition_trials
}
/*// Navigate to Mturk
function transferComplete() {
  window.opener.location.href = "https://app.prolific.ac/submissions/complete?cc=RPM4P8JM";
  self.close();
}*/
