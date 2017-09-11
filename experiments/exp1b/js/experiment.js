function make_slides(f) {
  var   slides = {};

  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.instructions = slide({
    name : "instructions",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });
  
  slides.instructions1 = slide({
    name : "instructions1",
    start : function() {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	
    	var inst1 = "";
//    	console.log(block_order);
    	if (exp.stims_block1[0].block == "ai") {
    		inst1 = inst1 + "First you'll answer questions about what the people at the party are asking about."
    	} else {
    		inst1 = inst1 + "First you'll answer questions about what the people at the party are certain about."    		
    		}
    	$("#inst1").html(inst1);
    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  }); 
     

  slides.block1 = slide({
    name : "block1",
    present : exp.stims_block1,
    start : function() {
      $(".err").hide();
    },
    present_handle : function(stim) {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	    	    
      this.stim = stim;
    	this.stim.trial_start = Date.now();      
        $(".err").hide();    	
	  this.init_sliders();
      exp.sliderPost = null;	 
      console.log(this.stim);     
      var utterance = this.stim.name + " asks: \"<strong><i>"+this.stim.utterance+"</i></strong>\""
	  $(".sentence").html(utterance);
	  var question = "";
	  console.log(this.stim.block);
	  if (this.stim.block == "ai") {
	  		question = "Is "+this.stim.name+" asking whether "+this.stim.question+"?";
	  } else {
	  		question = "Is "+this.stim.name+" certain that "+this.stim.question+"?";	  	
	  	}
	  $(".question").html(question);	  
    },

    button : function() {
    	console.log(exp.sliderPost);
      if (exp.sliderPost != null) {
        this.log_responses();
        _stream.apply(this); //use exp.go() if and only if there is no "present" data.
      } else {
        $(".err").show();
      }
    },
    init_sliders : function() {
      utils.make_slider("#single_slider", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },
    log_responses : function() {
      exp.data_trials.push({
      "block" : "block1",
      "question_type" : this.stim.block,      
   	  "slide_number_in_experiment" : exp.phase,
   	  "short_trigger": this.stim.short_trigger,
   	  "trigger": this.stim.trigger,
   	  "content": this.stim.content,
   	  "trigger_class": this.stim.trigger_class,
      "response" : exp.sliderPost,
      "rt" : Date.now() - this.stim.trial_start
      });
    }
  }); 
  
  slides.instructions2 = slide({
    name : "instructions2",
    start : function() {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	    	
    	var inst2 = "That was the first half! ";
    	if (exp.stims_block2[0].block == "ai") {
    		inst2 = inst2 + "Now you'll answer questions about what the people at the party are asking about."
    	} else {
    		inst2 = inst2 + "Now you'll answer questions about what the people at the party are certain about."    		
    		}
    	$("#inst2").html(inst2);
    },
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });   
  
  slides.block2 = slide({
    name : "block2",
    present : exp.stims_block2,
    start : function() {
    $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	    	
      $(".err").hide();
    },
    present_handle : function(stim) {
      this.stim = stim;
    	this.stim.trial_start = Date.now();      
        $(".err").hide();    	
	  this.init_sliders();
      exp.sliderPost = null;	      
      var utterance = this.stim.name + " asks: \"<strong><i>"+this.stim.utterance+"</i></strong>\""
	  $(".sentence").html(utterance);
	  var question = "";
	  console.log(this.stim.block);	  
	  if (this.stim.block == "ai") {
	  		question = "Is "+this.stim.name+" asking whether "+this.stim.question+"?";
	  } else {
	  		question = "Is "+this.stim.name+" certain that "+this.stim.question+"?";	  	
	  	}
	  $(".question").html(question);	  
    },

    button : function() {
    	console.log(exp.sliderPost);
      if (exp.sliderPost != null) {
        this.log_responses();
        _stream.apply(this); //use exp.go() if and only if there is no "present" data.
      } else {
        $(".err").show();
      }
    },
    init_sliders : function() {
      utils.make_slider("#single_slider2", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },
    log_responses : function() {
      exp.data_trials.push({
      "block" : "block2",
      "question_type" : this.stim.block,     
   	  "slide_number_in_experiment" : exp.phase,
   	  "short_trigger": this.stim.short_trigger,   	  
   	  "trigger": this.stim.trigger,
   	  "content": this.stim.content,
   	  "trigger_class": this.stim.trigger_class,
      "response" : exp.sliderPost,
      "rt" : Date.now() - this.stim.trial_start
      });
    }
  });        
 

  slides.questionaire =  slide({
    name : "questionaire",
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        language : $("#language").val(),
//        enjoyment : $("#enjoyment").val(),
//        asses : $('input[name="assess"]:checked').val(),
        american : $('input[name="ame"]:checked').val(),
        age : $("#age").val(),
//        gender : $("#gender").val(),
//        education : $("#education").val(),
        comments : $("#comments").val(),
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.finished = slide({
    name : "finished",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          "condition" : exp.condition,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      setTimeout(function() {turk.submit(exp.data);}, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {

  var names = _.shuffle([
    {
      "name":"James",
      "gender":"M"
    },
//    {
//      "name":"John",
//      "gender":"M"
//    },
    {
      "name":"Robert",
      "gender":"M"
    },
//     {
//       "name":"Michael",
//       "gender":"M"
//     },
    {
      "name":"William",
      "gender":"M"
    },
    {
      "name":"David",
      "gender":"M"
    },
//    {
//      "name":"Richard",
//      "gender":"M"
//    },
    {
      "name":"Joseph",
      "gender":"M"
    },
    {
      "name":"Charles",
      "gender":"M"
    },
    {
      "name":"Thomas",
      "gender":"M"
    },
    {
      "name":"Christopher",
      "gender":"M"
    },
    {
      "name":"Daniel",
      "gender":"M"
    },
    {
      "name":"Matthew",
      "gender":"M"
    },
//    {
//      "name":"Donald",
//      "gender":"M"
//    },
    {
      "name":"Anthony",
      "gender":"M"
    },
    {
      "name":"Paul",
      "gender":"M"
    },
//    {
//      "name":"Mark",
//      "gender":"M"
//    },
    {
      "name":"George",
      "gender":"M"
    },
    {
      "name":"Steven",
      "gender":"M"
    },
    {
      "name":"Kenneth",
      "gender":"M"
    },
//    {
//      "name":"Andrew",
//      "gender":"M"
//    },
    {
      "name":"Edward",
      "gender":"M"
    },
    {
      "name":"Joshua",
      "gender":"M"
    },
    {
      "name":"Brian",
      "gender":"M"
    },
    {
      "name":"Kevin",
      "gender":"M"
    },
    {
      "name":"Ronald",
      "gender":"M"
    },
    {
      "name":"Timothy",
      "gender":"M"
    },
    {
      "name":"Jason",
      "gender":"M"
    },
    {
      "name":"Jeffrey",
      "gender":"M"
    },
    {
      "name":"Gary",
      "gender":"M"
    },
    {
      "name":"Ryan",
      "gender":"M"
    },
    {
      "name":"Nicholas",
      "gender":"M"
    },
    {
      "name":"Eric",
      "gender":"M"
    },
    {
      "name":"Jacob",
      "gender":"M"
    },
    {
      "name":"Jonathan",
      "gender":"M"
    },
    {
      "name":"Larry",
      "gender":"M"
    },
//    {
//      "name":"Frank",
//      "gender":"M"
//    },
    {
      "name":"Scott",
      "gender":"M"
    },
    {
      "name":"Justin",
      "gender":"M"
    },
    {
      "name":"Brandon",
      "gender":"M"
    },
    {
      "name":"Raymond",
      "gender":"M"
    },
    {
      "name":"Gregory",
      "gender":"M"
    },
    {
      "name":"Samuel",
      "gender":"M"
    },
    {
      "name":"Benjamin",
      "gender":"M"
    },
    {
      "name":"Patrick",
      "gender":"M"
    },
//    {
//      "name":"Jack",
//      "gender":"M"
//    },
    {
      "name":"Dennis",
      "gender":"M"
    },
    {
      "name":"Jerry",
      "gender":"M"
    },
    {
      "name":"Alexander",
      "gender":"M"
    },
    {
      "name":"Tyler",
      "gender":"M"
    },
//    {
//      "name":"Mary",
//      "gender":"F"
//    },
    {
      "name":"Jennifer",
      "gender":"F"
    },
    {
      "name":"Elizabeth",
      "gender":"F"
    },
    {
      "name":"Linda",
      "gender":"F"
    },
    {
      "name":"Emily",
      "gender":"F"
    },
//    {
//      "name":"Susan",
//      "gender":"F"
//    },
    {
      "name":"Margaret",
      "gender":"F"
    },
    {
      "name":"Jessica",
      "gender":"F"
    },
    {
      "name":"Dorothy",
      "gender":"F"
    },
//     {
//       "name":"Sarah",
//       "gender":"F"
//     },
    {
      "name":"Karen",
      "gender":"F"
    },
    {
      "name":"Nancy",
      "gender":"F"
    },
//     {
//       "name":"Betty",
//       "gender":"F"
//     },
    {
      "name":"Lisa",
      "gender":"F"
    },
    {
      "name":"Sandra",
      "gender":"F"
    },
//     {
//       "name":"Helen",
//       "gender":"F"
//     },
    {
      "name":"Ashley",
      "gender":"F"
    },
    {
      "name":"Donna",
      "gender":"F"
    },
    {
      "name":"Kimberly",
      "gender":"F"
    },
    {
      "name":"Carol",
      "gender":"F"
    },
    {
      "name":"Michelle",
      "gender":"F"
    },
    {
      "name":"Emily",
      "gender":"F"
    },
//     {
//       "name":"Amanda",
//       "gender":"F"
//     },
    {
      "name":"Melissa",
      "gender":"F"
    },
    {
      "name":"Deborah",
      "gender":"F"
    },
    {
      "name":"Laura",
      "gender":"F"
    },
    {
      "name":"Stephanie",
      "gender":"F"
    },
    {
      "name":"Rebecca",
      "gender":"F"
    },
    {
      "name":"Sharon",
      "gender":"F"
    },
    {
      "name":"Cynthia",
      "gender":"F"
    },
    {
      "name":"Kathleen",
      "gender":"F"
    },
    {
      "name":"Ruth",
      "gender":"F"
    },
//    {
//      "name":"Anna",
//      "gender":"F"
//    },
    {
      "name":"Shirley",
      "gender":"F"
    },
    {
      "name":"Amy",
      "gender":"F"
    },
    {
      "name":"Angela",
      "gender":"F"
    },
    {
      "name":"Virginia",
      "gender":"F"
    },
    {
      "name":"Brenda",
      "gender":"F"
    },
 //    {
//       "name":"Catherine",
//       "gender":"F"
//     },
    {
      "name":"Nicole",
      "gender":"F"
    },
    {
      "name":"Christina",
      "gender":"F"
    },
//     {
//       "name":"Janet",
//       "gender":"F"
//     },
//     {
//       "name":"Samantha",
//       "gender":"F"
//     },
    {
      "name":"Carolyn",
      "gender":"F"
    },
    {
      "name":"Rachel",
      "gender":"F"
    },
    {
      "name":"Heather",
      "gender":"F"
    },
    {
      "name":"Diane",
      "gender":"F"
    },
//     {
//       "name":"Joyce",
//       "gender":"F"
//     },
    {
      "name":"Julie",
      "gender":"F"
    },
    {
      "name":"Emma",
      "gender":"F"
    }
  ]);

var items = _.shuffle([ 
   {
     "trigger":"MC1",
     "trigger_class":"NonProj"
   }, 
   {
     "trigger":"MC2",
     "trigger_class":"NonProj"
   },
   {
     "trigger":"MC3",
     "trigger_class":"NonProj"
   }, 
   {
     "trigger":"MC4",
     "trigger_class":"NonProj"
   },
   {
     "trigger":"MC5",
     "trigger_class":"NonProj"
   },
   {
     "trigger":"MC6",
     "trigger_class":"NonProj"
   },
   {
     "trigger":"MC7",
     "trigger_class":"NonProj"
   },
   {
     "trigger":"MC8",
     "trigger_class":"NonProj"
   },
   {
     "trigger":"saw",
     "trigger_class":"C"
   }, 
   {
     "trigger":"revealed",
     "trigger_class":"C"
   }, 
   {
     "trigger":"is_amused",
     "trigger_class":"C"
   }, 
   {
     "trigger":"is_annoyed",
     "trigger_class":"C"
   },
   {
     "trigger":"discovered",
     "trigger_class":"C"
   },
   {
     "trigger":"learned",
     "trigger_class":"C"
   }, 
   {
     "trigger":"found_out",
     "trigger_class":"C"
   }, 
   {
     "trigger":"is_aware",
     "trigger_class":"C"
   }, 
   {
     "trigger":"realize",
     "trigger_class":"C"
   },
   {
     "trigger":"noticed",
     "trigger_class":"C"
   },
   {
     "trigger":"established",
     "trigger_class":"C"
   },
   {
     "trigger":"confessed",
     "trigger_class":"C"
   }
 ]);

 var contents = {
   "tea": {
     "question":"Raul was drinking chamomile tea",
     "MC":"Was Raul drinking chamomile tea?",
     "saw":"Did Mandy see that Raul was drinking chamomile tea?",
     "revealed":"Did Mandy reveal that Raul was drinking chamomile tea?",
     "is_amused":"Is Mandy amused that Raul was drinking chamomile tea?",
     "is_annoyed":"Is Mandy annoyed that Raul was drinking chamomile tea?",
     "discovered":"Did Mandy discover that Raul was drinking chamomile tea?",
     "learned":"Did Mandy learn that Raul was drinking chamomile tea?",
     "found_out":"Did Mandy find out that Raul was drinking chamomile tea?",
     "is_aware":"Is Mandy aware that Raul was drinking chamomile tea?",
     "realize":"Does Mandy realize that Raul was drinking chamomile tea?",
     "noticed":"Did Mandy notice that Raul was drinking chamomile tea?",
     "established":"Did Mandy establish that Raul was drinking chamomile tea?",
     "confessed":"Did Mandy confess that Raul was drinking chamomile tea?"
   },
   "frisbee": {
     "question":"Jack played frisbee with the kids",
     "MC":"Did Jack play frisbee with the kids?",
     "saw":"Did Sarah see that Jack played frisbee with the kids?",
     "revealed":"Did Sarah reveal that Jack played frisbee with the kids?",
     "is_amused":"Is Sarah amused that Jack played frisbee with the kids?",
     "is_annoyed":"Is Sarah annoyed that Jack played frisbee with the kids?",
     "discovered":"Did Sarah discover that Jack played frisbee with the kids?",
     "learned":"Did Sarah learn that Jack played frisbee with the kids?",
     "found_out":"Did Sarah find out that Jack played frisbee with the kids?",
     "is_aware":"Is Sarah aware that Jack played frisbee with the kids?",
     "realize":"Does Sarah realize that Jack played frisbee with the kids?",
     "noticed":"Did Sarah notice that Jack played frisbee with the kids?",
     "established":"Did Sarah establish that Jack played frisbee with the kids?",
     "confessed":"Did Sarah confess that Jack played frisbee with the kids?"
   },
   "garage": {
     "question":"John was hiding in the garage",
     "MC":"Was John hiding in the garage?",
     "saw":"Did Kim see that John was hiding in the garage?",
     "revealed":"Did Kim reveal that John was hiding in the garage?",
     "is_amused":"Is Kim amused that John was hiding in the garage?",
     "is_annoyed":"Is Kim annoyed that John was hiding in the garage?",
     "discovered":"Did Kim discover that John was hiding in the garage?",
     "learned":"Did Kim learn that John was hiding in the garage?",
     "found_out":"Did Kim find out that John was hiding in the garage?",
     "is_aware":"Is Kim aware that John was hiding in the garage?",
     "realize":"Does Kim realize that John was hiding in the garage?",
     "noticed":"Did Kim notice that John was hiding in the garage?",
     "established":"Did Kim establish that John was hiding in the garage?",
     "confessed":"Did Kim confess that John was hiding in the garage?"
   },
   "zoo": {
     "question":"Mike visited the zoo",
     "MC":"Did Mike visit the zoo?",
     "saw":"Did Jane see that Mike visited the zoo?",
     "revealed":"Did Jane reveal that Mike visited the zoo?",
     "is_amused":"Is Jane amused that Mike visited the zoo?",
     "is_annoyed":"Is Jane annoyed that Mike visited the zoo?",
     "discovered":"Did Jane discover that Mike visited the zoo?",
     "learned":"Did Jane learn that Mike visited the zoo?",
     "found_out":"Did Jane find out that Mike visited the zoo?",
     "is_aware":"Is Jane aware that Mike visited the zoo?",
     "realize":"Does Jane realize that Mike visited the zoo?",
     "noticed":"Did Jane notice that Mike visited the zoo?",
     "established":"Did Jane establish that Mike visited the zoo?",
     "confessed":"Did Jane confess that Mike visited the zoo?"
   },
   "purple": {
     "question":"Zach dyed his hair purple",
     "MC":"Did Zach dye his hair purple?",
     "saw":"Did Jane see that Zach dyed his hair purple?",
     "revealed":"Did Jane reveal that Zach dyed his hair purple?",
     "is_amused":"Is Jane amused that Zach dyed his hair purple?",
     "is_annoyed":"Is Jane annoyed that Zach dyed his hair purple?",
     "discovered":"Did Jane discover that Zach dyed his hair purple?",
     "learned":"Did Jane learn that Zach dyed his hair purple?",
     "found_out":"Did Jane find out that Zach dyed his hair purple?",
     "is_aware":"Is Jane aware that Zach dyed his hair purple?",
     "realize":"Does Jane realize that Zach dyed his hair purple?",
     "noticed":"Did Jane notice that Zach dyed his hair purple?",
     "established":"Did Jane establish that Zach dyed his hair purple?",
     "confessed":"Did Jane confess that Zach dyed his hair purple?"
   },
   "cupcakes": {
     "question":"Marissa brought almond cupcakes",
     "MC":"Did Marissa bring almond cupcakes?",
     "saw":"Did Frank see that Marissa brought almond cupcakes?",
     "revealed":"Did Frank reveal that Marissa brought almond cupcakes?",
     "is_amused":"Is Frank amused that Marissa brought almond cupcakes?",
     "is_annoyed":"Is Frank annoyed that Marissa brought almond cupcakes?",
     "discovered":"Did Frank discover that Marissa brought almond cupcakes?",
     "learned":"Did Frank learn that Marissa brought almond cupcakes?",
     "found_out":"Did Frank find out that Marissa brought almond cupcakes?",
     "is_aware":"Is Frank aware that Marissa brought almond cupcakes?",
     "realize":"Does Frank realize that Marissa brought almond cupcakes?",
     "noticed":"Did Frank notice that Marissa brought almond cupcakes?",
     "established":"Did Frank establish that Marissa brought almond cupcakes?",
     "confessed":"Did Frank confess that Marissa brought almond cupcakes?"
   },
   "swing": {
     "question":"Chad put up a swing in his backyard",
     "MC":"Did Chad put up a swing in his backyard?",
     "saw":"Did Andrea see that Chad put up a swing in his backyard?",
     "revealed":"Did Andrea reveal that Chad put up a swing in his backyard?",
     "is_amused":"Is Andrea amused that Chad put up a swing in his backyard?",
     "is_annoyed":"Is Andrea annoyed that Chad put up a swing in his backyard?",
     "discovered":"Did Andrea discover that Chad put up a swing in his backyard?",
     "learned":"Did Andrea learn that Chad put up a swing in his backyard?",
     "found_out":"Did Andrea find out that Chad put up a swing in his backyard?",
     "is_aware":"Is Andrea aware that Chad put up a swing in his backyard?",
     "realize":"Does Andrea realize that Chad put up a swing in his backyard?",
     "noticed":"Did Andrea notice that Chad put up a swing in his backyard?",
     "established":"Did Andrea establish that Chad put up a swing in his backyard?",
     "confessed":"Did Andrea confess that Chad put up a swing in his backyard?"
   },
   "ditch": {
     "question":"Greg drove his car into a ditch",
     "MC":"Did Greg drive his car into a ditch?",
     "saw":"Did Chloe see that Greg drove his car into a ditch?",
     "revealed":"Did Chloe reveal that Greg drove his car into a ditch?",
     "is_amused":"Is Chloe amused that Greg drove his car into a ditch?",
     "is_annoyed":"Is Chloe annoyed that Greg drove his car into a ditch?",
     "discovered":"Did Chloe discover that Greg drove his car into a ditch?",
     "learned":"Did Chloe learn that Greg drove his car into a ditch?",
     "found_out":"Did Chloe find out that Greg drove his car into a ditch?",
     "is_aware":"Is Chloe aware that Greg drove his car into a ditch?",
     "realize":"Does Chloe realize that Greg drove his car into a ditch?",
     "noticed":"Did Chloe notice that Greg drove his car into a ditch?",
     "established":"Did Chloe establish that Greg drove his car into a ditch?",
     "confessed":"Did Chloe confess that Greg drove his car into a ditch?"
   },
   "horse": {
     "question":"Kate fell from her horse",
     "MC":"Did Kate fall from her horse?",
     "saw":"Did Andrew see that Kate fell from her horse?",
     "revealed":"Did Andrew reveal that Kate fell from her horse?",
     "is_amused":"Is Andrew amused that Kate fell from her horse?",
     "is_annoyed":"Is Andrew annoyed that Kate fell from her horse?",
     "discovered":"Did Andrew discover that Kate fell from her horse?",
     "learned":"Did Andrew learn that Kate fell from her horse?",
     "found_out":"Did Andrew find out that Kate fell from her horse?",
     "is_aware":"Is Andrew aware that Kate fell from her horse?",
     "realize":"Does Andrew realize that Kate fell from her horse?",
     "noticed":"Did Andrew notice that Kate fell from her horse?",
     "established":"Did Andrew establish that Kate fell from her horse?",
     "confessed":"Did Andrew confess that Kate fell from her horse?"
   },
   "poodle": {
     "question":"Joyce got a poodle",
     "MC":"Did Joyce get a poodle?",
     "saw":"Did Mark see that Joyce got a poodle?",
     "revealed":"Did Mark reveal that Joyce got a poodle?",
     "is_amused":"Is Mark amused that Joyce got a poodle?",
     "is_annoyed":"Is Mark annoyed that Joyce got a poodle?",
     "discovered":"Did Mark discover that Joyce got a poodle?",
     "learned":"Did Mark learn that Joyce got a poodle?",
     "found_out":"Did Mark find out that Joyce got a poodle?",
     "is_aware":"Is Mark aware that Joyce got a poodle?",
     "realize":"Does Mark realize that Joyce got a poodle?",
     "noticed":"Did Mark notice that Joyce got a poodle?",
     "established":"Did Mark establish that Joyce got a poodle?",
     "confessed":"Did Mark confess that Joyce got a poodle?"
   },
   "poem": {
     "question":"Carl wrote a poem for his wife",
     "MC":"Did Carl write a poem for his wife?",
     "saw":"Did Kathryn see that Carl wrote a poem for his wife?",
     "revealed":"Did Kathryn reveal that Carl wrote a poem for his wife?",
     "is_amused":"Is Kathryn amused that Carl wrote a poem for his wife?",
     "is_annoyed":"Is Kathryn annoyed that Carl wrote a poem for his wife?",
     "discovered":"Did Kathryn discover that Carl wrote a poem for his wife?",
     "learned":"Did Kathryn learn that Carl wrote a poem for his wife?",
     "found_out":"Did Kathryn find out that Carl wrote a poem for his wife?",
     "is_aware":"Is Kathryn aware that Carl wrote a poem for his wife?",
     "realize":"Does Kathryn realize that Carl wrote a poem for his wife?",
     "noticed":"Did Kathryn notice that Carl wrote a poem for his wife?",
     "established":"Did Kathryn establish that Carl wrote a poem for his wife?",
     "confessed":"Did Kathryn confess that Carl wrote a poem for his wife?"
   },
   "picture": {
     "question":"Bea posted a family picture on Facebook",
     "MC":"Did Bea post a family picture on Facebook?",
     "saw":"Did Walt see that Bea posted a family picture on Facebook?",
     "revealed":"Did Walt reveal that Bea posted a family picture on Facebook?",
     "is_amused":"Is Walt amused that Bea posted a family picture on Facebook?",
     "is_annoyed":"Is Walt annoyed that Bea posted a family picture on Facebook?",
     "discovered":"Did Walt discover that Bea posted a family picture on Facebook?",
     "learned":"Did Walt learn that Bea posted a family picture on Facebook?",
     "found_out":"Did Walt find out that Bea posted a family picture on Facebook?",
     "is_aware":"Is Walt aware that Bea posted a family picture on Facebook?",
     "realize":"Does Walt realize that Bea posted a family picture on Facebook?",
     "noticed":"Did Walt notice that Bea posted a family picture on Facebook?",
     "established":"Did Walt establish that Bea posted a family picture on Facebook?",
     "confessed":"Did Walt confess that Bea posted a family picture on Facebook?"
   },
   "damp": {
     "question":"Janet moved into a damp apartment",
     "MC":"Did Janet move into a damp apartment?",
     "saw":"Did Randy see that Janet moved into a damp apartment?",
     "revealed":"Did Randy reveal that Janet moved into a damp apartment?",
     "is_amused":"Is Randy amused that Janet moved into a damp apartment?",
     "is_annoyed":"Is Randy annoyed that Janet moved into a damp apartment?",
     "discovered":"Did Randy discover that Janet moved into a damp apartment?",
     "learned":"Did Randy learn that Janet moved into a damp apartment?",
     "found_out":"Did Randy find out that Janet moved into a damp apartment?",
     "is_aware":"Is Randy aware that Janet moved into a damp apartment?",
     "realize":"Does Randy realize that Janet moved into a damp apartment?",
     "noticed":"Did Randy notice that Janet moved into a damp apartment?",
     "established":"Did Randy establish that Janet moved into a damp apartment?",
     "confessed":"Did Randy confess that Janet moved into a damp apartment?"
   },
   "hat": {
     "question":"Samantha bought a fur hat",
     "MC":"Did Samantha buy a fur hat?",
     "saw":"Did Herbert see that Samantha bought a fur hat?",
     "revealed":"Did reveal that Samantha bought a fur hat?",
     "is_amused":"Is Herbert amused that Samantha bought a fur hat?",
     "is_annoyed":"Is Herbert annoyed that Samantha bought a fur hat?",
     "discovered":"Did Herbert discover that Samantha bought a fur hat?",
     "learned":"Did Herbert learn that Samantha bought a fur hat?",
     "found_out":"Did Herbert find out that Samantha bought a fur hat?",
     "is_aware":"Is Herbert aware that Samantha bought a fur hat?",
     "realize":"Does Herbert realize that Samantha bought a fur hat?",
     "noticed":"Did Herbert notice that Samantha bought a fur hat?",
     "established":"Did Herbert establish that Samantha bought a fur hat?",
     "confessed":"Did Herbert confess that Samantha bought a fur hat?"
   },
   "chili": {
     "question":"Don ate a chili dog",
     "MC":"Did Don eat a chili dog?",
     "saw":"Did Helen see that Don ate a chili dog?",
     "revealed":"Did Helen reveal that Don ate a chili dog?",
     "is_amused":"Is Helen amused that Don ate a chili dog?",
     "is_annoyed":"Is Helen annoyed that Don ate a chili dog?",
     "discovered":"Did Helen discover that Don ate a chili dog?",
     "learned":"Did Helen learn that Don ate a chili dog?",
     "found_out":"Did Helen find out that Don ate a chili dog?",
     "is_aware":"Is Helen aware that Don ate a chili dog?",
     "realize":"Does Helen realize that Don ate a chili dog?",
     "noticed":"Did Helen notice that Don ate a chili dog?",
     "established":"Did Helen establish that Don ate a chili dog?",
     "confessed":"Did Helen confess that Don ate a chili dog?"
   },
   "nails": {
     "question":"Mary was biting her nails",
     "MC":"Was Mary biting her nails?",
     "saw":"Did Brad see that Mary was biting her nails?",
     "revealed":"Did Brad reveal that Mary was biting her nails?",
     "is_amused":"Is Brad amused that Mary was biting her nails?",
     "is_annoyed":"Is Brad annoyed that Mary was biting her nails?",
     "discovered":"Did Brad discover that Mary was biting her nails?",
     "learned":"Did Brad learn that Mary was biting her nails?",
     "found_out":"Did Brad find out that Mary was biting her nails?",
     "is_aware":"Is Brad aware that Mary was biting her nails?",
     "realize":"Does Brad realize that Mary was biting her nails?",
     "noticed":"Did Brad notice that Mary was biting her nails?",
     "established":"Did Brad establish that Mary was biting her nails?",
     "confessed":"Did Brad confess that Mary was biting her nails?"
   },
   "pool": {
     "question":"Richie jumped into the pool",
     "MC":"Did Richie jump into the pool?",
     "saw":"Did Jordan see that Richie jumped into the pool?",
     "revealed":"Did Jordan reveal that Richie jumped into the pool?",
     "is_amused":"Is Jordan amused that Richie jumped into the pool?",
     "is_annoyed":"Is Jordan annoyed that Richie jumped into the pool?",
     "discovered":"Did Jordan discover that Richie jumped into the pool?",
     "learned":"Did Jordan learn that Richie jumped into the pool?",
     "found_out":"Did Jordan find out that Richie jumped into the pool?",
     "is_aware":"Is Jordan aware that Richie jumped into the pool?",
     "realize":"Does Jordan realize that Richie jumped into the pool?",
     "noticed":"Did Jordan notice that Richie jumped into the pool?",
     "established":"Did Jordan establish that Richie jumped into the pool?",
     "confessed":"Did Jordan confess that Richie jumped into the pool?"
   },
   "bmw": {
     "question":"Martha came in her new BMW",
     "MC":"Did Martha come in her new BMW?",
     "saw":"Did Cole see that Martha came in her new BMW?",
     "revealed":"Did Cole reveal that Martha came in her new BMW?",
     "is_amused":"Is Cole amused that Martha came in her new BMW?",
     "is_annoyed":"Is Cole annoyed that Martha came in her new BMW?",
     "discovered":"Did Cole discover that Martha came in her new BMW?",
     "learned":"Did Cole learn that Martha came in her new BMW?",
     "found_out":"Did Cole find out that Martha came in her new BMW?",
     "is_aware":"Is Cole aware that Martha came in her new BMW?",
     "realize":"Does Cole realize that Martha came in her new BMW?",
     "noticed":"Did Cole notice that Martha came in her new BMW?",
     "established":"Did Cole establish that Martha came in her new BMW?",
     "confessed":"Did Cole confess that Martha came in her new BMW?"
   },
   "dancing": {
     "question":"Ann was dancing in the corner",
     "MC":"Was Ann dancing in the corner?",
     "saw":"Did Dexter see that Ann was dancing in the corner?",
     "revealed":"Did Dexter reveal that Ann was dancing in the corner?",
     "is_amused":"Is Dexter amused that Ann was dancing in the corner?",
     "is_annoyed":"Is Dexter annoyed that Ann was dancing in the corner?",
     "discovered":"Did Dexter discover that Ann was dancing in the corner?",
     "learned":"Did Dexter learn that Ann was dancing in the corner?",
     "found_out":"Did Dexter find out that Ann was dancing in the corner?",
     "is_aware":"Is Dexter aware that Ann was dancing in the corner?",
     "realize":"Does Dexter realize that Ann was dancing in the corner?",
     "noticed":"Did Dexter notice that Ann was dancing in the corner?",
     "established":"Did Dexter establish that Ann was dancing in the corner?",
     "confessed":"Did Dexter confess that Ann was dancing in the corner?"
   },
   "yoga": {
     "question":"Sue was doing yoga in the yard",
     "MC":"Was Sue doing yoga in the yard?",
     "saw":"Did Anton see that Sue was doing yoga in the yard?",
     "revealed":"Did Anton reveal that Sue was doing yoga in the yard?",
     "is_amused":"Is Anton amused that Sue was doing yoga in the yard?",
     "is_annoyed":"Is Anton annoyed that Sue was doing yoga in the yard?",
     "discovered":"Did Anton discover that Sue was doing yoga in the yard?",
     "learned":"Did Anton learn that Sue was doing yoga in the yard?",
     "found_out":"Did Anton find out that Sue was doing yoga in the yard?",
     "is_aware":"Is Anton aware that Sue was doing yoga in the yard?",
     "realize":"Does Anton realize that Sue was doing yoga in the yard?",
     "noticed":"Did Anton notice that Sue was doing yoga in the yard?",
     "established":"Did Anton establish that Sue was doing yoga in the yard?",
     "confessed":"Did Anton confess that Sue was doing yoga in the yard?"
   }
 };
  
var items_content_mapping = {
"saw":["tea","frisbee","garage","zoo","purple","cupcakes","swing","ditch","horse","poodle","poem","picture","damp","hat","chili","nails","pool","bmw","dancing","yoga"],
"revealed":["tea","frisbee","garage","zoo","purple","cupcakes","swing","ditch","horse","poodle","poem","picture","damp","hat","chili","nails","pool","bmw","dancing","yoga"],
"is_amused":["tea","frisbee","garage","zoo","purple","cupcakes","swing","ditch","horse","poodle","poem","picture","damp","hat","chili","nails","pool","bmw","dancing","yoga"],
"learned":["tea","frisbee","garage","zoo","purple","cupcakes","swing","ditch","horse","poodle","poem","picture","damp","hat","chili","nails","pool","bmw","dancing","yoga"],
"found_out":["tea","frisbee","garage","zoo","purple","cupcakes","swing","ditch","horse","poodle","poem","picture","damp","hat","chili","nails","pool","bmw","dancing","yoga"],
"is_aware":["tea","frisbee","garage","zoo","purple","cupcakes","swing","ditch","horse","poodle","poem","picture","damp","hat","chili","nails","pool","bmw","dancing","yoga"],
"realize":["tea","frisbee","garage","zoo","purple","cupcakes","swing","ditch","horse","poodle","poem","picture","damp","hat","chili","nails","pool","bmw","dancing","yoga"],  
"noticed":["tea","frisbee","garage","zoo","purple","cupcakes","swing","ditch","horse","poodle","poem","picture","damp","hat","chili","nails","pool","bmw","dancing","yoga"],
"established":["tea","frisbee","garage","zoo","purple","cupcakes","swing","ditch","horse","poodle","poem","picture","damp","hat","chili","nails","pool","bmw","dancing","yoga"],
"confessed":["tea","frisbee","garage","zoo","purple","cupcakes","swing","ditch","horse","poodle","poem","picture","damp","hat","chili","nails","pool","bmw","dancing","yoga"],
"discovered":["tea","frisbee","garage","zoo","purple","cupcakes","swing","ditch","horse","poodle","poem","picture","damp","hat","chili","nails","pool","bmw","dancing","yoga"],
"is_annoyed":["tea","frisbee","garage","zoo","purple","cupcakes","swing","ditch","horse","poodle","poem","picture","damp","hat","chili","nails","pool","bmw","dancing","yoga"],	 "MC":["tea","frisbee","garage","zoo","purple","cupcakes","swing","ditch","horse","poodle","poem","picture","damp","hat","chili","nails","pool","bmw","dancing","yoga"]     
};  

// get trigger contents
  function getContent(trigger) {
//  		console.log("items_content_mapping before throwing out "+trigger);
//  		console.log(items_content_mapping);
//  		for (var j in items_content_mapping) {  	
//  		console.log("items_content_mapping at "+j);  			
//  		console.log(items_content_mapping[j]);  		
//  		}  		
//  		console.log("items_content_mapping at the trigger before shuffling");
//  		console.log(items_content_mapping[trigger]);  		
  		items_content_mapping[trigger] = _.shuffle(items_content_mapping[trigger]);
//  		console.log("items_content_mapping at the trigger after shuffling");
//  		console.log(items_content_mapping[trigger]);  		  		
//  		console.log("items_content_mapping after shuffling "+trigger);
//  		console.log(items_content_mapping);
  		var content = items_content_mapping[trigger].shift();//items_content_mapping[trigger][0];
  		console.log("this is the selected content: " + content);
//		var index = items_content_mapping[trigger].indexOf(content);  		
//  		items_content_mapping[trigger] = items_content_mapping[trigger].splice(index,1);
//  		console.log("items_content_mapping at the trigger after throwing it out");
//  		console.log(items_content_mapping[trigger]);  		  		
  		for (var j in items_content_mapping) {
			var index = items_content_mapping[j].indexOf(content);  
			console.log("the next three lines: the array before removal, the index of content, the array after removal")
			console.log(items_content_mapping[j]);
			console.log(index);		
			if (index != -1)
			{			  			
				items_content_mapping[j].splice(index,1);			
			}
			console.log(items_content_mapping[j]);			
			}
//  		console.log("items_content_mapping after throwing out "+trigger);
//  		console.log(items_content_mapping);
//  		for (var j in items_content_mapping) {  	
//  		console.log("items_content_mapping at "+j);  			
//  		console.log(items_content_mapping[j]);  		
//  		}   		  		
  		return content;
  	}
  	  
// assign contents to triggers
  var trigger_contents = {
  	"saw": getContent("saw"),  	  	
  	"revealed": getContent("revealed"),
  	"is_amused": getContent("is_amused"),  	  	
  	"is_annoyed": getContent("is_annoyed"),
  	"discovered": getContent("discovered"),
  	"learned": getContent("learned"),
  	"found_out": getContent("found_out"),  	
  	"is_aware": getContent("is_aware"),  	
  	"realize": getContent("realize"),
  	"noticed": getContent("noticed"),
  	"established": getContent("established"),
  	"confessed": getContent("confessed"),
  	"MC1": getContent("MC"),
  	"MC2": getContent("MC"),  	
  	"MC3": getContent("MC"),
  	"MC4": getContent("MC"),
  	"MC5": getContent("MC"),
  	"MC6": getContent("MC"),
  	"MC7": getContent("MC"),
  	"MC8": getContent("MC")  	
  	};
  

  function makeStim(i) {
    //get item
    var item = items[i];
	//get a speaker
    var name_data = names[i];
    var name = name_data.name;
    var gender = name_data.gender;
    // get content
    var trigger_cont = trigger_contents[item.trigger];
    var trigger = item.trigger;
    var short_trigger = trigger;
    if (trigger.indexOf("MC") != -1) {
    	short_trigger = "MC";
    	}
//  console.log("short_trigger: "+short_trigger);
//	console.log("trigger: "+trigger);
    console.log("trigger_cont: "+trigger_cont);
//    console.log("utterance: "+contents[trigger_cont][short_trigger]);    
//    console.log(contents[trigger_cont]);    
    var utterance = contents[trigger_cont][short_trigger];
    var question = contents[trigger_cont].question;   
//    console.log(contents[trigger_cont]); 
    return {
	  "name": name,
	  "gender": gender,	  
	  "trigger": item.trigger,
	  "short_trigger": short_trigger,	  
	  "trigger_class": item.trigger_class,
      "content": trigger_cont,
      "utterance": utterance,
      "question": question
    }
  }
  exp.stims_block1 = [];
   exp.stims_block2 = []; 
  for (var i=0; i<items.length; i++) {
  	var stim = makeStim(i);
//    exp.stims_block1.push(makeStim(i));
	exp.stims_block1.push(jQuery.extend(true, {}, stim));
	exp.stims_block2.push(jQuery.extend(true, {}, stim));	
  }  
  
console.log(exp.stims_block1);
console.log(exp.stims_block2);   

	exp.stims_block1 = _.shuffle(exp.stims_block1);  
	exp.stims_block2 = _.shuffle(exp.stims_block2); 
	
// decide which block comes first
  var block_order = _.shuffle(["ai","projective"]);
  var block1type = block_order[0];
  var block2type = block_order[1];  
  console.log(block_order);
  console.log(block1type);  
  console.log(block2type);    

   for (k in exp.stims_block2) {
   		exp.stims_block2[k].block = block2type;//block_order[1];   	
   	}
   	
   for (i in exp.stims_block1) {
   		exp.stims_block1[i].block = block1type;//block_order[0];   	
   	}


console.log(exp.stims_block1);
console.log(exp.stims_block2);   	

//  exp.all_stims = [];
//  for (var i=0; i<items.length; i++) {
//    exp.all_stims.push(makeStim(i));
//  }
//
//	for (k in exp.all_stims) {
//		console.log(exp.all_stims[k].content)
//		}

  exp.trials = [];
  exp.catch_trials = [];
  exp.condition = {}; //can randomize between subject conditions here
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  exp.structure=["i0", "instructions", "instructions1", "block1", "instructions2", "block2", 'questionaire', 'finished'];
  
  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

//  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined
                    
   exp.nQs = 3 + 20 + 1 + 20 + 1; 
  $(".nQs").html(exp.nQs);

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });

  exp.go(); //show first slide
}