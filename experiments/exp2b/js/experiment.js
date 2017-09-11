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
      var utterance = "<table><tr><td><strong>"+this.stim.name + ":</strong> \"<i>"+this.stim.utterance+"</i>\"</td></tr><tr><td><strong>"+this.stim.name2 + ":</strong> \"<i>Are you sure?</i>\""+"</td></tr><tr><td><strong>"+this.stim.name + ": </strong> \"<i>Yes, I'm sure that "+this.stim.question+".</i>\""+"</td></tr></table>"
      // var utterance = "<p>"+this.stim.name + ": \"<i>"+this.stim.utterance+"</i>\"</p>" +"<p>"+this.stim.name2 + ": \"<i>Are you sure?</i>\"</p>"+this.stim.name + ": \"<i>Yes, I'm sure that "+this.stim.question+".</i>\""
	  $(".sentence").html(utterance);
	  var question = "";
	  question = "Did "+this.stim.name+" answer "+this.stim.name2+"'s question?";
	  // console.log(this.stim.block);
// 	  if (this.stim.block == "ai") {
// 	  		question = "Is "+this.stim.name+" asking whether "+this.stim.question+"?";
// 	  } else {
// 	  		question = "Is "+this.stim.name+" certain that "+this.stim.question+"?";	  	
// 	  	}
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
//     {
//       "name":"Margaret",
//       "gender":"F"
//     },
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
  "MC":"Raul was drinking chamomile tea.",
  "saw":"Mandy saw that Raul was drinking chamomile tea.",
  "revealed":"Mandy revealed that Raul was drinking chamomile tea.",
  "is_amused":"Mandy is amused that Raul was drinking chamomile tea.",
  "is_annoyed": "Mandy is annoyed that Raul was drinking chamomile tea.",
  "discovered":"Mandy discovered that Raul was drinking chamomile tea.",
  "learned":"Mandy learned that Raul was drinking chamomile tea.",
  "found_out":"Mandy found out that Raul was drinking chamomile tea.",
  "is_aware":"Mandy is aware that Raul was drinking chamomile tea.",
  "realize":"Mandy realizes that Raul was drinking chamomile tea.",
  "noticed":"Mandy noticed that Raul was drinking chamomile tea.",
  "established":"Mandy established that Raul was drinking chamomile tea.",
  "confessed":"Mandy confessed that Raul was drinking chamomile tea."
 },
 "frisbee": {
  "question":"Jack played frisbee with the kids",
  "MC":"Jack played frisbee with the kids.",
  "saw":"Sarah saw that Jack played frisbee with the kids.",
  "revealed":"Sarah revealed that Jack played frisbee with the kids.",
  "is_amused":"Sarah is amused that Jack played frisbee with the kids.",
  "is_annoyed" :"Sarah is annoyed that Jack played frisbee with the kids.",
  "discovered":"Sarah discovered that Jack played frisbee with the kids.",
  "learned":"Sarah learned that Jack played frisbee with the kids.",
  "found_out":"Sarah found out that Jack played frisbee with the kids.",
  "is_aware":"Sarah is aware that Jack played frisbee with the kids.",
  "realize":"Sarah realizes that Jack played frisbee with the kids.",
  "noticed":"Sarah noticed that Jack played frisbee with the kids.",
  "established":"Sarah established that Jack played frisbee with the kids.",
  "confessed":"Sarah confessed that Jack played frisbee with the kids."
 },
 "garage": {
  "question":"John was hiding  in the garage",
  "MC":"John was hiding  in the garage.",
  "saw":"Kim saw that John was hiding  in the garage.",
  "revealed":"Kim revealed that John was hiding  in the garage.",
  "is_amused":"Kim is amused that John was hiding  in the garage.",
  "is_annoyed" :"Kim is annoyed that John was hiding  in the garage.",
  "discovered":"Kim discovered that John was hiding  in the garage.",
  "learned":"Kim learned that John was hiding  in the garage.",
  "found_out":"Kim found out that John was hiding  in the garage.",
  "is_aware":"Kim is aware that John was hiding  in the garage.",
  "realize":"Kim realizes that John was hiding  in the garage.",
  "noticed":"Kim noticed that John was hiding  in the garage.",
  "established":"Kim established that John was hiding  in the garage.",
  "confessed":"Kim confessed that John was hiding  in the garage."
 },
 "zoo": {
  "question":"Mike visited the zoo",
  "MC":"Mike visited the   zoo.",
  "saw":"Jane saw that Mike visited the zoo.",
  "revealed":"Jane revealed that Mike visited the zoo.",
  "is_amused":"Jane is amused that Mike visited the zoo.",
  "is_annoyed" :"Jane is annoyed that Mike visited the zoo.",
  "discovered":"Jane discovered that Mike visited the zoo.",
  "learned":"Jane learned that Mike visited the zoo.",
  "found_out":"Jane found out that Mike visited the zoo.",
  "is_aware":"Jane is aware that Mike visited the zoo.",
  "realize":"Jane realizes that Mike visited the zoo.",
  "noticed":"Jane noticed that Mike visited the zoo.",
  "established":"Jane established that Mike visited the zoo.",
  "confessed":"Jane confessed that Mike visited the zoo."
 },
 "purple": {
  "question":"Zach dyed his hair purple",
  "MC":"Zach dyed his hair purple.",
  "saw":"Jane saw that Zach dyed his hair purple.",
  "revealed":"Jane revealed that Zach dyed his hair purple.",
  "is_amused":"Jane is amused that Zach dyed his hair purple.",
  "is_annoyed" :"Jane is annoyed that Zach dyed his hair purple.",
  "discovered":"Jane discovered that Zach dyed his hair purple.",
  "learned":"Jane learned that Zach dyed his hair purple.",
  "found_out":"Jane found out that Zach dyed his hair purple.",
  "is_aware":"Jane is aware that Zach dyed his hair purple.",
  "realize":"Jane realizes that Zach dyed his hair purple.",
  "noticed":"Jane noticed that Zach dyed his hair purple.",
  "established":"Jane established that Zach dyed his hair purple.",
  "confessed":"Jane confessed that Zach dyed his hair purple."
 },
 "cupcakes": {
  "question":"Marissa brought almond cupcakes",
  "MC":"Marissa brought almond cupcakes.",
  "saw":"Frank saw that Marissa brought almond cupcakes.",
  "revealed":"Frank revealed that Marissa brought almond cupcakes.",
  "is_amused":"Frank is amused that Marissa brought almond cupcakes.",
  "is_annoyed" :"Frank is annoyed that Marissa brought almond cupcakes.",
  "discovered":"Frank discovered that Marissa brought almond cupcakes.",
  "learned":"Frank learned that Marissa brought almond cupcakes.",
  "found_out":"Frank found out that Marissa brought almond cupcakes.",
  "is_aware":"Frank is aware that Marissa brought almond cupcakes.",
  "realize":"Frank realizes that Marissa brought almond cupcakes.",
  "noticed":"Frank noticed that Marissa brought almond cupcakes.",
  "established":"Frank established that Marissa brought almond cupcakes.",
  "confessed":"Frank confessed that Marissa brought almond cupcakes."
 },
 "swing": {
  "question":"Chad put up a swing in his backyard",
  "MC":"Chad put up a swing in his backyard.",
  "saw":"Andrea saw that Chad put up a swing in his backyard.",
  "revealed":"Andrea revealed that Chad put up a swing in his backyard.",
  "is_amused":"Andrea is amused that Chad put up a swing in his backyard.",
  "is_annoyed" :"Andrea is annoyed that Chad put up a swing in his backyard.",
  "discovered":"Andrea discovered that Chad put up a swing in his backyard.",
  "learned":"Andrea learned that Chad put up a swing in his backyard.",
  "found_out":"Andrea found out that Chad put up a swing in his backyard.",
  "is_aware":"Andrea is aware that Chad put up a swing in his backyard.",
  "realize":"Andrea realizes that Chad put up a swing in his backyard.",
  "noticed":"Andrea noticed that Chad put up a swing in his backyard.",
  "established":"Andrea established that Chad put up a swing in his backyard.",
  "confessed":"Andrea confessed that Chad put up a swing in his backyard."
 },
 "ditch": {
  "question":"Greg drove his car into a ditch",
  "MC":"Greg drove his car into a ditch.",
  "saw":"Chloe saw that Greg drove his car into a ditch.",
  "revealed":"Chloe revealed that Greg drove his car into a ditch.",
  "is_amused":"Chloe is amused that Greg drove his car into a ditch.",
  "is_annoyed" :"Chloe is annoyed that Greg drove his car into a ditch.",
  "discovered":"Chloe discovered that Greg drove his car into a ditch.",
  "learned":"Chloe learned that Greg drove his car into a ditch.",
  "found_out":"Chloe found out that Greg drove his car into a ditch.",
  "is_aware":"Chloe is aware that Greg drove his car into a ditch.",
  "realize":"Chloe realizes that Greg drove his car into a ditch.",
  "noticed":"Chloe noticed that Greg drove his car into a ditch.",
  "established":"Chloe established that Greg drove his car into a ditch.",
  "confessed":"Chloe confessed that Greg drove his car into a ditch."
 },
 "horse": {
  "question":"Kate fell from her horse",
  "MC":"Kate fell from her horse.",
  "saw":"Andrew saw that Kate fell from her horse.",
  "revealed":"Andrew revealed that Kate fell from her horse.",
  "is_amused":"Andrew is amused that Kate fell from her horse.",
  "is_annoyed" :"Andrew is annoyed that Kate fell from her horse.",
  "discovered":"Andrew discovered that Kate fell from her horse.",
  "learned":"Andrew learned that Kate fell from her horse.",
  "found_out":"Andrew found out that Kate fell from her horse.",
  "is_aware":"Andrew is aware that Kate fell from her horse.",
  "realize":"Andrew realizes that Kate fell from her horse.",
  "noticed":"Andrew noticed that Kate fell from her horse.",
  "established":"Andrew established that Kate fell from her horse.",
  "confessed":"Andrew confessed that Kate fell from her horse."
 },
 "poodle": {
  "question":"Joyce got a poodle",
  "MC":"Joyce got a poodle.",
  "saw":"Mark saw that Joyce got a poodle.",
  "revealed":"Mark revealed that Joyce got a poodle.",
  "is_amused":"Mark is amused that Joyce got a poodle.",
  "is_annoyed" :"Mark is annoyed that Joyce got a poodle.",
  "discovered":"Mark discovered that Joyce got a poodle.",
  "learned":"Mark learned that Joyce got a poodle.",
  "found_out":"Mark found out that Joyce got a poodle.",
  "is_aware":"Mark is aware that Joyce got a poodle.",
  "realize":"Mark realizes that Joyce got a poodle.",
  "noticed":"Mark noticed that Joyce got a poodle.",
  "established":"Mark established that Joyce got a poodle.",
  "confessed":"Mark confessed that Joyce got a poodle."
 },
 "poem": {
  "question":"Carl wrote a poem for his wife",
  "MC":"Carl wrote a poem for his wife.",
  "saw":"Kathryn saw that Carl wrote a poem for his wife.",
  "revealed":"Kathryn revealed that Carl wrote a poem for his wife.",
  "is_amused":"Kathryn is amused that Carl wrote a poem for his wife.",
  "is_annoyed" :"Kathryn is annoyed that Carl wrote a poem for his wife.",
  "discovered":"Kathryn discovered that Carl wrote a poem for his wife.",
  "learned":"Kathryn learned that Carl wrote a poem for his wife.",
  "found_out":"Kathryn found out that Carl wrote a poem for his wife.",
  "is_aware":"Kathryn is aware that Carl wrote a poem for his wife.",
  "realize":"Kathryn realizes that Carl wrote a poem for his wife.",
  "noticed":"Kathryn noticed that Carl wrote a poem for his wife.",
  "established":"Kathryn established that Carl wrote a poem for his wife.",
  "confessed":"Kathryn confessed that Carl wrote a poem for his wife."
 },
 "picture": {
  "question":"Bea posted a family picture on Facebook",
  "MC":"Bea posted a family picture on Facebook.",
  "saw":"Walt saw that Bea posted a family picture on Facebook.",
  "revealed":"Walt revealed that Bea posted a family picture on Facebook.",
  "is_amused":"Walt is amused that Bea posted a family picture on Facebook.",
  "is_annoyed" :"Walt is annoyed that Bea posted a family picture on Facebook.",
  "discovered":"Walt discovered that Bea posted a family picture on Facebook.",
  "learned":"Walt learned that Bea posted a family picture on Facebook.",
  "found_out":"Walt found out that Bea posted a family picture on Facebook.",
  "is_aware":"Walt is aware that Bea posted a family picture on Facebook.",
  "realize":"Walt realizes that Bea posted a family picture on Facebook.",
  "noticed":"Walt noticed that Bea posted a family picture on Facebook.",
  "established":"Walt established that Bea posted a family picture on Facebook.",
  "confessed":"Walt confessed that Bea posted a family picture on Facebook."
 },
 "damp": {
  "question":"Janet moved into a damp apartment",
  "MC":"Janet moved into a damp apartment.",
  "saw":"Randy saw that Janet moved into a damp apartment.",
  "revealed":"Randy revealed that Janet moved into a damp apartment.",
  "is_amused":"Randy is amused that Janet moved into a damp apartment.",
  "is_annoyed" :"Randy is annoyed that Janet moved into a damp apartment.",
  "discovered":"Randy discovered that Janet moved into a damp apartment.",
  "learned":"Randy learned that Janet moved into a damp apartment.",
  "found_out":"Randy found out that Janet moved into a damp apartment.",
  "is_aware":"Randy is aware that Janet moved into a damp apartment.",
  "realize":"Randy realizes that Janet moved into a damp apartment.",
  "noticed":"Randy noticed that Janet moved into a damp apartment.",
  "established":"Randy established that Janet moved into a damp apartment.",
  "confessed":"Randy confessed that Janet moved into a damp apartment."
 },
 "hat": {
  "question":"Samantha bought a fur hat",
  "MC":"Samantha bought a fur hat.",
  "saw":"Herbert saw that Samantha bought a fur hat.",
  "revealed":"revealed that Samantha bought a fur hat.",
  "is_amused":"Herbert is amused that Samantha bought a fur hat.",
  "is_annoyed" :"Herbert is annoyed that Samantha bought a fur hat.",
  "discovered":"Herbert discovered that Samantha bought a fur hat.",
  "learned":"Herbert learned that Samantha bought a fur hat.",
  "found_out":"Herbert found out that Samantha bought a fur hat.",
  "is_aware":"Herbert is aware that Samantha bought a fur hat.",
  "realize":"Herbert realizes that Samantha bought a fur hat.",
  "noticed":"Herbert noticed that Samantha bought a fur hat.",
  "established":"Herbert established that Samantha bought a fur hat.",
  "confessed":"Herbert confessed that Samantha bought a fur hat."
 },
 "chili": {
  "question":"Don ate a chili dog",
  "MC":"Don ate a chili dog.",
  "saw":"Helen saw that Don ate a chili dog.",
  "revealed":"Helen revealed that Don ate a chili dog.",
  "is_amused":"Helen is amused that Don ate a chili dog.",
  "is_annoyed" :"Helen is annoyed that Don ate a chili dog.",
  "discovered":"Helen discovered that Don ate a chili dog.",
  "learned":"Helen learned that Don ate a chili dog.",
  "found_out":"Helen found out that Don ate a chili dog.",
  "is_aware":"Helen is aware that Don ate a chili dog.",
  "realize":"Helen realizes that Don ate a chili dog.",
  "noticed":"Helen noticed that Don ate a chili dog.",
  "established":"Helen established that Don ate a chili dog.",
  "confessed":"Helen confessed that Don ate a chili dog."
 },
 "nails": {
  "question":"Mary was biting her nails",
  "MC":"Mary was biting her nails.",
  "saw":"Brad saw that Mary was biting her nails.",
  "revealed":"Brad revealed that Mary was biting her nails.",
  "is_amused":"Brad is amused that Mary was biting her nails.",
  "is_annoyed" :"Brad is annoyed that Mary was biting her nails.",
  "discovered":"Brad discovered that Mary was biting her nails.",
  "learned":"Brad learned that Mary was biting her nails.",
  "found_out":"Brad found out that Mary was biting her nails.",
  "is_aware":"Brad is aware that Mary was biting her nails.",
  "realize":"Brad realizes that Mary was biting her nails.",
  "noticed":"Brad noticed that Mary was biting her nails.",
  "established":"Brad established that Mary was biting her nails.",
  "confessed":"Brad confessed that Mary was biting her nails."
 },
 "pool": {
  "question":"Richie jumped into the pool",
  "MC":"Richie jumped into the pool.",
  "saw":"Jordan saw that Richie jumped into the pool.",
  "revealed":"Jordan revealed that Richie jumped into the pool.",
  "is_amused":"Jordan is amused that Richie jumped into the pool.",
  "is_annoyed" :"Jordan is annoyed that Richie jumped into the pool.",
  "discovered":"Jordan discovered that Richie jumped into the pool.",
  "learned":"Jordan learned that Richie jumped into the pool.",
  "found_out":"Jordan found out that Richie jumped into the pool.",
  "is_aware":"Jordan is aware that Richie jumped into the pool.",
  "realize":"Jordan realizes that Richie jumped into the pool.",
  "noticed":"Jordan noticed that Richie jumped into the pool.",
  "established":"Jordan established that Richie jumped into the pool.",
  "confessed":"Jordan confessed that Richie jumped into the pool."
 },
 "bmw": {
  "question":"Martha came in her new BMW",
  "MC":"Martha came in her new BMW.",
  "saw":"Cole saw that Martha came in her new BMW.",
  "revealed":"Cole revealed that Martha came in her new BMW.",
  "is_amused":"Cole is amused that Martha came in her new BMW.",
  "is_annoyed" :"Cole is annoyed that Martha came in her new BMW.",
  "discovered":"Cole discovered that Martha came in her new BMW.",
  "learned":"Cole learned that Martha came in her new BMW.",
  "found_out":"Cole found out that Martha came in her new BMW.",
  "is_aware":"Cole is aware that Martha came in her new BMW.",
  "realize":"Cole realizes that Martha came in her new BMW.",
  "noticed":"Cole noticed that Martha came in her new BMW.",
  "established":"Cole established that Martha came in her new BMW.",
  "confessed":"Cole confessed that Martha came in her new BMW."
 },
 "dancing": {
  "question":"Ann was dancing in the corner",
  "MC":"Ann was dancing in the corner.",
  "saw":"Dexter saw that Ann was dancing in the corner.",
  "revealed":"Dexter revealed that Ann was dancing in the corner.",
  "is_amused":"Dexter is amused that Ann was dancing in the corner.",
  "is_annoyed" :"Dexter is annoyed that Ann was dancing in the corner.",
  "discovered":"Dexter discovered that Ann was dancing in the corner.",
  "learned":"Dexter learned that Ann was dancing in the corner.",
  "found_out":"Dexter found out that Ann was dancing in the corner.",
  "is_aware":"Dexter is aware that Ann was dancing in the corner.",
  "realize":"Dexter realizes that Ann was dancing in the corner.",
  "noticed":"Dexter noticed that Ann was dancing in the corner.",
  "established":"Dexter established that Ann was dancing in the corner.",
  "confessed":"Dexter confessed that Ann was dancing in the corner."
 },
 "yoga": {
  "question":"Sue was doing yoga in the yard",
  "MC":"Sue was doing yoga in the yard.",
  "saw":"Anton saw that Sue was doing yoga in the yard.",
  "revealed":"Anton revealed that Sue was doing yoga in the yard.",
  "is_amused":"Anton is amused that Sue was doing yoga in the yard.",
  "is_annoyed" :"Anton is annoyed that Sue was doing yoga in the yard.",
  "discovered":"Anton discovered that Sue was doing yoga in the yard.",
  "learned":"Anton learned that Sue was doing yoga in the yard.",
  "found_out":"Anton found out that Sue was doing yoga in the yard.",
  "is_aware":"Anton is aware that Sue was doing yoga in the yard.",
  "realize":"Anton realizes that Sue was doing yoga in the yard.",
  "noticed":"Anton noticed that Sue was doing yoga in the yard.",
  "established":"Anton established that Sue was doing yoga in the yard.",
  "confessed":"Anton confessed that Sue was doing yoga in the yard."
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
//  		console.log("this is the selected content: " + content);
//		var index = items_content_mapping[trigger].indexOf(content);  		
//  		items_content_mapping[trigger] = items_content_mapping[trigger].splice(index,1);
//  		console.log("items_content_mapping at the trigger after throwing it out");
//  		console.log(items_content_mapping[trigger]);  		  		
  		for (var j in items_content_mapping) {
			var index = items_content_mapping[j].indexOf(content);  
//			console.log("the next three lines: the array before removal, the index of content, the array after removal")
//			console.log(items_content_mapping[j]);
//			console.log(index);		
			if (index != -1)
			{			  			
				items_content_mapping[j].splice(index,1);			
			}
//			console.log(items_content_mapping[j]);			
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
    //get another speaker
    var name_data2 = names[i+1];
    var name2 = name_data2.name;
    var gender2 = name_data2.gender;
    
    // get content
    var trigger_cont = trigger_contents[item.trigger];
    var trigger = item.trigger;
    var short_trigger = trigger;
    if (trigger.indexOf("MC") != -1) {
    	short_trigger = "MC";
    	}
//	console.log("short_trigger: "+short_trigger);
//	console.log("trigger: "+trigger);
//    console.log("trigger_cont: "+trigger_cont);
//    console.log("utterance: "+contents[trigger_cont][short_trigger]);    
//    console.log(contents[trigger_cont]);    
    var utterance = contents[trigger_cont][short_trigger];
    var question = contents[trigger_cont].question;   
//    console.log(contents[trigger_cont]); 
    return {
	  "name": name,
	  "name2": name2,
	  "gender": gender,	
	  "gender2": gender2,  
	  "trigger": item.trigger,
	  "short_trigger": short_trigger,	  
	  "trigger_class": item.trigger_class,
      "content": trigger_cont,
      "utterance": utterance,
      "question": question
    }
  }
  exp.stims_block1 = [];
//   exp.stims_block2 = []; 
  for (var i=0; i<items.length; i++) {
  	var stim = makeStim(i);
//    exp.stims_block1.push(makeStim(i));
	exp.stims_block1.push(jQuery.extend(true, {}, stim));
//	exp.stims_block2.push(jQuery.extend(true, {}, stim));	
  }  
  
console.log(exp.stims_block1);
//console.log(exp.stims_block2);   

	exp.stims_block1 = _.shuffle(exp.stims_block1);  
//	exp.stims_block2 = _.shuffle(exp.stims_block2); 
	
// decide which block comes first
//   var block_order = _.shuffle(["ai","projective"]);
//   var block1type = block_order[0];
//   var block2type = block_order[1];  
//   console.log(block_order);
//   console.log(block1type);  
//   console.log(block2type);    
// 
//    for (k in exp.stims_block2) {
//    		exp.stims_block2[k].block = block2type;//block_order[1];   	
//    	}
//    	
//    for (i in exp.stims_block1) {
//    		exp.stims_block1[i].block = block1type;//block_order[0];   	
//    	}


console.log(exp.stims_block1);
//console.log(exp.stims_block2);   	

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
  exp.structure=["i0", "instructions", "block1", 'questionaire', 'finished'];
  
  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

//  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined
                    
   exp.nQs = 2 + 20 + 1; 
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