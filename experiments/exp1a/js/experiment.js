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
    {
      "name":"Richard",
      "gender":"M"
    },
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
    {
      "name":"Mark",
      "gender":"M"
    },
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
    {
      "name":"Andrew",
      "gender":"M"
    },
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
    {
      "name":"Frank",
      "gender":"M"
    },
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
    {
      "name":"Susan",
      "gender":"F"
    },
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
    {
      "name":"Sarah",
      "gender":"F"
    },
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
    {
      "name":"Helen",
      "gender":"F"
    },
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
    {
      "name":"Amanda",
      "gender":"F"
    },
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
    {
      "name":"Catherine",
      "gender":"F"
    },
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
     "trigger":"NomApp",
     "trigger_class":"B"
   }, 
   {
     "trigger":"NRRC",
     "trigger_class":"B"
   }, 
   {
     "trigger":"possNP",
     "trigger_class":"B"
   }, 
   {
     "trigger":"annoyed",
     "trigger_class":"C"
   },
   {
     "trigger":"discover",
     "trigger_class":"C"
   },
   {
     "trigger":"only",
     "trigger_class":"C"
   }, 
   {
     "trigger":"stop",
     "trigger_class":"C"
   }, 
   {
     "trigger":"stupid",
     "trigger_class":"C"
   }, 
   {
     "trigger":"know",
     "trigger_class":"C"
   }
 ]);

 var contents = {
   "muffins": {
     "question":"these muffins have blueberries in them",
     "MC":"Do these muffins have blueberries in them?",
     "NRRC":"Are these muffins, which have blueberries in them, gluten-free and low-fat?",
     "only":"Do these muffins only have blueberries in them?"
   },
   "pizza": {
     "question":"this pizza has mushrooms on it",
     "MC":"Does this pizza have mushrooms on it?",
     "only":"Does this pizza only have mushrooms on it?",
     "annoyed":"Is Sam annoyed that this pizza has mushrooms on it?",
     "discover":"Did Sam discover that this pizza has mushrooms on it?"
   },
	"play": {
     "question":"Jack was playing outside with the kids",
     "MC":"Was Jack playing outside with the kids?",
     "stop":"Did Jack stop playing outside with the kids?",
     "know":"Does Daria know that Jack was playing outside with the kids?",
     "discover":"Did Paula discover that Jack was playing outside with the kids?"
   },
   "veggie": {
     "question":"Don is a vegetarian",
     "NomApp":"Is Don, a vegetarian, going to find something to eat here?",
     "NRRC":"Is Don, who is a vegetarian, going to find something to eat here?",
     "MC":"Is Don a vegetarian?"
   },
   	"cheat": {
     "question":"Raul cheated on his wife",
     "MC":"Did Raul cheat on his wife?",
     "know":"Does Daria know that Raul cheated on his wife?",
     "stupid":"Was Raul stupid to cheat on his wife?"
   },
	"nails": {
     "question":"Mary's daughter has been biting her nails",
     "MC":"Has Mary's daughter been biting her nails?",
     "discover":"Did Mary discover that her daughter has been biting her nails?",
     "stop":"Has Mary's daughter stopped biting her nails?",
     "stupid":"Is Mary's daughter stupid to be biting her nails?"
   },
   "ballet": {
     "question":"Ann used to dance ballet",
     "MC":"Did Ann use to dance ballet?",
     "NomApp":"Is Ann, a former ballet dancer, limping?",
     "stop":"Did Ann stop dancing ballet?"
   },
   "kids": {
     "question":"John's kids were in the garage",
     "only":"Were John's kids only in the garage?",
     "MC":"Were John's kids in the garage?",
     "stupid":"Were John's kids stupid to be in the garage?"
   },
   "hat": {
     "question":"Samantha has a new hat",
     "MC":"Does Samantha have a new hat?",
     "possNP":"Was Samantha's new hat expensive?",
     "know":"Does Daria know that Samantha has a new hat?",
     "annoyed":"Is Joyce annoyed that Samantha has a new hat?"
   },
   "bmw": {
     "question":"Martha has a new BMW",
     "MC":"Does Martha have a new BMW?",
     "possNP":"Was Martha's new BMW expensive?",
     "NomApp":"Was Martha's new car, a BMW, expensive?",
     "annoyed":"Is Martha's neighbor annoyed that Martha has a new BMW?",
     "know":"Does Billy know that Martha has a new BMW?"
   },
   "boyfriend": {
     "question":"Betsy has a boyfriend",
     "MC":"Does Betsy have a boyfriend?",
     "NRRC":"Is Betsy, who has a boyfriend, flirting with the neighbor?",
     "possNP":"Is Betsy's boyfriend from around here?"
   },
   "alcatraz": {
     "question":"Mike visited Alcatraz",
     "MC":"Did Mike visit Alcatraz?",
     "NRRC":"Is Mike, who visited Alcatraz, a history fan?",
     "discover":"Did Jane discover that Mike visited Alcatraz?",
     "know":"Does Jane know that Mike visited Alcatraz?"
   },
   "aunt": {
     "question":"Janet has a sick aunt",
     "MC":"Does Janet have a sick aunt?",
     "NRRC":"Is Janet, who has a sick aunt, very compassionate?",
     "know":"Does Melissa know that Janet has a sick aunt?",
     "possNP":"Has Janet's sick aunt been recovering?"
   },
   "cupcakes": {
     "question":"Marissa brought the cupcakes",
     "MC":"Did Marissa bring the cupcakes?",
     "NRRC":"Is Marissa, who brought the cupcakes, a good baker?",
     "know":"Does Max know that Marissa brought the cupcakes?",
   },
   "soccer": {
     "question":"the soccer ball has a hole in it",
     "MC":"Does the soccer ball have a hole in it?",
     "NRRC":"Was the soccer ball, which has a hole in it, a gift from Uncle Bill?",
     "annoyed":"Is Mandy annoyed that the soccer ball has a hole in it?",
     "discover":"Did Mandy discover that the soccer ball has a hole in it?",
     "know":"Does Mandy know that the soccer ball has a hole in it?"
   },
   "olives": {
   	"question":"this bread has olives in it",
   	"MC":"Does this bread have olives in it?",
   	"annoyed":"Is Barbara annoyed that this bread has olives in it?",
   	},
   	"stuntman": {
   	"question":"Richie is a stuntman",
   	"MC":"Is Richie a stuntman?",
   	"NomApp":"Did Richie, a stuntman, break his leg?",
   	"stupid":"Is Richie stupid to be a stuntman?"
   	}
 };
  
var items_content_mapping = {
     "only":["muffins","kids","pizza"],
     "stop":["play","nails","ballet"],	
     "stupid":["kids","cheat","nails","stuntman"],
     "NomApp":["bmw","veggie","ballet","stuntman"],
     "possNP":["hat","bmw","boyfriend","aunt"],     
     "discover":["play","pizza","nails","alcatraz","soccer"],
     "annoyed":["hat","bmw","pizza","soccer","olives"],
     "NRRC":["veggie","boyfriend","muffins","alcatraz","aunt","cupcakes","soccer"],
     "know":["play","cheat","hat","alcatraz","aunt","cupcakes","soccer","bmw"],
 	 "MC":["muffins","ballet","cheat","veggie","kids","boyfriend","alcatraz","aunt","cupcakes","soccer","olives","pizza","play","nails","hat","bmw","stuntman"]     
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
  	"only": getContent("only"),  	  	
  	"stop": getContent("stop"),
  	"stupid": getContent("stupid"),  	  	
  	"NomApp": getContent("NomApp"),
  	"possNP": getContent("possNP"),
  	"discover": getContent("discover"),
  	"annoyed": getContent("annoyed"),  	
  	"NRRC": getContent("NRRC"),  	
  	"know": getContent("know"),
  	"MC1": getContent("MC"),
  	"MC2": getContent("MC"),  	
  	"MC3": getContent("MC"),
  	"MC4": getContent("MC"),
  	"MC5": getContent("MC"),
  	"MC6": getContent("MC")  	
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
                    
   exp.nQs = 2 + 15 + 1 + 15 + 1; 
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