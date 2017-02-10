# Variability in the information structural status of projective content

## JD's tasks

1. write up exp 1 results:
	- Is there variability in projectivity? Yes. (show by-trigger projectivity violin plots)
	- Is projectivity predicted by NAI-ness? Yes: 
		- LME analysis m.proj.2 <- lmer(projective ~ short_trigger + ai + (1+ai|workerid) + (0+ai|content), data=t)
		- show by-trigger ai/proj correlation plot
		- show within-trigger by-content ai/proj correlation plot (projection variability not just across, but also within triggers, content-dependent, and in particular ai-dependent!)
	- Discussion points:	
		- discuss additional lexical trigger effect (ie, it's not ALL NAI-ness)
		- discuss individual variability (looks like about a third of participants is generally at ceiling with projectivity; within the rest, most show the effect in the expected direction) -- show an individual variability plot?
2. analyze exp 2 like exp 1 and write up

## JT comments
- NAI predicts projectivity, but the individual triggers also explain additional variance in projectivity 
	==> look at the factive verb experiment to see if the contents that the triggers were paired with accounts for the variance accounted for by the triggers
	i.e., are the trigger effects reducible to content effects that the triggers were paired with
