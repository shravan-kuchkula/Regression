# Randomized Block Design
Shravan Kuchkula  
10/10/2017  
## Introduction

Suppose you're interested in the T-cell counts of patients taking one of three medications, including a placebo, and you conduct an experiment. In this design, your primary variable, or factor of interest, is the type of medication. But you later realize that other factors, nuisance factors, play a role in the results of your experiment. For example, you believe that the ages of the patients greatly affect the responses to the medications. In an effort to control the factors that contribute to the outcome you want to measure, you redesign your experiment to group the patients by age groups: under 30, 30 to 50, and over 50. You're not interested in the effect of age on T-cell counts, but you're interested in grouping, or blocking by age to minimize variability, thus leading to greater precision. Here's what you learn in this topic.

- Recognize the difference b/w a completely randomized design and a randomized blocked design.
- Differentiate b/w observed data and designed experiments.
- Analyze data from a randomized block design in R.

## Questions

- What is a Randomized block design ?
- Waht is an observational study ?
- What is a Randomized study ?

## Observation Study

Observational Studies Versus Controlled Experiments

An observational, or retrospective, study is when you want to draw inferences about the effects of a treatment on subjects, but the assignment of the subjects into a treated group versus a controlled group is outside of your control. For example, in an observational study, the gender or ethnicity of the subjects naturally occurs, so you simply record this data as you observe it because it is what it is. Oftentimes in an observational study, you look back at data that's already been collected because it's the best that you can do based on resource issues or ethical issues. For example, it's unethical to assign someone to a smoking or a non-smoking group, so you attempt to answer questions like "Does smoking cause lung cancer" by looking at data where subjects have not been randomly assigned to smoke, but where you merely observe whether someone smokes. In an observational study, you sometimes have very little control over other factors that contribute to the outcome you're measuring. 

In a controlled experiment, you have the flexibility to design the analysis prospectively and control for other factors that contribute to the outcome that you're measuring. You can do this by blocking, or grouping, to minimize variability. You might also design a controlled experiment with the intention of reducing selection bias. For example, you randomly assign each subject to a treatment group or a control group before the start of the experiment. Randomization lessens the effects of things you can't control for in your experiment.

## Nuisance Factors
Nuisance factors are factors that can affect the outcome of your experiment but are not of interest in your study. For example, in the T-cell count scenario, a factor that might affect the measured results, that is, patient responses to the medications, is patient age, but the effect of age isn’t your primary interest. The variation due to Age becomes part of the random variation. In a randomized block design, you can use a blocking variable to control for the nuisance factors and reduce or eliminate their contribution to the experimental error. You typically need to spend some time deciding which nuisance factors are important enough to keep track of or, if possible, to control during the experiment.


