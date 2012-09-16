Power analysis for a human computer interaction experiment. n subjects are assigned two data entry tasks t1 and t2. If subject Bob performs task t1 with method A then enters task t2 with method B as so:

Bob 	task1 MethodA
Bob 	task2 MethodB
Alice 	task2 MethodA
Alice	task1 MethodB

And so on for n subjects. The distribution of task completion times depends on the person, task, and method in the generalized linear model. It is encouraging to see a highly significant effect on Method using relaxed parameters on variance. The R script includes the ANOVA.

![Rplot](https://raw.github.com/raold/Power-Analysis/master/Rplot.png)