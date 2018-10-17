#include <R.h>
#include <Rinternals.h>
#include <Rmath.h> 
#include <math.h> 
#include <stdlib.h>
#include <float.h>
#include <stdbool.h>
#include <stdio.h>

#include "Functions.h"

/*static void check_user_interrupt_handler() {R_CheckUserInterrupt();}

bool check_user_interrupt() {return R_ToplevelExec(check_user_interrupt_handler,NULL) == FALSE;}*/


void updatewithobservation_poisson(int ii, struct orderedobservationlist_mean *list, double penaltychange)
{
	
	double factor = 0, x, saving;

	x        = list[ii].observation;

     	struct orderedobservationlist_mean* current = NULL;
	current = list[0].next;

	while (current->numberofobservation < ii+1)
	{

		factor  = (ii - current->numberofobservation + 1);
		current->cumulativesum = current->cumulativesum + (x - current->cumulativesum)/factor;

		saving = factor;

		if(current->cumulativesum > DBL_MIN)
		{
			saving = factor*(1-current->cumulativesum + current->cumulativesum * log(current->cumulativesum));
		} 

		current->segmentcost = current->optimalcostofprevious - saving + penaltychange;
		current = current->next;
	}

}

void findoptimaloption_poisson(int ii, struct orderedobservationlist_mean *list, int minseglength, double penaltyoutlier)
{
	
	int option = 0;
	struct orderedobservationlist_mean *bestcut = NULL;
	double optimalscore = 0, scoreanomalous = 0, saving = 0;
	
	optimalscore = list[ii].optimalcostofprevious;
	bestcut= &(list[ii-1]);
	option = 0;

	if(list[ii].observation < DBL_MIN)
	{
		saving = 1;
	} 
	else 
	{
		saving = 1 - list[ii].observation + list[ii].observation * log(list[ii].observation);
	}

	scoreanomalous = list[ii].optimalcostofprevious - saving + penaltyoutlier;
	
	if (scoreanomalous < optimalscore)
	{
		optimalscore = scoreanomalous;
		option = 1;
	}

	struct orderedobservationlist_mean* currentcheck = NULL;
	currentcheck = list[0].next;

	while (currentcheck->numberofobservation < ii - minseglength + 2)
	{
		if (currentcheck->segmentcost < optimalscore)
		{
			bestcut   = &(list[currentcheck->numberofobservation-1]);
			option = 2;
			optimalscore = currentcheck->segmentcost;
		}

		currentcheck = currentcheck->next;
	}	
	
	list[ii].optimalcut              = bestcut;
	list[ii].optimalcost             = optimalscore;
	list[ii].option                  = option;
	list[ii+1].optimalcostofprevious = optimalscore;
}


int solveorderedobservationlist_poisson(struct orderedobservationlist_mean *list, int n, double penaltychange, double penaltyoutlier, int minseglength, int maxseglength)
{

	int ii = 1;

	for (ii = 1; ii < n+1; ii++)
	{
	  
		updatewithobservation_poisson(ii,list,penaltychange);
		findoptimaloption_poisson(ii,list,minseglength,penaltyoutlier);
		pruner_mean(list,ii,penaltychange,minseglength,maxseglength);
		
		if (ii % 128 == 0)
		{
		  if(check_user_interrupt())
		  {
		    return(1);  
		  }
		}
		
	}
	
	return(0);

}















