#include <float.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#include "Functions_mean.h"

namespace anomalymv
{

void populate_mean(struct orderedobservationlist_mean **list, double* x , int n, int p, int l) 
{

	int ii = 0, jj = 0;

	*list = (struct orderedobservationlist_mean*) calloc(n + l + 2, sizeof( struct orderedobservationlist_mean ) );

	struct orderedobservationlist_mean *mylist = *list;

	for (ii = 0; ii < n+l+2; ii ++){

		mylist[ii].numberofobservation = ii;

		mylist[ii].observation        = NULL;
		mylist[ii].mean_of_xs         = NULL;

		mylist[ii].segmentcosts          = NULL;
		mylist[ii].best_end_costs        = NULL;

		mylist[ii].optimalcostofprevious =    0;
		mylist[ii].optimalcost           =    0;
		mylist[ii].costofstartingsegment =    0;
	
		mylist[ii].affectedcomponents = NULL;
		mylist[ii].startlag           = NULL;  
		mylist[ii].endlag             = NULL;

		mylist[ii].optimalcut         = NULL;
		mylist[ii].option             =   -1;

		mylist[ii].destruction = n + 100;
		mylist[ii].next        =    NULL;
		mylist[ii].previous    =    NULL;

	}


	mylist[0].next       = &(mylist[1]);
	mylist[n+l+1].previous = &(mylist[n+l]);


	for (ii = 1; ii < n+l+1; ii++)
	{

		mylist[ii].observation        = (double *) calloc( p, sizeof(double) );
		mylist[ii].mean_of_xs         = (double *) calloc( p, sizeof(double) );

		mylist[ii].segmentcosts   = (double *) calloc( p * (l+1), sizeof(double) );
		mylist[ii].best_end_costs = (double *) calloc( p        , sizeof(double) );
	
		mylist[ii].affectedcomponents = (int *) calloc( p, sizeof(int) );
		mylist[ii].startlag           = (int *) calloc( p, sizeof(int) ); 
		mylist[ii].endlag             = (int *) calloc( p, sizeof(int) );

		for (jj = 0; jj < p; jj ++)
		{

			mylist[ii].observation[jj]        = x[n*jj+ii-1];
			mylist[ii].mean_of_xs[jj]         = 0.0;
			mylist[ii].best_end_costs[jj]     = 100;

			mylist[ii].affectedcomponents[jj] = 0;
			mylist[ii].startlag[jj]           = 0;
			mylist[ii].endlag[jj]             = 0;

		}

		for (jj = 0; jj < p* (l+1); jj ++)
		{

			mylist[ii].segmentcosts[jj] = 100;

		}

		mylist[ii].next        = &(mylist[ii+1]);
		mylist[ii].previous    = &(mylist[ii-1]);

	}


}

} // namespace anomalymv
