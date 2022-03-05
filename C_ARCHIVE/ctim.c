#include<sys/time.h>
/*struct itimerval TimePointer; JSW for gfortran */
struct  timeval TimeValue;
struct  timezone TZ;
int Time_type;                         /* variables in InFoExplorer */
int ctim_(int *sec1,int *nsec1)
{
int rc;                                /* return code */
rc = gettimeofday(&TimeValue,&TZ);
*sec1 = TimeValue.tv_sec%86400;        /* Convert to time of day in sec nds */
*nsec1 = 1000*TimeValue.tv_usec;
return(1);
}
