#include <time.h>
#include <stdio.h>

struct timespec request, remaining;

void ns_(int *nsecs) 
{
   int rc;
       request.tv_sec = 0;
       request.tv_nsec = *nsecs;
       rc = nanosleep(&request,&remaining);
   if (rc!=0)
      {
       printf("ERROR - rc from nanosleep: %i\n",rc);
      }
}
