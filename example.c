/*
   Simple Xlib application drawing a box in a window.
 */
 
 #include <X11/Xlib.h>
 #include <stdio.h>
 #include <stdlib.h>
 #include <string.h>
 
 int main() 
  {
   char *display_name = NULL;
   Display *d;
   int s;
   Window w;
   XEvent e;
 
   printf("EXAMPLE.C - about to open display\n");
   if( (d=XOpenDisplay(display_name)) == NULL)                /* open connection with the server */
               {
                printf("EXAMPLE.C - Cannot open display %s\n",XDisplayName(display_name));
                exit(1);                                      /* Failure... */
               }
    printf("EXAMPLE.C: has a good window on display %20d\n",d);
   s=DefaultScreen(d);
   w=XCreateSimpleWindow(d, RootWindow(d, s), 10, 10, 220, 100, 1,
                         BlackPixel(d, s), WhitePixel(d, s)); /* create window */
   XSelectInput(d, w,  ExposureMask       | VisibilityChangeMask | ButtonPressMask
                     | ButtonReleaseMask  | PropertyChangeMask   | StructureNotifyMask 
                     | ResizeRedirectMask | KeyPressMask         | KeyReleaseMask | PointerMotionMask
                     | FocusChangeMask);           /* select kind of events we are interested in */
   XMapWindow(d, w);                                          /* map (show) the window */
   while(1)
      {                                /* event loop */
       XNextEvent(d, &e);
       printf("Event code is %10d\n",e.type);
/*       printf("   Event name: %s\n",e.type);  */
       if(e.type==Expose)
         {                             /* draw or redraw the window */
          XFillRectangle(d, w, DefaultGC(d, s), 20, 20, 10, 10);
          XDrawString(d, w, DefaultGC(d, s), 2, 50, "Press any alphanumeric key to exit",34);
         }
       if(e.type==KeyPress) break;     /* exit on key press */
      }
   XCloseDisplay(d);                   /* close connection to server */
   return 0;
  }
