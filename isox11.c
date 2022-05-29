#include <stdio.h>           
#include <stdlib.h>
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
/*
#include <X11/Xos.h> 
*/
#include <X11/cursorfont.h>
#include <X11/Xcms.h>       
#include <X11/keysym.h>     
#include "keydefs.h"
#include <X11/X.h>     
#include <X11/Xatom.h>     
/*
#include <X11/bitmaps/gray1>
#include <X11/Xmu/WinUtil.h>     
#include <X11/extensions/shape.h>
*/

#define BORDER_WIDTH 2
#define MAXLINE 132
/***********/
/* Globals */
/***********/

   Cursor             cursor;
   KeySym             myKeysym;
   char               buffer[MAXLINE],*isodisplayname;
   XComposeStatus     myStatus;
   Status             tmpstatus,event_msg;
   XSizeHints         sizehints;
   XWindowAttributes  win_attributes;
   XSetWindowAttributes  set_win_attributes;
   XTextItem          items[100];
   XImage             *isoimage;
   XVisualInfo        vistemplate, *vinfo, vismatchinfo;
   XPoint             *polypoints;
   Display            *isodisplay;
   Visual             *visual;
   Window             window,ChildReturn,RootReturn,TextWindow,JSWindow;
   Pixmap             PicturePixmap,PicturePixmap_B;
   XEvent             xev;
   XKeyEvent          xkev;
   XKeyPressedEvent   xkpev;
   XColor             rectcolor,black;
   Colormap           cmap,cmap1;
   GC                 gc,gcb,gcw,gctext;
   XGCValues          gcv,gcbv,gcwv,gctextv;
   Screen             *scr;
   int                ncell[256],nred[256],ngreen[256],nblue[256];
   int                i,wx,wy,x1,y1a,x2,y2,ncol,xouter,youter;
   int                nScreen,visual_class,visual_depth;
   int                shading,mapping,last_font_id;
   int                mycol,mapcols,newcols,map_calc_cols,junk,rx,rx1,rx2;  
   int                *maxcols,*rc,*isocols;/* Returned values to FORTRAN */
   int                *winx1,*winy1,*winx2,*winy2,*maxwidth,*maxheight,*White;
   unsigned int       *winwidth,*winheight;
   int                *nbut,*mousex,*mousey,*expose,*resize,*newx,*newy;
   double             phiredf,phigreenf,phibluef;  /* Colour phases at start  */
   double             aredf,  agreenf,  abluef;    /* Colour amplitudes       */
   double             predf,  pgreenf,  pbluef;    /* Colour periods in cycle */
   double             phired,phigreen,phiblue;  /* Colour phases at start  */
   double             ared,  agreen,  ablue;    /* Colour amplitudes       */
   double             pred,  pgreen,  pblue;    /* Colour periods in cycle */
   float              pi;
   Window             winRoot;
   unsigned long int  wBorder;
   unsigned long int  wBackground;
   FILE               * isolog;
   FILE               * isocmap;

/*
 * Lookup: lookup a code in a table. (From /usr/lpp/X11/Xamples/clients/xwininfo.c)
 */
typedef struct {
        long code;
        char *name;
} binding;

static char _lookup_buffer[100];

char *Lookup(code, table)
    int code;
    binding *table;
{
        char *name;

        name = _lookup_buffer;

        while (table->name) {
                if (table->code == code) {
                        name = table->name;
                        break;
                }
                table++;
        }

        return(name);
}


/*             WINDOW STUFF       

display     DisplayPtr   Specifies the connection to the X server.
winRoot     Parent       Specifies the parent window ID.
nScreen     ScreenNumber Specifies the screen number of the display device.
window                   Window ID of the created window 
cmap                     The default color-map ID.

 * /usr/lpp/X11/include/X11/X.h has tons of definitions in it, among which are
 * these....
 * 
 * Display classes  used in opening the connection
 * Note that the statically allocated ones are even numbered and the
 * dynamically changeable ones are odd numbered

#define StaticGray              0
#define GrayScale               1
#define StaticColor             2
#define PseudoColor             3
#define TrueColor               4
#define DirectColor             5



             END OF OF WINDOW STUFF             */


static binding _visual_classes[] = {
        { StaticGray,  "StaticGray"  },
        { GrayScale,   "GrayScale"   },
        { StaticColor, "StaticColor" },
        { PseudoColor, "PseudoColor" },
        { TrueColor,   "TrueColor"   },
        { DirectColor, "DirectColor" },
        { 0, 0 }};


void x11textwin_(int ixm, int iym,char *title_text)

   {
   isodisplay =XOpenDisplay(NULL);
   winRoot    =DefaultRootWindow(isodisplay);
   nScreen    =DefaultScreen    (isodisplay); 
   wBorder    =XBlackPixel      (isodisplay,nScreen);
   wBackground=XBlackPixel      (isodisplay,nScreen);    
   last_font_id=-1;
   cmap       =DefaultColormap  (isodisplay,nScreen);
   TextWindow=XCreateSimpleWindow(isodisplay,winRoot,0,0,ixm,iym,
                              BORDER_WIDTH,wBorder,wBackground);
   gctext=XCreateGC(isodisplay,TextWindow,0L,&gctextv);
   XSetForeground(isodisplay,gctext,WhitePixel(isodisplay,nScreen));
   rectcolor.flags  = DoRed | DoGreen | DoBlue;
   XMapWindow(isodisplay,TextWindow);
   XStoreName(isodisplay,TextWindow,title_text);
   XSelectInput(isodisplay,winRoot,KeymapStateMask);
   XSelectInput(isodisplay,TextWindow,
        VisibilityChangeMask | ButtonPressMask    | FocusChangeMask
       | ButtonReleaseMask   | PointerMotionMask  | KeyPressMask       
       | ButtonReleaseMask   | PropertyChangeMask | StructureNotifyMask
       | ResizeRedirectMask  | ExposureMask       | KeymapStateMask  

                );
   }/* x11textwin */

void x11winope_(int WIDTH, int HEIGHT,int *maxcols,int *isocols, int *rc,
                double phiredf, double phigreenf, double phibluef,
                double   aredf, double   agreenf, double   abluef,
                double   predf, double   pgreenf, double   pbluef )

   {
   int  k;
   isolog =fopen("isox11.log" ,"w");
   isocmap=fopen("isox11.cmap","w"); 
   fprintf(isolog,"ISOX11.C: x11winope has been entered\n");
   *rc = -2;
   isodisplayname=XDisplayName(NULL);
/*   char *isodisplayname = NULL; */
   (void) fprintf(isolog,"ISOX11.C: x11winope about to connect to displayname %s\n",isodisplayname);
   (void)  printf(       "ISOX11.C: x11winope about to connect to displayname %s\n",isodisplayname);

       if((isodisplay=XOpenDisplay(isodisplayname)) == NULL)  /*Obtain default display */
         {
   fprintf(isolog,"ISOX11.C: x11winope: Failure in XopenDisplay:\n Width, Height,maxcols, isocols, result=%8d%8d%8d%8d%8d\n",WIDTH,HEIGHT,*maxcols,*isocols,*rc);
    printf(       "ISOX11.C: x11winope: Failure in XopenDisplay:\n Width, Height,maxcols, isocols, result=%8d%8d%8d%8d%8d\n",WIDTH,HEIGHT,*maxcols,*isocols,*rc);
          *rc=-1;
          *isocols=0;
          *maxcols=2;
           return;
         }
       *rc=0;
   fprintf(isolog,"ISOX11.C: x11winope: XopenDisplay:\n Width, Height,maxcols, isocols, result=%8d%8d%8d%8d%8d\n",WIDTH,HEIGHT,*maxcols,*isocols,*rc);
    printf(       "ISOX11.C: x11winope: XopenDisplay:\n Width, Height,maxcols, isocols, result=%8d%8d%8d%8d%8d\n",WIDTH,HEIGHT,*maxcols,*isocols,*rc);
   fprintf(isolog,"ISOX11.C: x11winope has a good window on display %20d\n",isodisplay);
    printf(       "ISOX11.C: x11winope has a good window on display %20d\n",isodisplay);
   mapping=0;
   if(WIDTH < 0)
               {
                mapping=-1;
                WIDTH=-WIDTH;
               }

                  phired  =phiredf;
                  phigreen=phigreenf;
                  phiblue =phibluef;

                    ared  =aredf;
                    agreen=agreenf;
                    ablue =abluef;

                    pred  =predf;
                    pgreen=pgreenf;
                    pblue =pbluef;

   rectcolor.flags  = DoRed | DoGreen | DoBlue;
   polypoints=(XPoint *) malloc(sizeof(XPoint)*4);
   shading=FillStippled;
   nScreen    =DefaultScreen  (isodisplay); 
   cmap       =DefaultColormap(isodisplay,nScreen);
   *maxcols   =XDisplayCells  (isodisplay,nScreen);
   if(*maxcols > 256) *maxcols=256;
   mapcols    =*maxcols;
/*
      Try to find a good visual, ie PseudoColor....
   visual_class=PseudoColor;
*/
   visual_class=TrueColor;
   visual_depth=8;
   tmpstatus=XMatchVisualInfo(isodisplay,nScreen,visual_depth,visual_class,&vismatchinfo);
   fprintf(isolog,"ISOX11.C: Match returned %8d\n", tmpstatus);
/*
   fprintf(isolog,"ISOX11.C: Visual ID is   %8d\n", XVisualIDFromVisual(visual));
   */

   winRoot    =DefaultRootWindow(isodisplay);
   wBorder    =XBlackPixel(isodisplay,nScreen);
   wBackground=XBlackPixel(isodisplay,nScreen);    
   window=XCreateSimpleWindow(isodisplay,winRoot,0,0,WIDTH+1,HEIGHT+1,
                              BORDER_WIDTH,wBorder,wBackground);
/*
 *      Window attribute stuff - begin
 */
   if (!XGetWindowAttributes(isodisplay, winRoot, &win_attributes))
    printf("ISOX11.C: Can't get window attributes.");
   vistemplate.visualid = XVisualIDFromVisual(win_attributes.visual);
   vinfo=XGetVisualInfo(isodisplay, VisualIDMask, &vistemplate, &junk);
   mycol=vinfo->class;
   fprintf(isolog,"ISOX11.C:    isodisplay: %20d\n", isodisplay);
   fprintf(isolog,"ISOX11.C:  Visual Class: %8d\n", mycol);
   fprintf(isolog,"ISOX11.C:  Visual Class: %s\n", Lookup(mycol, _visual_classes));
   fprintf(isolog,"ISOX11.C:  WhitePixel: %8d\n", WhitePixel(isodisplay,nScreen));
   fprintf(isolog,"ISOX11.C:  BlackPixel: %8d\n", BlackPixel(isodisplay,nScreen));
XTranslateCoordinates(isodisplay,window,winRoot,0,0
            ,&xouter,&youter,&ChildReturn);
    fprintf(isolog, "ISOX11.C: x=           %8d\n" ,win_attributes.x);
    fprintf(isolog, "ISOX11.C: y=           %8d\n" ,win_attributes.y);
    fprintf(isolog, "ISOX11.C: width=       %8d\n" ,win_attributes.width);
    fprintf(isolog, "ISOX11.C: height=      %8d\n" ,win_attributes.height);
    fprintf(isolog, "ISOX11.C: border_width=%8d\n" ,win_attributes.border_width);
    fprintf(isolog, "ISOX11.C: depth=       %8d\n" ,win_attributes.depth);
    fprintf(isolog, "ISOX11.C: Class       =%8d\n" ,vinfo->class       );
    fprintf(isolog, "ISOX11.C: Colormapsize=%8d\n" ,vinfo->colormap_size);
    fprintf(isolog, "ISOX11.C: bits_per_rgb=%8d\n" ,vinfo->bits_per_rgb);
    fprintf(isolog, "ISOX11.C: xouter=      %8d\n" ,xouter);
    fprintf(isolog, "ISOX11.C: youter=      %8d\n" ,youter);
    fprintf(isolog, "ISOX11.C: phired/green/blue %8.2f,%8.2f,%8.2f\n",phired,phigreen,phiblue);
    fprintf(isolog, "ISOX11.C:   ared/green/blue %8.2f,%8.2f,%8.2f\n",   ared,  agreen,  ablue);
    fprintf(isolog, "ISOX11.C:   pred/green/blue %8.2f,%8.2f,%8.2f\n",   pred,  pgreen,  pblue);
/*
      Don't steal private colour-map space for Cold-Hot mapping...
    if(mapping == -1)mycol=4;
*/
/*
 *      Window attribute stuff - end
 */
   XStoreName(isodisplay,window,"Design and copyright reserved John Watts 2012");
/*
      Create Graphics Contexts...
*/
   gcb   =XCreateGC(isodisplay,window,0L,&gcbv);
   gcw   =XCreateGC(isodisplay,window,0L,&gcwv);
   gc    =XCreateGC(isodisplay,window,0L,&gcv );
   gctext=XCreateGC(isodisplay,window,0L,&gctextv);
/*
      Next two lines vital to stop zillions of events telling you the screen
      hasn't been newly exposed.   IE, is just sitting there....
*/
   gcv.graphics_exposures = False;
   gcbv.graphics_exposures = False;
   gcwv.graphics_exposures = False;
   XSelectInput(isodisplay,window,
        VisibilityChangeMask | ButtonPressMask | ButtonReleaseMask  
      | StructureNotifyMask  | FocusChangeMask   
      | KeyPressMask         | KeyReleaseMask      | PointerMotionMask
/*    | ResizeRedirectMask   | PropertyChangeMask  | ExposureMask */      
      | ResizeRedirectMask   |                       ExposureMask         
               ); 
   XSetLineAttributes(isodisplay,gcb,0,LineSolid,CapButt,JoinBevel);
   XSetTSOrigin      (isodisplay,gcb,1,1);
   XSetFillStyle     (isodisplay,gcb,shading);
   XChangeGC         (isodisplay,gcb,GCGraphicsExposures,&gcbv);
   
   XSetLineAttributes(isodisplay,gcw,0,LineSolid,CapButt,JoinBevel);
   XSetTSOrigin      (isodisplay,gcw,1,1);
   XSetFillStyle     (isodisplay,gcw,shading);
   XChangeGC         (isodisplay,gcw,GCGraphicsExposures,&gcwv);
   
   XSetLineAttributes(isodisplay,gc,0,LineSolid,CapButt,JoinBevel);
   XSetTSOrigin      (isodisplay,gc,1,1);
   XSetFillStyle     (isodisplay,gc,shading);
   XChangeGC         (isodisplay,gc,GCGraphicsExposures,&gcv);
   
   XSetLineAttributes(isodisplay,gctext,0,LineSolid,CapButt,JoinBevel);
   XSetTSOrigin      (isodisplay,gctext,1,1);
   XSetFillStyle     (isodisplay,gctext,shading);
   XChangeGC         (isodisplay,gctext,GCGraphicsExposures,&gctextv);

   last_font_id=-1;

   XSetForeground(isodisplay,gctext,WhitePixel(isodisplay,nScreen));
   XSetForeground(isodisplay,gcw,   WhitePixel(isodisplay,nScreen));
   XSetForeground(isodisplay,gcb,   BlackPixel(isodisplay,nScreen));
/* */
   XDefineCursor(isodisplay,window,XCreateFontCursor(isodisplay,XC_crosshair));
   fprintf(isolog,"ISOX11.C: Mapping isodisplay: %20d\n", isodisplay);
   XMapWindow(isodisplay,window);
   fprintf(isolog,"ISOX11.C: Mapped window     : %20d\n", window);
   XFlush(isodisplay);
/*
   fprintf(isolog,"ISOX11.C:Foreground black/white/neutral: %10d %10d %10d %10d %10d \n",gcv.foreground,gcbv.foreground,gcwv.foreground,BlackPixel(isodisplay,nScreen),WhitePixel(isodisplay,nScreen));
*/
   if(mycol == 3)
               {
                 printf(        "ISOX11.C: Variable mapping - setting custom colours\n");
                fprintf(isocmap,"ISOX11.C: Variable mapping - setting custom colours\n");
                cmap1=XCreateColormap
                  (isodisplay,window,DefaultVisual(isodisplay,nScreen),AllocAll);
                *isocols=x11cmap_();
               }
          else
               {

                *maxcols=x11cmap_();

                *isocols=0;
                 printf(        "ISOX11.C: Best-fit mapping using %8d original colours\n",*maxcols);
                fprintf(isocmap,"ISOX11.C: Best-fit mapping using %8d original colours\n",*maxcols);
               }
/*
   Create and CLEAR the pixmap (it hangs around in memory on some servers...)
*/
   PicturePixmap=XCreatePixmap(isodisplay,window,win_attributes.width
                                             ,win_attributes.height
                                             ,win_attributes.depth
                               );
   XFillRectangle(isodisplay,PicturePixmap,gcb,0,0,win_attributes.width,win_attributes.height);
/* 
*/
   *rc=0;
   (void) fprintf(isolog,"ISOX11.C: Width, Height,maxcols, isocols, result=%8d%8d%8d%8d%8d\n"
                                             ,win_attributes.width
                                             ,win_attributes.height
		   ,*maxcols,*isocols,*rc);
   (void)  printf(       "ISOX11.C: Width, Height,maxcols, isocols, result=%8d%8d%8d%8d%8d\n"
                                             ,win_attributes.width
                                             ,win_attributes.height
		   ,*maxcols,*isocols,*rc);
   return;
   }/* x11winope */

int x11cmap_()
   {
     /* create shades of colour scale (from heat2D ex Maui)*/
      int     k,j,kcell[256];


   fprintf(isocmap,"ISOX11.C: Colour-mapping with mycol=%8d and mapcols=%8d\n",mycol,mapcols);
    if(mycol==3)
     {
/*
      Copy the original colour map (in cmap) to the new colour map 
      (in cmap1)
*/
      for (k=0;k<mapcols;k++)  
        {
           rectcolor.pixel=k;
           XQueryColor(isodisplay, cmap,  &rectcolor);
           ncell[k]=rectcolor.pixel;
           XStoreColor(isodisplay, cmap1, &rectcolor);
        }
      }
/*
      Try to find out how many colours are free, for a perfectly smooth
      new colour map.   In the meantime, try to use the base colour map
      for a smooth gradation of colours
*/
      newcols=1;
      map_calc_cols   =x11mapcalc_(mapcols-2);
  fprintf(isocmap,"     RED   GREEN    BLUE       K      RX   PIXEL NEWCOLS   RCRED RCGREEN  RCBLUE NCELL[NEWCOLS]\n");
      for (k=0;k<mapcols;k++)  
        {
           rectcolor.red  =nred  [k];
           rectcolor.green=ngreen[k];
           rectcolor.blue =nblue [k];
           rectcolor.flags  = DoRed | DoGreen | DoBlue;
           rx=XAllocColor (isodisplay, cmap, &rectcolor);
           if(rx)
                 {
                  if(
                     rectcolor.pixel!=BlackPixel(isodisplay,nScreen) &&
                     rectcolor.pixel!=WhitePixel(isodisplay,nScreen)
                    )
                     {
                      ncell [newcols]=rectcolor.pixel;
  fprintf(isocmap,"%8d%8d%8d%8d%8d%8d%8d%8d%8d%8d%10d \n",nred[k],ngreen[k],nblue[k],k,rx,rectcolor.pixel,newcols,rectcolor.red,rectcolor.green,rectcolor.blue,ncell[newcols]);
                      newcols++;
                     }
                 else
                     {
                      if(rectcolor.pixel=BlackPixel(isodisplay,nScreen))fprintf(isocmap,"ISOX11.C: Black in cell  %8d \n",k);
                      if(rectcolor.pixel=WhitePixel(isodisplay,nScreen))fprintf(isocmap,"ISOX11.C: White in cell  %8d \n",k);
                     }
                 }
             else
                 fprintf(isocmap,"ISOX11.C: No access to color cell %8d \n",k);
        } 
         XFlush(isodisplay);

/* Don't try clever stuff on fixed colour servers */

   fprintf(isocmap,"ISOX11.C: Colour-maxxing with mycol=%8d and mapcols=%8d\n",mycol,mapcols);
         if(mycol != 3)
                     {
                      if(newcols == 1)   /*...ie, no cells found....*/
                        {
                         newcols=map_calc_cols;
                        }
                      return(newcols);  /* Now again returns newcols... */
                     }

      map_calc_cols=x11mapcalc_(newcols);

      rectcolor.flags  = DoRed | DoGreen | DoBlue;
      for (k=1;k<1+newcols;k++)  
        {
           rectcolor.red  =nred  [k];
           rectcolor.green=ngreen[k];
           rectcolor.blue =nblue [k];
           rectcolor.pixel=ncell [k];
           XStoreColor(isodisplay, cmap1, &rectcolor);
        }
      rx=XUninstallColormap(isodisplay,cmap);

      if(rx)fprintf(isocmap,"ISOX11.C: Original colour map has been uninstalled\n");
       else fprintf(isocmap,"ISOX11.C: Original colour map has NOT been uninstalled\n");

       rx=XInstallColormap(isodisplay,cmap1);
       if(rx)fprintf(isocmap,"ISOX11.C: New colour map has been installed\n");
        else fprintf(isocmap,"ISOX11.C: New colour map has NOT been installed\n");

/* The next line ensures that the swirl works */

       XSetWindowColormap(isodisplay,window,cmap1);

         XFlush(isodisplay);
         return(newcols);   

   }/* x11cmap */

void x11kaleido_(int delta)
      {
       int k,l,m;
 
         if(mycol!=3)return;/* Don't try clever stuff on fixed colour servers*/

         rectcolor.flags  = DoRed | DoGreen | DoBlue;
         for (k=0;k<newcols;k++)  
           {
              l=(k+delta)%newcols;
              m=k-delta;
              if(m<0)m=newcols+m;
              rectcolor.red  =nred  [l];
              rectcolor.green=ngreen[k];
              rectcolor.blue =nblue [m];
              rectcolor.pixel=ncell [k];
              XStoreColor(isodisplay,cmap1,&rectcolor);  
           }
   XFlush(isodisplay);
      }/* x11kaleido */


void x11config_(Window idmover)
   {

   XWindowChanges     windelta;
   int                h;

/*
      windelta.x=x;
      windelta.y=y;
      windelta.height=h;
      windelta.width=w;
      windelta.border_width=bw;
      windelta.stack_mode=Above;
      fprintf(isolog,"X      %d \n",windelta.x);
      fprintf(isolog,"Y      %d \n",windelta.y);
      fprintf(isolog,"Width  %d \n",windelta.width);
      fprintf(isolog,"Height %d \n",windelta.height);
      fprintf(isolog,"Border %d \n",windelta.border_width);
      fprintf(isolog,"Above  %d \n",windelta.stack_mode);
*/
     i=XGetWindowAttributes (isodisplay,idmover,&win_attributes);
      h=win_attributes.height;
      windelta.height=h+1;
      XConfigureWindow(isodisplay,idmover,CWHeight,&windelta);
      windelta.height=h;
      XConfigureWindow(isodisplay,idmover,CWHeight,&windelta);
      XFlush(isodisplay);
   } /* x11windowmove */

void x11sendevent_(Window idmover,int x, int y, int h, int w, int bw)
   {
      xev.type=ConfigureNotify;
      xev.xconfigure.window=idmover;
      xev.xconfigure.x=x;
      xev.xconfigure.y=y;
      xev.xconfigure.height=h;
      xev.xconfigure.width=w;
      xev.xconfigure.border_width=bw;
      xev.xconfigure.above=False;
      xev.xconfigure.override_redirect=False;

      fprintf(isolog,"ISOX11.C: True is %d, False is %d \n",True,False);
      fprintf(isolog,"ISOX11.C: Event  %d \n",xev.xconfigure.type);
      fprintf(isolog,"ISOX11.C: Window %d \n",xev.xconfigure.window);
      fprintf(isolog,"ISOX11.C: X      %d \n",xev.xconfigure.x);
      fprintf(isolog,"ISOX11.C: Y      %d \n",xev.xconfigure.y);
      fprintf(isolog,"ISOX11.C: Width  %d \n",xev.xconfigure.width);
      fprintf(isolog,"ISOX11.C: Height %d \n",xev.xconfigure.height);
      fprintf(isolog,"ISOX11.C: Border %d \n",xev.xconfigure.border_width);
      fprintf(isolog,"ISOX11.C: Above  %d \n",xev.xconfigure.above);
      fprintf(isolog,"ISOX11.C: O_ride %d \n",xev.xconfigure.override_redirect);

      if(XSendEvent(isodisplay,InputFocus,False,StructureNotifyMask,&xev))
        fprintf(isolog,"ISOX11.C: Result of XSendEvent is OK \n \n");
      else
        fprintf(isolog,"ISOX11.C: XSendevent failed \n \n"); 
        XFlush(isodisplay);
   } /* x11windowmove */

void x11peeker_()
   {
      fprintf(isolog,"ISOX11.C: Event: %d %d\n",XPeekEvent(isodisplay,&xev),xev.type);
/*
      XFlush(isodisplay);
*/
   } /* x11windowmove */

void x11windowmove_(Window idmover,int x, int y, int h, int w, int bw)
   {
/*
      fprintf(isolog,"ISOX11.C: Old window was at %d %d \n",x_old,y_old);
      fprintf(isolog,"ISOX11.C: New window  is at %d %d \n",x_new,y_new);
*/
      XMoveWindow(isodisplay,idmover,x,y);
      XFlush(isodisplay);
   } /* x11windowmove */

void x11swirlup_(int delta)
      {
       int k,l;
 
         if(mycol!=3)return;/* Don't try clever stuff on fixed colour servers*/

         rectcolor.flags  = DoRed | DoGreen | DoBlue;
         
         for (k=0;k<newcols;k++)  
           {
              rectcolor.red  =nred  [k];
              rectcolor.green=ngreen[k];
              rectcolor.blue =nblue [k];
              rectcolor.pixel=ncell [(k+delta)%newcols];
              XStoreColor(isodisplay, cmap1, &rectcolor);  
           }
   XFlush(isodisplay);
      }/* x11swirlup */


int x11mapcalc_(int navail)
   {
/* create shades of colour scale (from heat2D ex Maui)*/
      int     k,k1,k2,j,nc,ng,ncycles,nc2;
      double  a,b,c,r,rr,pr,pg,pb;

      if(mycol != 3)
         {
           if(mapcols < 256)
                            navail=mapcols-4;
           else
                            navail=254;
         }

      r=navail;
     rr=navail;

 if(mapping == 0)
     {
      r   =M_PI/r;
      pr  =pred  *r;
      pg  =pgreen*r;
      pb  =pblue *r;
      for (k=0;k<navail;k++)
        {
           c=k;
           nred  [k]=colcalc(ared  ,pr,c,phired  );
           ngreen[k]=colcalc(agreen,pg,c,phigreen);
           nblue [k]=colcalc(ablue ,pb,c,phiblue );
        }
      }

 if(mapping == -1)
     {
      k2=0;
/*
      Prepare the colour map in quarters blue-cyan-green-yellow-red
*/

      k1=k2;
      k2=(4*navail)/16;

      a=k2-k1;
      rr=1./a;
      c=0;

      for (k=k1;k<k2;k++)
        {
           nred  [k]=0;
           ngreen[k]=(int)(65535.*sqrt(c*rr));
           nblue [k]=65535;
           c=c+1.;
        }

      k1=k2;
      k2=navail/2;

      a=k2-k1;
      rr=1./a;
      c=0;

      for (k=k1;k<k2;k++)
        {
           nred  [k]=0;
           ngreen[k]=65535;
           nblue [k]=(int)(65535.*(sqrt(1.-(c*rr))));
           c=c+1.;
        }

      k1=k2;
      k2=(12*navail)/16;

      a=k2-k1;
      rr=1./a;
      c=0;

      for (k=k1;k<k2;k++)
        {
           nred  [k]=(int)(65535.*sqrt(c*rr));
           ngreen[k]=65535;
           nblue [k]=0;
           c=c+1.;
        }

      k1=k2;
      k2=navail;

      a=k2-k1;
      rr=1./a;
      c=0;

      for (k=k1;k<k2;k++)
        {
           nred  [k]=65535;
           ngreen[k]=(int)(65535.*(sqrt(1.-(c*rr))));
           nblue [k]=0;
           c=c+1.;
        }
     }

        return(navail);
   }/* x11mapcalc_ */



int colcalc(double base, double pc, double c, double phi)
   {
      int    ncol;
      double sin2,amp;
      amp=1.-base;
/*
           For twice the colours (monotonic map)
*/
      c=c/2.0;
      sin2=sin(pc*c+phi);
      ncol=(int)(65535.*((amp*sin2*sin2)+base));
      return(ncol);
   } 

void x11title_(char *title)
   {
   XStoreName(isodisplay,window,title);
   XFlush(isodisplay);
   }/* x11title */

void x11clearpixmap_()
   {
      XFillRectangle(isodisplay,PicturePixmap,gcb,0,0,1+win_attributes.width,1+win_attributes.height);
   } /* x11clearpixmap */
 
void x11clear_()
   {
      XClearWindow(isodisplay,window);
   }/* Clear window */

void x11setfont_(int font_id)
   {
    char *font_def;
    Font x_font_id;

                    font_def="8x13\0"; /* Fail-safe.... */
    if(font_id == 0)font_def="8x13\0";
    if(font_id == 1)font_def="6x12\0";
    if(font_id == 2)font_def="6x10\0";
    if(font_id == 3)font_def="6x9\0";
    if(font_id == 4)font_def="5x8\0";
    if(font_id == 5)font_def="5x7\0";

    x_font_id=XLoadFont(isodisplay, font_def);
    XSetFont(isodisplay, gctext,  x_font_id);
   }/* x11setfont */

void x11textw_(int xstart,int ystart,char *disp_text, int font_id,int nchars)
   {
    int nitems = 1;
    if(! last_font_id == font_id)
      {
       x11setfont_(font_id);
       last_font_id=font_id;
      }
    XTextItem items[] = {
                         {disp_text,nchars,0,None}
                        }; 
    XDrawText(isodisplay, TextWindow, gctext, xstart, ystart, items, nitems);
   }/* x11text */

void x11text_(int xstart,int ystart,char *disp_text,int font_id,int nchars)
   {
    int nitems = 1;
    if(! last_font_id == font_id)
      {
       x11setfont_(font_id);
       last_font_id=font_id;
      }
    XTextItem items[] = {
                         {disp_text,nchars,0,None}
                        }; 
    XDrawText(isodisplay, PicturePixmap, gctext, xstart, ystart, items, nitems);
   }/* x11text */

int x11colxlate_(int mcol, int *xcol)
   {
    *xcol=ncell[mcol];
   }/* x11colxlate */

void x11qev_()
   {
      fprintf(isolog,"ISOX11.C: Number of events received from X-server, but not removed from the event queue %d \n", XPending(isodisplay));
      fprintf(isolog,"ISOX11.C: Number of events already in the event queue %d \n", XEventsQueued(isodisplay,QueuedAlready));
   }/* x11qev */ 

void x11blok_button_(int *nbut,int *mousex,int *mousey)

   {
      Drawable       drawable;
      int            x,y,border,depth,width,height,mx,isox,rcx;
  
        XMaskEvent(isodisplay,ButtonReleaseMask,&xev);
                     
       *nbut=xev.xbutton.button;
       *mousex=xev.xbutton.x;
       *mousey=xev.xbutton.y;
       
                     
   } /* x11blok_button */
 
void x11spotbutton_(int *nbut,int *mousex,int *mousey)

   {
      Drawable       drawable;
  
         *nbut=-999;
         if(XCheckMaskEvent(isodisplay,ExposureMask,&xev))
                    {
                     *nbut=-10; 
                     return;
                    }
         if(XCheckMaskEvent(isodisplay,PointerMotionMask,&xev))
                    {
                     *nbut=-994;	/* Mouse has moved */
                     *mousex=xev.xbutton.x;
                     *mousey=xev.xbutton.y;
                     return;
                    }
  
         if(XCheckMaskEvent(isodisplay,ButtonPressMask,&xev))
                    {
                     *nbut=-xev.xbutton.button;
                     *mousex=xev.xbutton.x;
                     *mousey=xev.xbutton.y;
                     return;
                    }
       
         if(XCheckMaskEvent(isodisplay,ButtonReleaseMask,&xev))
                    {
                     *nbut=xev.xbutton.button;
                     *mousex=xev.xbutton.x;
                     *mousey=xev.xbutton.y;
                     return;
                    }

         if(XCheckMaskEvent(isodisplay,KeyPressMask,&xev))
                    {
                     switch (xev.type)
                      {
                        case KeyPress:
                           {
                            XLookupString(&xev.xkey,buffer,MAXLINE,&myKeysym,&myStatus);
                            *nbut=xev.xkey.keycode;
                            break;
                           }
                      }
                      return;
                    }

  
   } /* x11spotbutton */
 
void x11mouse_(int *nbut,int *mousex,int *mousey,int *newx,int *newy)

/* Blocking..... 
         ExposureMask      |VisibilityChangeMask| ButtonPressMask
       | ButtonReleaseMask | PropertyChangeMask | StructureNotifyMask 
       | ResizeRedirectMask|KeyPressMask        | PointerMotionMask
       | FocusChangeMask   | KeymapStateMask
*/
     
   {
      Drawable       drawable;
      int            x,y,border,depth,width,height,mx,isox,rcx;
      char           key_pressed;
  
        *nbut=-999;
        event_msg=True;

         XNextEvent(isodisplay,&xev);
/*
        XCheckMaskEvent(isodisplay,StructureNotifyMask,&xev);
if(event_msg)fprintf(isolog,"ISOX11.C: Next event is %d \n",xev.type);
*/
         switch (xev.type)
          {

            case ConfigureNotify:
               {
if(event_msg)fprintf(isolog,"ISOX11.C: Next event from x11mouse is ConfigureNotify %d \n",xev.type);

                 *nbut=-12;
                 *mousex=xev.xconfigure.x;
                 *mousey=xev.xconfigure.y;
             /*  *newx=xev.xconfigure.width -xev.xconfigure.border_width;
                 *newy=xev.xconfigure.height-xev.xconfigure.border_width; */
                 *newx=xev.xconfigure.width;
                 *newy=xev.xconfigure.height;
		 JSWindow=xev.xconfigure.window;
		 if(event_msg)fprintf(isolog,"Changed window is %d\n",JSWindow);
/* For PJDM */   XMapWindow(isodisplay,window);
                 break;
               }

            case PropertyNotify:
               {
if(event_msg)fprintf(isolog,"ISOX11.C: Next event from x11mouse is PropertyNotify %d \n",xev.type);
                 *nbut=-9;
		 /*
		 *mousex=-900;
		 *mousey=-900;
		 */
                 break;
               }
	       

            case VisibilityNotify:
               {
if(event_msg)fprintf(isolog,"ISOX11.C: Next event from x11mouse is VisibilityNotify %d \n",xev.type);
                 *nbut=-8;
		 *mousex=-800;
		 *mousey=-800;
                 break;
               }

            case FocusOut:
               {
if(event_msg)fprintf(isolog,"ISOX11.C: Next event from x11mouse is FocusOut %d \n",xev.type);
                 *nbut=-7;
		 *mousex=-700;
		 *mousey=-700;
                 break;
               }

            case FocusIn:
               {
if(event_msg)fprintf(isolog,"ISOX11.C: Next event from x11mouse is FocusIn %d \n",xev.type);
                 *nbut=-6;
		 *mousex=-600;
		 *mousey=-600;
                 break;
               }

            case KeymapNotify:
               {
if(event_msg)fprintf(isolog,"ISOX11.C: Next event from x11mouse is KeymapNotify %d \n",xev.type);
                 *nbut=-5;
		 *mousex=-500;
		 *mousey=-500;
                 break;
               }

            case KeyPress:
               {
if(event_msg)fprintf(isolog,"ISOX11.C: Next event from x11mouse is KeyPress %d \n",xev.type);
XLookupString(&xev.xkey,buffer,MAXLINE,&myKeysym,&myStatus);
#include "keydefs.h"
                 *nbut=xev.xkey.keycode;
if(event_msg)fprintf(isolog,"ISOX11.C: Key %d pressed in x11mouse..!\n",*nbut);
                 *mousex=xev.xkey.x;;
                 *mousey=xev.xkey.y;
                 *newx=0;
                 *newy=0;
                 break;
               }

            case KeyRelease:
               {
if(event_msg)fprintf(isolog,"ISOX11.C: Next event from x11mouse is KeyRelease %d \n",xev.type);
XLookupString(&xev.xkey,buffer,MAXLINE,&myKeysym,&myStatus);
#include "keydefs.h"
                 *nbut=xev.xkey.keycode;
if(event_msg)fprintf(isolog,"ISOX11.C: Key %d released in x11mouse..!\n",*nbut);
                 *mousex=xev.xkey.x;;
                 *mousey=xev.xkey.y;
                 *newx=-1;
                 *newy=-1;
                 break;
               }

            case MotionNotify:
               {
/*
if(event_msg)fprintf(isolog,"ISOX11.C: Next event from x11mouse is MotionNotify %d \n",xev.type);
*/
                *nbut=-994;
                *mousex=xev.xbutton.x;
                *mousey=xev.xbutton.y;
                 break;
                } 
 

            case ResizeRequest:
               {
if(event_msg)fprintf(isolog,"ISOX11.C: Next event from x11mouse is ResizeRequest %d \n",xev.type);
                  *nbut=-11;
                  *newx=xev.xresizerequest.width;
                  *newy=xev.xresizerequest.height;
                  width=*newx;
                  height=*newy;
                  break;
               }


            case Expose:
               {
if(event_msg)fprintf(isolog,"ISOX11.C: Next event from x11mouse is Expose %d \n",xev.type);
                  *nbut=-10; /* Window exposed*/
                     *mousex=xev.xexpose.x;
                     *mousey=xev.xexpose.y;
                     *newx=xev.xexpose.width;
                     *newy=xev.xexpose.height;
                  break;
               }
 
            case ButtonRelease:
               {
if(event_msg)fprintf(isolog,"ISOX11.C: Next event from x11mouse is ButtonRelease %d \n",xev.type);
                     *nbut=xev.xbutton.button;
                     *mousex=xev.xbutton.x;
                     *mousey=xev.xbutton.y;
                     break;
               }
 
            case ButtonPress:
               {
if(event_msg)fprintf(isolog,"ISOX11.C: Next event from x11mouse is ButtonPress %d \n",xev.type);
                     *nbut=-xev.xbutton.button;
                     *mousex=xev.xbutton.x;
                     *mousey=xev.xbutton.y;
                     break;
               }

           }/* switch type */
       

    }/* x11mouse */

void x11close_()
   {
   (void)  fprintf(isolog,"ISOX11.C: x11winope about to close display %20d\n",isodisplay);
                  if(mycol == 3)XFreeColormap(isodisplay,cmap1);
                  XFreePixmap(isodisplay,PicturePixmap);
                  XFlush(isodisplay);
                  XCloseDisplay(isodisplay);
                  isodisplay=NULL;
   }/* x11close */

void x11rectgc_(int x1, int y1a, int x2, int y2, int ncol)

   {     
/*
  fprintf(isolog,"ISOX11.C: X11rectgc: Colour %8d at %8d %8d %8d %8d \n",ncol,x1,y1a,x2,y2);
*/
      XSetForeground(isodisplay,gc,ncol);
      XFillRectangle(isodisplay,PicturePixmap,gc,x1,y1a,(x2-x1+1),(y2-y1a+1));
   }/* x11rectgc */      

void x11rectfi_(int x1, int y1a, int x2, int y2, int ncol)

   {     
/*
  fprintf(isolog,"ISOX11.C: X11rectfi: Colour %8d at %8d %8d %8d %8d \n",ncol,x1,y1a,x2,y2);
*/
      XSetForeground(isodisplay,gc,ncell[ncol]);
      XFillRectangle(isodisplay,PicturePixmap,gc,x1,y1a,(x2-x1+1),(y2-y1a+1));
   }/* x11rectfi */      


void x11resize_(int wx, int wy)
   {
      XResizeWindow(isodisplay,window,wx,wy);
   } /* x11resize */

void x11whiteline_(int x1, int y1, int x2, int y2)
   {
      XDrawLine(isodisplay,PicturePixmap,gcw,x1,y1,x2,y2);
   } /* x11whiteline */

void x11blackline_(int x1, int y1, int x2, int y2)
   {
      XDrawLine(isodisplay,PicturePixmap,gcb,x1,y1,x2,y2);
   } /* x11blackline */

void x11ruler_(int x1, int y1, int y2, int y3, int y4, int ndx1, int ndx2)

/*
      Sets the drawing function to 'invert', then draws an inverting line
      Drawing function is then reset to 'copy'
*/
   {     

/*
fprintf(isolog,"ISOX11.C: X11RULER: Parameters are %8d,%8d,%8d,%8d,%8d,%8d,%8d\n",x1,y1,y2,y3,y4,ndx1,ndx2);
*/

      XSetFunction(isodisplay,gcw,GXinvert);
      XDrawLine(isodisplay,window,gcw,x1,y1,x1,y3);
      XDrawLine(isodisplay,window,gcw,x1,y2,x1,y4);
/*
          Draw extra lines between y3 and y4
*/
      XDrawLine(isodisplay,window,gcw,ndx1,y4-1,ndx2,y3+1);
      XDrawLine(isodisplay,window,gcw,ndx1,y3+1,ndx2,y4-1);

      XSetFunction(isodisplay,gcw,GXcopy);
      XFlush(isodisplay);
    }/* x11ruler */

void x11box_(int x1, int y1a, int x2, int y2)

   {     
      wx=x2-x1-2;
      wy=y2-y1a-2;
      x1=x1+1;
      y1a=y1a+1;
      XSetFunction(isodisplay,gcw,GXinvert);
      XDrawRectangle(isodisplay,window,gcw,x1,y1a,wx,wy);
      XSetFunction(isodisplay,gcw,GXcopy);
      XFlush(isodisplay);
    }/* x11box */

void x11invertzone_(int x1, int y1, int x2, int y2)
/*
      Almost identical to x11bound....
*/

   {     
      XSetFunction(isodisplay,gcw,GXandReverse);
      wx=x2-x1+1;
      wy=y2-y1+1;
      XFillRectangle(isodisplay,window,gcw,x1,y1,wx,wy);
      XSetFunction(isodisplay,gcw,GXcopy);
      XFlush(isodisplay);
    }/* x11invertzone */

void x11bound_(int x1, int y1a, int x2, int y2)

   {     
      XSetFunction(isodisplay,gcw,GXinvert);
      wx=x2-x1-2;
      wy=y2-y1a-2;
      x1=x1+1;
      y1a=y1a+1;
      XFillRectangle(isodisplay,PicturePixmap,gcw,x1,y1a,wx,wy);
      XSetFunction(isodisplay,gcw,GXcopy);
      XDrawRectangle(isodisplay,PicturePixmap,gcw,x1,y1a,wx,wy);
      XFlush(isodisplay);
    }/* x11bound */

void x11drawbutton_(int up_down, int x1, int y1, int x2, int y2, int ncol)

   {     

      wx=x2-x1-1;
      wy=y2-y1-1;

      XSetForeground(isodisplay,gc,ncell[ncol]);
      XFillRectangle(isodisplay,PicturePixmap,gc,x1,y1,wx,wy); /* Internal colour */

      if(up_down == 0)
                     {

/*   Draw NW lines */

                      XDrawLine(isodisplay,PicturePixmap,gcb,x1  ,y1  ,x2  ,y1  );	/* North */
                      XDrawLine(isodisplay,PicturePixmap,gcb,x1  ,y1  ,x1  ,y2  );	/* West  */
                      XDrawLine(isodisplay,PicturePixmap,gcb,x1+1,y1+1,x2-1,y1+1);	/* North */
                      XDrawLine(isodisplay,PicturePixmap,gcb,x1+1,y1+1,x1+1,y2-1);	/* West  */

/*   Draw SE lines */

                      XDrawLine(isodisplay,PicturePixmap,gcw,x1  ,y2  ,x2  ,y2  );	/* South */
                      XDrawLine(isodisplay,PicturePixmap,gcw,x2  ,y1  ,x2  ,y2  );	/* East  */
                      XDrawLine(isodisplay,PicturePixmap,gcw,x1+1,y2-1,x2-1,y2-1);	/* South */
                      XDrawLine(isodisplay,PicturePixmap,gcw,x2-1,y1+1,x2-1,y2+1);	/* East  */
                     }
         else
                     {

/*   Draw NW lines */

                      XDrawLine(isodisplay,PicturePixmap,gcw,x1  ,y1  ,x2  ,y1  );	/* North */
                      XDrawLine(isodisplay,PicturePixmap,gcw,x1  ,y1  ,x1  ,y2  );	/* West  */
                      XDrawLine(isodisplay,PicturePixmap,gcw,x1+1,y1+1,x2-1,y1+1);	/* North */
                      XDrawLine(isodisplay,PicturePixmap,gcw,x1+1,y1+1,x1+1,y2-1);	/* West  */

/*   Draw SE lines */

                      XDrawLine(isodisplay,PicturePixmap,gcb,x1  ,y2  ,x2  ,y2  );	/* South */
                      XDrawLine(isodisplay,PicturePixmap,gcb,x2  ,y1  ,x2  ,y2  );	/* East  */
                      XDrawLine(isodisplay,PicturePixmap,gcb,x1+1,y2-1,x2-1,y2-1);	/* South */
                      XDrawLine(isodisplay,PicturePixmap,gcb,x2-1,y1+1,x2-1,y2+1);	/* East  */
                     }
/*
      XSetForeground(isodisplay,gc,BlackPixel(isodisplay,nScreen));
*/

   }/* x11drawbutton */      

void x11work_(int x1, int y1a, int x2, int y2, int ncol)

   {     
      wx=x2-x1-2;
      wy=y2-y1a-2;
      x1=x1+1;
      y1a=y1a+1;
      XSetForeground(isodisplay,gc,WhitePixel(isodisplay,nScreen));
      XDrawRectangle(isodisplay,PicturePixmap,gc,x1,y1a,wx,wy);
      x1=x1+1;
      y1a=y1a+1;
      wx=wx-1;
      wy=wy-1;
      XSetForeground(isodisplay,gc,ncell[ncol]);
      XFillRectangle(isodisplay,PicturePixmap,gc,x1,y1a,wx,wy);
      XFlush(isodisplay);
   }/* x11work */      

void x11bisect_(int x1, int y1a, int x2, int y2, int ncol)
   {
      XSetForeground(isodisplay,gc,ncol);
      XDrawLine(isodisplay,PicturePixmap,gc,x1,y1a,x2,y2);
   }/* x11bisect */

void x11movezone_(int ix, int iy, int ix2, int iy2, int ix3, int iy3)

/*
      Moves a block of the picture around.  Does NOT clear abandoned space...
*/
   {
      int iwidth, iheight;

      iwidth =ix2-ix+1;
      iheight=iy2-iy+1;
      XCopyArea(isodisplay,PicturePixmap,PicturePixmap,gc,ix ,iy ,iwidth,iheight,ix3,iy3);
      XCopyArea(isodisplay,PicturePixmap,window       ,gc,ix3,iy3,iwidth,iheight,ix3,iy3);
   }/* x11movezone */

void x11updatezone_(int ix, int iy, int ix2, int iy2)
   {
      int iwidth, iheight;

      iwidth =ix2-ix+1;
      iheight=iy2-iy+1;
/*
fprintf(isolog,"X11UPDATE_ZONE: Pixmap/window top left is %8d,%8d, width is %8d, height is %8d\n",ix,iy,iwidth,iheight);
*/
      XCopyArea(isodisplay,PicturePixmap,window,gc,ix,iy,iwidth,iheight,ix,iy);
   }/* x11updatezone */


void x11flush_()
   {

      XCopyArea(isodisplay,PicturePixmap,window,gc,0,0,win_attributes.width,win_attributes.height,0,0);
      /*
fprintf(isolog,"X11FLUSH: Pixmap/window width is %8d, height is %8d\n",win_attributes.width,win_attributes.height);
      */
      XFlush(isodisplay);
   }/* x11flush */

void x11line_(int x1, int y1a, int x2, int y2, int ncol)
   {
      XSetForeground(isodisplay,gc,ncell[ncol]);
      XDrawLine(isodisplay,PicturePixmap,gc,x1,y1a,x2,y2);
   }/* x11line */

void x11pixel_(int ix, int iy, int ncol)
   {
      XSetForeground(isodisplay,gc,ncell[ncol]);
      XDrawPoint(isodisplay,PicturePixmap,gc,ix,iy);
   }/* x11pixel */
      

void x11quad_(int x1, int y1, int x2, int y2,
             int x3, int y3, int x4, int y4, int ncol)
   {  
      polypoints[0].x=x1;
      polypoints[0].y=y1;
      polypoints[1].x=x2;
      polypoints[1].y=y2;
      polypoints[2].x=x3;
      polypoints[2].y=y3;
      polypoints[3].x=x4;
      polypoints[3].y=y4;

      XSetForeground(isodisplay,gc,ncell[ncol]);
      XFillPolygon(isodisplay,PicturePixmap,gc,polypoints,4,Convex,CoordModeOrigin);
   }/* x11quad */

int x11winsize_(int *winx1,int *winy1,int *winwidth,int *winheight)
   {
       unsigned int bordwidth,depth,ww,wh,bw;
                int wx1,wy1;

/*
       XGetGeometry(isodisplay,window,&RootReturn,&wx1,&wy1,&ww,&wh,&bordwidth,&depth);
fprintf(isolog,"X11WINSIZE: Window spec is %8d %8d %8d %8d %8d %8d \n",ww,wh,wx1,wy1,bordwidth,depth);
*/

       i=XGetWindowAttributes (isodisplay,window,&win_attributes);
       *winwidth =win_attributes.width +12;
       *winheight=win_attributes.height+26;
       i=XTranslateCoordinates(isodisplay,window,win_attributes.root
                                     ,-6
                                     ,-22
                                     ,&*winx1,&*winy1,&ChildReturn);

fprintf(isolog,"XGetAttributes: X=%4d, %4d, Y=%4d, %4d, Width=%4d, Height=%4d, Border_width=%2d, Depth=%2d \n"
                 ,win_attributes.x,*winx1
                 ,win_attributes.y,*winy1
                 ,win_attributes.width
                 ,win_attributes.height
                 ,win_attributes.border_width
                 ,win_attributes.depth
           );

   } /* X11winsize */

int x11putimage_(int imgx1,int imgy1,int winx1,int winy1,int imgwidth,int imgheight)
   {
      XFlush(isodisplay);
/*
     XSync(isodisplay,True);
   fprintf(isolog," imgx1=%4d, imgy1=%4d, winx1=%4d, winy1=%4d,imgwidth=%4d, imgheight=%4d\n",
            imgx1    , imgy1    , winx1    , winy1    ,imgwidth     ,imgheight);
*/
     XPutImage(isodisplay,window,gc,isoimage,imgx1,imgy1,winx1,winy1,imgwidth,imgheight);
      XFlush(isodisplay);
/*
     XSync(isodisplay,True);
*/
   } /* x11putimage */

void x11sync_()
   {
     XSync(isodisplay,True);
   } /* x11sync */
void x11getimage_(int xgx1,int xgy1,int xgwidth,int xgheight)
   {
     fprintf(isolog,"ISOX11.C: Securing image from %8d %8d %8d %8d\n",xgx1,xgy1,xgwidth,xgheight);
     isoimage=XGetImage(isodisplay,PicturePixmap,xgx1,xgy1,xgwidth,xgheight,AllPlanes,ZPixmap);
     fprintf(isolog,"ISOX11.C:  Image secured from %8d %8d %8d %8d\n",xgx1,xgy1,xgwidth,xgheight);
   } /* x11getimage */

void x11getpel_(int *npel, int x1, int y1)
   {
/*
     fprintf(isolog,"ISOX11.C: X11GetPel: Getting pixel from %8d %8d\n",x1,y1);
*/
      *npel=XGetPixel(isoimage,x1,y1);
/*
     fprintf(isolog,"ISOX11.C: X11GetPel: Pixel %8d secured from %8d %8d\n",*npel,x1,y1);
*/

   } /* x11getpel */

void x11dispinfo_(int *maxwidth, int *maxheight,int *White, int *Black, int *rc)
{
   if(isodisplay==NULL)
      {
   if((isodisplay=XOpenDisplay(NULL))==NULL) /* Obtain default isodisplay */
        {
	fprintf(isolog,"ISOX11.C: ERROR: unable to open isodisplay \n");
        *rc=-1;
	return;
        }
      }
   nScreen   = DefaultScreen(isodisplay); 
   *maxwidth = DisplayWidth (isodisplay,nScreen) ;
   *maxheight= DisplayHeight(isodisplay,nScreen) ;
   *White    =    WhitePixel(isodisplay,nScreen) ;
   *Black    =    BlackPixel(isodisplay,nScreen) ;
   *rc=0;

} /* x11_screen_info */

void x11qcmap_(int kpixel, int *kredx, int *kgreenx, int *kbluex)
   
/*
      Find out what colours are in this pixel...
*/
  {
                rectcolor.pixel=kpixel;
                if(mycol == 3)
                       {
                        XQueryColor(isodisplay, cmap1, &rectcolor);
                       }
                   else
                       {
                        XQueryColor(isodisplay, cmap, &rectcolor);
                       }
                *kredx  =rectcolor.red;
                *kgreenx=rectcolor.green;
                *kbluex =rectcolor.blue;

  }/* x11qcmap */
