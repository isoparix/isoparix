/*
 * Study for Xwindow events.
 */
#include <X11/Xlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//#define BACKGROUND_PAINT

void prtxevtt(int type)

{

    switch (type) {

        case 2:  fprintf(stderr, "KeyPress"); break;
        case 3:  fprintf(stderr, "KeyRelease"); break;
        case 4:  fprintf(stderr, "ButtonPress"); break;
        case 5:  fprintf(stderr, "ButtonRelease"); break;
        case 6:  fprintf(stderr, "MotionNotify"); break;
        case 7:  fprintf(stderr, "EnterNotify"); break;
        case 8:  fprintf(stderr, "LeaveNotify"); break;
        case 9:  fprintf(stderr, "FocusIn"); break;
        case 10: fprintf(stderr, "FocusOut"); break;
        case 11: fprintf(stderr, "KeymapNotify"); break;
        case 12: fprintf(stderr, "Expose"); break;
        case 13: fprintf(stderr, "GraphicsExpose"); break;
        case 14: fprintf(stderr, "NoExpose"); break;
        case 15: fprintf(stderr, "VisibilityNotify"); break;
        case 16: fprintf(stderr, "CreateNotify"); break;
        case 17: fprintf(stderr, "DestroyNotify"); break;
        case 18: fprintf(stderr, "UnmapNotify"); break;
        case 19: fprintf(stderr, "MapNotify"); break;
        case 20: fprintf(stderr, "MapRequest"); break;
        case 21: fprintf(stderr, "ReparentNotify"); break;
        case 22: fprintf(stderr, "ConfigureNotify"); break;
        case 23: fprintf(stderr, "ConfigureRequest"); break;
        case 24: fprintf(stderr, "GravityNotify"); break;
        case 25: fprintf(stderr, "ResizeRequest"); break;
        case 26: fprintf(stderr, "CirculateNotify"); break;
        case 27: fprintf(stderr, "CirculateRequest"); break;
        case 28: fprintf(stderr, "PropertyNotify"); break;
        case 29: fprintf(stderr, "SelectionClear"); break;
        case 30: fprintf(stderr, "SelectionRequest"); break;
        case 31: fprintf(stderr, "SelectionNotify"); break;
        case 32: fprintf(stderr, "ColormapNotify"); break;
        case 33: fprintf(stderr, "ClientMessage"); break;
        case 34: fprintf(stderr, "MappingNotify"); break;
        case 35: fprintf(stderr, "GenericEvent"); break;
        default: fprintf(stderr, "???"); break;

    }

}

void prtxevt(Display* d, XEvent* e)

{

    fprintf(stderr, "X Event: %5ld Window: %lx ", e->xany.serial,
            e->xany.window);
    prtxevtt(e->type);
    switch (e->type) {

        case Expose: fprintf(stderr, ": x: %d y: %d w: %d h: %d",
                             e->xexpose.x, e->xexpose.y,
                             e->xexpose.width, e->xexpose.height); break;
        case ConfigureNotify: fprintf(stderr, ": x: %d y: %d w: %d h: %d",
                             e->xconfigure.x, e->xconfigure.y,
                             e->xconfigure.width, e->xconfigure.height); break;
        case MotionNotify: fprintf(stderr, ": x: %d y: %d",
                                   e->xmotion.x, e->xmotion.y); break;
        case PropertyNotify: fprintf(stderr, ": atom: %s",
                                     XGetAtomName(d, e->xproperty.atom));

    }
    fprintf(stderr, "\n"); fflush(stderr);

}

int main(void) {

    Window         w;
    GC             gracxt;
    XEvent         e;
    const char*    msg = "Hello, window";
    int            s;
    Display*       d;
    XFontStruct*   font;
 
    d = XOpenDisplay(NULL);
    if (d == NULL) {

        fprintf(stderr, "Cannot open display\n");
        exit(1);

    }
 
    s = DefaultScreen(d);

    font = XLoadQueryFont(d,
        "-bitstream-courier 10 pitch-bold-r-normal--0-0-200-200-m-0-iso8859-1");
    if (!font) {

        fprintf(stderr, "*** No font ***\n");
        exit(1);

    }
    gracxt = XDefaultGC(d, s);
    XSetFont(d, gracxt, font->fid);

#ifdef BACKGROUND_PAINT
    w = XCreateSimpleWindow(d, RootWindow(d, s), 10, 10, 400, 300, 5,
                            BlackPixel(d, s), WhitePixel(d, s));
#else
    // no background draw
    w = XCreateWindow(d, RootWindow(d, s), 0, 0, 400, 300, 0, CopyFromParent,
                      InputOutput, CopyFromParent, 0, NULL);
#endif
    XSelectInput(d, w, ExposureMask|KeyPressMask|/*PointerMotionMask|*/
                       StructureNotifyMask/*|PropertyChangeMask*/);
    XMapWindow(d, w);

    while (1) {

        XNextEvent(d, &e);
        prtxevt(d, &e);
        if (e.type == Expose) {

            XSetForeground(d, gracxt, WhitePixel(d, s));
            XFillRectangle(d, e.xany.window, gracxt,
                           e.xexpose.x, e.xexpose.y,
                           e.xexpose.width, e.xexpose.height);
            XSetForeground(d, gracxt, BlackPixel(d, s));
            XDrawString(d, e.xany.window, gracxt, 10, 50, msg, strlen(msg));

        }

    }

    XCloseDisplay(d);

    return 0;

}
