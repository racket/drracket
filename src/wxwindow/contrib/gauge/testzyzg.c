/** testzyzg.c
 *
 *  DESCRIPTION: 
 *      This app is designed to show how to use the zYzGauge control in
 *      your applications.  It doesn't do a whole lot--so you don't have
 *      to wade through a bunch of code trying to figure out how to use
 *      it.
 *
 *  HISTORY:
 *      3/14/91     cjp     put in this comment
 *      6/19/92     cjp     updated a few things
 *
 ** cjp */
// COPYRIGHT:
//
//   (C) Copyright Microsoft Corp. 1992.  All rights reserved.
//
//   You have a royalty-free right to use, modify, reproduce and
//   distribute the Sample Files (and/or any modified version) in
//   any way you find useful, provided that you agree that
//   Microsoft has no warranty obligations or liability for any
//   Sample Application Files which are modified.
//


/* the includes we need */
#include <windows.h>
#include "zyzgauge.h"
#include "testzyzg.h"


/* tweakers for the dialog box using the zYzGauge */
#define TZYZG_RANGE     150     /* some reasonable range for control*/
#define TZYZG_MILLISECS 200     /* timer frequency in milliseconds  */
#define TZYZG_TIMERID   1       /* some unique local timer ID       */


/* prototypes for good measure */
LRESULT FAR PASCAL tzyzgWndProc(HWND, UINT, WPARAM, LPARAM);
BOOL FAR PASCAL tzyzgTestControlDlgProc(HWND, UINT, WPARAM, LPARAM);


/* globals, no less */
char    gszAppName[] = "zYzGauge Test Application";
HANDLE  ghInstance;
HWND    ghwnd;


/** void FAR PASCAL tzyzgSetControlParams(hwnd, wOrient, wRange, wFace,
 *                                          wEdge, rgbBkColor, rgbFgColor)
 *
 *  DESCRIPTION: 
 *      This function is used to set a bunch of parameters for the zYzGauge
 *      that hwnd defines.
 *
 *  NOTES:
 *
 ** cjp */

void FAR PASCAL tzyzgSetControlParams(HWND hwnd, WORD wOrient, WORD wRange,
                                    WORD wFace, WORD wEdge, LONG rgbBkColor,
                                    LONG rgbFgColor)
{
    /* set the orientation according to lParam value */
    SendMessage(hwnd, ZYZG_SETORIENTATION, wOrient, 0);

    /* set the range to some oddball value (demo) */
    SendMessage(hwnd, ZYZG_SETRANGE, wRange, 0);

    /* set the bezel face to some reasonable width */
    SendMessage(hwnd, ZYZG_SETBEZELFACE, wFace, 0);

    /* set the bezel edge to some width */
    SendMessage(hwnd, ZYZG_SETWIDTH3D, wEdge, 0);

    /* set the fore and back colors to cool values */
    SendMessage(hwnd, ZYZG_SETBKCOLOR, 0, rgbBkColor);
    SendMessage(hwnd, ZYZG_SETFGCOLOR, 0, rgbFgColor);
} /* tzyzgSetControlParams() */


/** BOOL FAR PASCAL tzyzgTestControlDlgProc(HWND, UINT, WPARAM, LPARAM)
 *
 *  DESCRIPTION: 
 *      This is the test dialog box for the zYzGauge control.  What it
 *      does is start a timer 
 *
 *  NOTES:
 *
 ** cjp */

BOOL FAR PASCAL tzyzgTestControlDlgProc(HWND   hdlg,
                                        UINT   uMsg,
                                        WPARAM wParam,
                                        LPARAM lParam)
{
    static WORD hTimer;             /* handle to the timer we use       */
    static WORD wCount;             /* counter for 'position' in control*/

    switch (uMsg)
    {
        case WM_INITDIALOG:
            /* set up all of the gauges */
            tzyzgSetControlParams(GetDlgItem(hdlg, IDD_ZYZGAUGE1),
                                   ZYZG_ORIENT_BOTTOMTOTOP, TZYZG_RANGE, 4, 2,
                                   RGB(128, 128, 255), RGB(0, 0, 255));
            tzyzgSetControlParams(GetDlgItem(hdlg, IDD_ZYZGAUGE2),
                                   ZYZG_ORIENT_RIGHTTOLEFT, TZYZG_RANGE, 2, 2,
                                   RGB(128, 128, 255), RGB(0, 0, 0));
            tzyzgSetControlParams(GetDlgItem(hdlg, IDD_ZYZGAUGE3),
                                   ZYZG_ORIENT_LEFTTORIGHT, TZYZG_RANGE, 1, 1,
                                   RGB(128, 0, 128), RGB(0, 255, 0));
            tzyzgSetControlParams(GetDlgItem(hdlg, IDD_ZYZGAUGE4),
                                   ZYZG_ORIENT_LEFTTORIGHT, TZYZG_RANGE, 1, 4,
                                   RGB(64, 64, 64), RGB(0, 0, 0));
            tzyzgSetControlParams(GetDlgItem(hdlg, IDD_ZYZGAUGE5),
                                   ZYZG_ORIENT_RIGHTTOLEFT, TZYZG_RANGE, 6, 0,
                                   RGB(128, 128, 128), RGB(255, 255, 255));
            tzyzgSetControlParams(GetDlgItem(hdlg, IDD_ZYZGAUGE6),
                                   ZYZG_ORIENT_TOPTOBOTTOM, TZYZG_RANGE, 4, 3,
                                   RGB(255, 128, 128), RGB(255, 0, 0));

            /* I want the focus on the ok button */
            SetFocus(GetDlgItem(hdlg, IDOK));

            /* start our timer and set counter to 'zero percent done' */
            wCount = 0;
            hTimer = SetTimer(hdlg, TZYZG_TIMERID, TZYZG_MILLISECS, NULL);
            break;

        case WM_TIMER:
            /* step */
            wCount++;

            /* set the position to +1 of the previous value */
            SendMessage(GetDlgItem(hdlg, IDD_ZYZGAUGE1),
                                ZYZG_SETPOSITION, wCount, 0);
            SendMessage(GetDlgItem(hdlg, IDD_ZYZGAUGE2),
                                ZYZG_SETPOSITION, wCount, 0);
            SendMessage(GetDlgItem(hdlg, IDD_ZYZGAUGE3),
                                ZYZG_SETPOSITION, wCount, 0);
            SendMessage(GetDlgItem(hdlg, IDD_ZYZGAUGE4),
                                ZYZG_SETPOSITION, wCount, 0);
            SendMessage(GetDlgItem(hdlg, IDD_ZYZGAUGE5),
                                ZYZG_SETPOSITION, wCount, 0);
            SendMessage(GetDlgItem(hdlg, IDD_ZYZGAUGE6),
                                ZYZG_SETPOSITION, wCount, 0);

            /* are we done?? */
            if (wCount > TZYZG_RANGE)
                hTimer = !KillTimer(hdlg, TZYZG_TIMERID);
            break;

        case WM_COMMAND:
            if ((wParam == IDOK) || (wParam == IDCANCEL))
            {
                /* if it isn't dead, kill it! */
                if (hTimer)
                    KillTimer(hdlg, TZYZG_TIMERID);

                EndDialog(hdlg, (wParam == IDOK));
            }
            break;
    }

    return (FALSE);
} /* tzyzgTestControlDlgProc() */



/** LRESULT FAR PASCAL tzyzgWndProc(HWND, UINT, WPARAM, LPARAM)
 *
 *  DESCRIPTION: 
 *      This is just your normal everyday WndProc().
 *
 *  NOTES:
 *
 ** cjp */

LRESULT FAR PASCAL tzyzgWndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    FARPROC  lpfnDlgProc;

    switch (uMsg) 
    {
        case WM_DESTROY:
            PostQuitMessage(0);
            break;

        case WM_COMMAND:
        {
            switch (wParam)
            {
                case IDM_TEST_TEST:
                    /* standard stuff (but not really necessary in 3.1) */
                    lpfnDlgProc = MakeProcInstance(tzyzgTestControlDlgProc, ghInstance);
                    DialogBox(ghInstance, DLG_TESTZYZG, ghwnd, lpfnDlgProc);
                    FreeProcInstance(lpfnDlgProc);
                    break;

                case IDM_TEST_EXIT:
                    SendMessage(hwnd, WM_CLOSE, 0, 0L);
                    break;
            }
        }
        break;

        case WM_CLOSE:
            DestroyWindow(hwnd);
            break;

        default:
            return (DefWindowProc(hwnd, uMsg, wParam, lParam));
    }

    return (0L);
} /* tzyzgWndProc() */


/** int PASCAL WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
 *
 *  DESCRIPTION: 
 *      This is just your normal everyday WinMain() with one exception:
 *      it calls the 'gaugeInit()' function to get the zYzGauge
 *      class initialized.  If you don't call this, the dialog box will
 *      fail to open because the zYzGauge control will not initialize
 *      properly (the class won't exist).
 *
 *  NOTES:
 *
 ** cjp */

int PASCAL WinMain(HINSTANCE   hInstance,
                   HINSTANCE   hPrevInstance,
                   LPSTR       lpszCmdLine,
                   int         nCmdShow)
{
    WNDCLASS    wc;
    MSG         msg;

    ghInstance = hInstance;

    /* initialize the zYzGauge class */
    if (!gaugeInit(hInstance))
        return (FALSE);

    if (!hPrevInstance) 
    {
        wc.style            = CS_HREDRAW | CS_VREDRAW;
        wc.lpfnWndProc      = tzyzgWndProc;
        wc.cbClsExtra       = 0;
        wc.cbWndExtra       = 0;
        wc.hInstance        = hInstance;
        wc.hIcon            = LoadIcon(hInstance, ICON_TESTZYZG);
        wc.hCursor          = LoadCursor(NULL, IDC_ARROW);
        wc.hbrBackground    = (HBRUSH)(COLOR_WINDOW + 1);
        wc.lpszMenuName     = MENU_TESTZYZG;
        wc.lpszClassName    = gszAppName;

        if (!RegisterClass(&wc))
            return (FALSE);
    }

    /* open our window */
    ghwnd = CreateWindow(gszAppName, gszAppName, WS_OVERLAPPEDWINDOW,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          TZYZG_WINDOW_WIDTH, TZYZG_WINDOW_HEIGHT,
                          NULL, NULL, hInstance, NULL);             

    /* was the window created?? */
    if (!ghwnd)
        return (FALSE);

    ShowWindow(ghwnd, nCmdShow);
//  UpdateWindow(ghwnd);

    /* the ubiquitous message dispatcher */
    while (GetMessage(&msg, NULL, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    return (msg.wParam);
} /* WinMain() */


/** EOF: testzyzg.c **/
