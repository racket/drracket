/** testzyzg.h                                                          **/

#ifndef _TESTZYZG_H_
#define _TESTZYZG_H_


/* bilingual... */
#ifdef RC_INVOKED
    #define RCID(id)    id
#else
    #define RCID(id)    MAKEINTRESOURCE(id)
#endif


/* misc. defines */
#define ICON_TESTZYZG           RCID(10)

#define MENU_TESTZYZG           RCID(20)
#define IDM_TEST_TEST           100
#define IDM_TEST_EXIT           104


/* for the test dialog box--the zyzgauge control id */
#define DLG_TESTZYZG            RCID(30)
#define IDD_ZYZGAUGE1           100
#define IDD_ZYZGAUGE2           101
#define IDD_ZYZGAUGE3           102
#define IDD_ZYZGAUGE4           103
#define IDD_ZYZGAUGE5           104
#define IDD_ZYZGAUGE6           105


/* more misc. defines */
#define TZYZG_WINDOW_WIDTH      400
#define TZYZG_WINDOW_HEIGHT     200


#endif

/** EOF: testzyzg.h **/


