/* See LICENSE file for copyright and license details. */

/* appearance */
static const unsigned int borderpx  = 4;        /* border pixel of windows */
static const unsigned int snap      = 0;        /* snap pixel */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
#ifdef USERFONT
static const char *fonts[]          = { USERFONT };
static const char dmenufont[]       = USERFONT;
#else
static const char *fonts[]          = { "Inconsolata:style=Regular:size=12:antialias=true:autohint=true" };
static const char dmenufont[]       = "Inconsolata:style=Regular:size=12:antialias=true:autohint=true";
#endif
static const char col_gray1[]       = "#222222";
static const char col_gray2[]       = "#444444";
static const char col_gray3[]       = "#bbbbbb";
static const char col_gray4[]       = "#eeeeee";
static const char col_purple[]      = "#7704a8";
static const char col_dark_purple[] = "#421654";
static const char *colors[][3]      = {
    /*               fg         bg          border   */
    [SchemeNorm] = { col_gray3, col_gray1,  col_gray2 },
    [SchemeSel]  = { col_gray4, col_dark_purple, col_purple  },
};

/* tagging */
static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

static const Rule rules[] = {
    /* xprop(1):
     *    WM_CLASS(STRING) = instance, class
     *    WM_NAME(STRING) = title
     */
    /* class      instance    title       tags mask     isfloating   monitor */
    { "Gimp",     NULL,       NULL,       0,            1,           -1 },
};

/* layout(s) */
static const float mfact     = 0.55; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */

static const Layout layouts[] = {
    /* symbol     arrange function */
    { "[M]",      monocle }, /* first entry is default */
    { "[]=",      tile },
    { "TTT",      bstack },
    { "===",      bstackhoriz },
    { "><>",      NULL },    /* no layout function means floating behavior */
};

/* key definitions */
#define MODKEY Mod1Mask
#define TAGKEYS(KEY,TAG) \
    { MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
    { MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
    { MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
    { MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = { "dmenu_histogram", "-m", dmenumon, "-fn", dmenufont, "-nb", col_gray1, "-nf", col_gray3, "-sb", col_purple, "-sf", col_gray4, NULL };
static const char *termcmd[]  = { "st", NULL };
static const char *browsercmd[]  = { "firefox", NULL };

static const char *monbrightnessinccmd[] = { "setbacklight", "+10", NULL };
static const char *monbrightnessdeccmd[] = { "setbacklight", "-10", NULL };
static const char *monbrightnessbiginccmd[] = { "setbacklight", "50", NULL };
static const char *monbrightnessbigdeccmd[] = { "setbacklight", "-50", NULL };
static const char *kbdbrightnessinccmd[] = { "kb-light.py", "--up", "1", NULL };
static const char *kbdbrightnessdeccmd[] = { "kb-light.py", "--down", "1", NULL };
static const char *volumeinccmd[] = { "amixer", "sset", "Master", "1%+", NULL };
static const char *volumedeccmd[] = { "amixer", "sset", "Master", "1%-", NULL };
static const char *volumemutecmd[] = { "amixer", "sset", "Master", "toggle", NULL };
static const char *volumebiginccmd[] = { "amixer", "sset", "Master", "5%+", NULL };
static const char *volumebigdeccmd[] = { "amixer", "sset", "Master", "5%-", NULL };
static const char *qwertycmd[] = { "aoeu", NULL };
static const char *dvorakcmd[] = { "asdf", NULL };

#include <X11/XF86keysym.h>

static Key keys[] = {
    /* modifier                     key        function        argument */
    { MODKEY,                       XK_p,      spawn,          {.v = dmenucmd } },
    { MODKEY|ShiftMask,             XK_Return, spawn,          {.v = termcmd } },
    { MODKEY,                       XK_Return, spawn,          {.v = browsercmd } },
    { MODKEY,                       XK_t,      togglebar,      {0} },
    { MODKEY,                       XK_apostrophe,      focusstack,     {.i = +1 } },
    { MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
    { MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
    { MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
    { MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
    { MODKEY|ShiftMask,             XK_space,  zoom,           {0} },
    { MODKEY,                       XK_Tab,    view,           {0} },
    { MODKEY|ShiftMask,             XK_c,      killclient,     {0} },
    { MODKEY,                       XK_y,      setlayout,      {.v = &layouts[0]} },
    { MODKEY,                       XK_u,      setlayout,      {.v = &layouts[1]} },
    { MODKEY,                       XK_i,      setlayout,      {.v = &layouts[2]} },
    { MODKEY,                       XK_o,      setlayout,      {.v = &layouts[3]} },
    { MODKEY,                       XK_p,      setlayout,      {.v = &layouts[4]} },
    { MODKEY,                       XK_space,  setlayout,      {0} },
    { MODKEY|ShiftMask,             XK_f,      togglefloating, {0} },
    { MODKEY,                       XK_0,      view,           {.ui = ~0 } },
    { MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
    { MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
    { MODKEY,                       XK_period, focusmon,       {.i = +1 } },
    { MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
    { MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
    TAGKEYS(                        XK_1,                      0)
    TAGKEYS(                        XK_2,                      1)
    TAGKEYS(                        XK_3,                      2)
    TAGKEYS(                        XK_4,                      3)
    TAGKEYS(                        XK_5,                      4)
    TAGKEYS(                        XK_6,                      5)
    TAGKEYS(                        XK_7,                      6)
    TAGKEYS(                        XK_8,                      7)
    TAGKEYS(                        XK_9,                      8)
    { MODKEY|ShiftMask,             XK_q,      quit,           {0} },
    { 0,                            XF86XK_MonBrightnessUp, spawn, {.v = monbrightnessinccmd } },
    { 0,                            XF86XK_MonBrightnessDown, spawn, {.v = monbrightnessdeccmd } },
    { ShiftMask,                    XF86XK_MonBrightnessUp, spawn, {.v = monbrightnessbiginccmd } },
    { ShiftMask,                    XF86XK_MonBrightnessDown, spawn, {.v = monbrightnessbigdeccmd } },
    { MODKEY,                       XF86XK_MonBrightnessUp, spawn, {.v = kbdbrightnessinccmd } },
    { MODKEY,                       XF86XK_MonBrightnessDown, spawn, {.v = kbdbrightnessdeccmd } },
    { 0,                            XF86XK_AudioRaiseVolume, spawn, {.v = volumeinccmd } },
    { 0,                            XF86XK_AudioLowerVolume, spawn, {.v = volumedeccmd } },
    { ShiftMask,                    XF86XK_AudioRaiseVolume, spawn, {.v = volumebiginccmd } },
    { ShiftMask,                    XF86XK_AudioLowerVolume, spawn, {.v = volumebigdeccmd } },
    { 0,                            XF86XK_AudioMute, spawn, {.v = volumemutecmd } },
    { MODKEY,                       XK_a,      spawn,          {.v = dvorakcmd } },
    { MODKEY,                       XK_m,      spawn,          {.v = qwertycmd } },
    { MODKEY,                       XK_b,      togglebar,      {0} },
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
    /* click                event mask      button          function        argument */
    { ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
    { ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
    { ClkWinTitle,          0,              Button2,        zoom,           {0} },
    { ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
    { ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
    { ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
    { ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
    { ClkTagBar,            0,              Button1,        view,           {0} },
    { ClkTagBar,            0,              Button3,        toggleview,     {0} },
    { ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
    { ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};

