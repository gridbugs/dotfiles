# OpenBSD Notes

## xsession

When logging into an X session with xenodm, the ~/.xsession script is run, which typically
performs some setup and starts a window manager. The xinitrc script invokes some scripts
from the home directory (such as the bar). Several additional directories must be added to
PATH for this to work correctly. Additionally, to prevent the need for new terminal windows
to run login shells, the ~/.profile script should be sourced when X starts, such that the
window manager (which launches terminals) is running in the .profile environment.
