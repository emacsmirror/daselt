# D-Emacs
This package is [Daselt](https://gitlab.com/nameiwillforget/daselt)'s [Emacs](https://www.gnu.org/software/emacs/) module. Daselt is a global configuration scheme for a GNU/Linux system, providing, among other things, a custom keyboard layout and a shortcut meta-layout. Daselt's Emacs component consists of several parts:

- `daselt-base` provides a host of functions for the other packages that might be useful for building other packages as well.

- `daselt-coords` provides functions to for the coordinatization of layouts.

- `daselt-xkb` can import custom [xkb](https://www.x.org/wiki/XKB/)-layouts into Emacs.

- `daselt-dfk` can generate [Dual Function Keys](https://gitlab.com/interception/linux/plugins/dual-function-keys) configurations from coordinates
  and add them to layouts as a zeroth layer.

- `daselt-bind` allows Emacs to read bindings that use coordinates and store
  and manipulate them in bindlists.

- `daselt-dirs` provides functions to recursively act on files in a directory
  and automatically apply actions on file save.

- `daselt-mode` provides a mode that implements Daselt's shortcut layout in
  Emacs.

- `daselt-stump` can generate [StumpWM](https://github.com/stumpwm/stumpwm/wiki) configurations from bindlists.

- `daselt-tri` can generate [Tridactyl](https://github.com/tridactyl/tridactyl) configurations from bindlists.

Daselt's Emacs module can now be installed through MELPA. However, to use Daselt, it is necessary to install some of its other components too.
For more information, please visit the main page of [Daselt](https://gitlab.com/nameiwillforget/daselt).
