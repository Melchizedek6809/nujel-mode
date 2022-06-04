# An Emacs major mode for working with Nujel

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

Greetings to my first attempt at providing a major mode for the Nujel programming language, I tried my best at making the keybindings
similar to what Slime/Geiser/Emacs use, so you should feel right at home.  Apart from that, it is heavily based on the Newlisp major mode by Tim Johnson with modifications making up for the syntactic differences in syntax.

Before you can use of the REPL functionality you need to make a Nujel executable available through your PATH.

Since the colours used have been chosen and tested with only morgentau-theme in mind, they might look quite jarring with another theme. When things look particularly hideous in a theme you enjoy then be sure to let me know by opening up an issue so that it might get fixed by swapping around some faces.

## Installation
Since nujel-mode is currently not available through melpa it is probably most convenient to clone this repostiory into a directory that is in your Emacs load-path.
