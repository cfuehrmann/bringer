Overview
--------

Bringer is a utility that acts as a program launcher and desktop
switcher for Linux. It occupies no screen space and only appears when
it is invoked (typically by a keyboard shortcut of the window manager
or desktop environment). While its appearance is simple, its features
are powerful:

* All commands in the user's path are presented and can be picked by
substring search or arrow keys.

* All commands the user entered (including commands with arguments,
like "emacs myFavoriteFile" of "vi myFavoriteFile") are remembered in
a file ("~/.bringerHistory"), and presented on a most-frequently-used
basis. They too can be picked by substring search or arrow keys.

* At the top of all choices, the desktops are presented with
descriptions of their windows. They too can be picked by substring
search or arrow keys.

* Bringer also displays the date and time whenever it is invoked

Bringer is optimized for tiling window managers, because the switching
is between desktops, not between windows. Still, it may also be useful
for "normal" (i.e. stacking or compositing) window managers.

The order of the desktops is based on the windows they contain, not on
the desktops' integer numbers. This is to keep the ordering
stable. For example, on my machine the desktop containing chromium is
always the topmost, so I could switch to chromium without looking at
the screen. For the same reason, if a desktop contains several
windows, those are sorted alphabetically by the underlying command
(not by the title, because that might change during a window's
life). The leftmost window in the display line for a desktop is the
most significant one when it comes to ordering the desktops. The
overall effect of these sorting measures is this: if one closes all
windows and then starts all those applications again, bringer's
desktop overview will look precisely as before.

The current desktop is marked by an asterisk.

After the desktop lines follow the entries from the command history.
The top one (and thereby the most frequently used) also has the date
and time displayed to the right of it. Below the history entries
follows a wide separator of the form "---...---". Below that follow
the commands from the path.

For details about searching and picking entries, see the manual of
dmenu.


More features
-------------

The extra features come in the form of "suffix commands", which start
with the character ! after the selected entry:
 
* Delete an unwanted history entry: when bringer is invoked, select
the unwanted history entry with the TAB key. Append the two characters
!d to it, and press ENTER. This deletes all occurrences of the entry
from the history file.

* Close an off-screen window: when bringer invoked, use the TAB key
to select the desktop from which you want to delete a window. Append
the two characters !c followed by the window's index within the
desktop. The leftmost window has index 0. If you leave away the index,
0 is assumed.


Tips
----

* When choosing a keyboard shortcut for bringer, it is worth noting
that RETURN and the arrow keys tend to be bringer's most frequently
used keys: RETURN for invoking a choice, and the arrow keys for
selecting a choice. So it is convenient to have the shortcut for
bringer in the same keyboard region. CTRL + RETURN works very well.


Requirements
------------

* Linux

* The command-line tool "dmenu"

* The command-line tool "wmctrl"

* A window manager that works with wmctrl, see
  [here](http://tomas.styblo.name/wmctrl/). Instructions for xmonad are 
  given further below.

* An OCaml compiler, in particular ocamlbuild (don't be afraid though,
  it's really simple)

* "OCaml Batteries included" (short: "Batteries"), a widely-available
  library for OCaml (it comes as a package with some distributions - I
  checked for Ubuntu and Gentoo, which I use myself)


Installation
------------

In the root directory of the project, execute

    ocamlbuild -cflags -I,+extlib -libs str,unix,extlib/extLib bringer.native

Or, in the unlikely case where you have no native compiler for your
platform, execute

    ocamlbuild -cflags -I,+extlib -libs str,unix,extlib/extLib bringer.byte

The executable you need to make a keyboard shortcut for is then
"bringer.native" or "bringer.byte", respectively.

Remark: the "extlib" stands for the above-mentioned "Batteries".

If you use xmonad, it must be made EWMH-compatible by using

    import Xmonad.Hooks.EwmhDesktops

in your "xmonad.hs" and enabling it for example as follows:

    main = do
        xmonad $ ewmh defaultConfig { ...


Configuration
-------------

Bringer is configured by editing the file "config.ml" and recompiling
(as described in the installation section). While the configuration
file is written in OCaml, it is so simple and self-explanatory that it
can be edited without knowledge of the language.  (Thus we follow the
approach of the window manager xmonad, which is written in Haskell,
just like its configuration file.)

Currently, the only aspect that can be configured is the formatting of
the human-readable description of a window. To see why this seems
necessary, consider the following cases:

* Suppose you launch "emacs myFavoriteFile". The title of the
resulting window will be "emacs@hostname" (no mentioning of the file),
so it is best to display the command instead.

* Suppose you launch Eclipse. The command, at least on my machine,
turns out to be "java hugeHorribleStringOfArguments". So it is best to
display the window title instead.
 

Original motivation
------------------- 

When using Linux, I use the xmonad window manager. By default, xmonad
offers dmenu and Gmrun for launching programs. In my setup, I used to
add a task bar to see which applications are running and to
switch between desktops. Dmenu is very slick but has no command
history. Gmrun takes one or two more key presses, but has a command
history, which is searchable. I wanted to deal with all those features
(launching searchable commands, searchable command history, awareness
of running applications and desktop switching) in such a way that the
following conditions are met:

* There should be only **one** utility, so that there is always one
optimal choice for a particular action

* No screen space should be used until the utility is invoked

* In particular, when I need an application to run, I want the
launcher to show me whether it is already running. To me this looks
like a key feature that most launchers and desktop environments fail
to understand. (Interestingly, the recent GnomeShell seems to address
precisely this issue, although many reviewers seem to understand
that.)


Future enhancements
-------------------

See the items labeled "Feature" in the GitHub issue tracker.


Why is bringer written in OCaml? 
--------------------------------

* First-class functions are a must for me, otherwise I feel like
  walking on one leg

* The type system of ML-type languages is very powerful and safe, and
at the same time nicely none-intrusive owing to automatic type
inference.

* So far, the same is true for Haskell. However, in my paid job in the
.Net-world, I am considering to switch from C# to F#, which is a
descendant of OCaml, so I get synergies from using the latter.

* That might still leave F# on Mono. But referencing Mono is too
heavy-weight for my liking, in particular for a small project.

* I wrote the first version of bringer in Bash, which I soon despised.
So I wondered how it would feel to use a high-level language
instead. Porting bringer to OCaml served as my test. The result was
that the code got a lot nicer, reusable, and more fun to work with. I
would never go back to Bash for any program longer than a dozen
lines. Admittedly, the OCaml code was about 20% longer than the Bash
code (counting characters). This is mostly because Bash needs no
boilerplate code to access operating-system functionality.
