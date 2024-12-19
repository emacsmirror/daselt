;;; d-tutorial.el --- Tutorial for daselt-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Alexander Prähauser

;; Author: Alexander Prähauser <ahprae@protonmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tutorial shown when daselt-mode is started if `daselt-show-tutorial' is t.

;;; Code:

(concat "#+title: Daselt-Tutorial
#+author:Alexander Prähauser

* Preamble
Welcome to the Daselt tutorial! Note that you can stop the display of this tutorial by setting the constant ~d-show-tutorial~ to ~nil~. You can save this tutorial by pressing *C-(1 0 5)* and you can save your position by setting a bookmark using *M-(2 -1 -2)*, see below for an explanation of these coordinates. You can re-generate this tutorial with the command ~d-generate-tutorial~. Note that parts of this tutorial are set based on Daselt customs that have been set automatically. If you decide to change them, you can re-generate this tutorial using the command ~d-generate-tutorial~.

* Introduction
Let's start with the basic navigation shortcuts. To move point (the Emacs cursor) left and right you can hold down the _Space_ bar and press the keys " (d-xkb--binding-from-coords '(1 0 -2)) " and " (d-xkb--binding-from-coords '(1 0 2))". Similarly you can move up and down with " (d-xkb--binding-from-coords '(1 0 -3)) " and "(d-xkb--binding-from-coords '(1 0 3))" and scroll down an entire screen using "(d-xkb--binding-from-coords '(1 0 1))

(if (or d-stump d-emacs-translate-C-1-1--2-C-g
        (not (string= (d-xkb--binding-from-coords '(1 0 -1)) "g")))
    (concat ". Silmilarly, you can scroll up using *C-"
            (d-xkb--binding-from-coords '(1 0 -1))
            "*. N")
  "Note that you cannot use *C-g* to scroll a screen up though. This is because the *C-g*-signal aborts running processes in Emacs, and this action cannot easily be remapped. There are ways to remedy this, but they come with some caveats, see [[*The *C-g* Issue]]. In the meantime you can use " (d-xkb--binding-from-coords '(1 1 -2)) "to scroll up a screen. In any case, n")

"otice the symmetry and flatness of these bindings. These are consistent features of Daselt's shortcuts. If any one of the shortcuts in this tutorial does not work for you don't panic, it might be a bad key (see the section [[Bad Key Configs]]). In the absolutely worst case you can use the arrow keys to navigate, but that shouldn't be necessary.

* Modifiers
"
(if d-emacs-avy-act
    (concat "If you hold down *_Space_* and either *_CapsLock_* or the key next to your right pinky on the home row and tap " (d-xkb--binding-from-coords `(1 1 2)) " , then you can see the lines in the current emacs buffer being prefixed by either one or two symbols. If you press the symbols next to the upper end of the diagram below, you can put it at the top of your screen. If you accidentally hit the wrong symbols, you can always return to the previous position by holding down _Space_ and tapping "
            (d-xkb--binding-from-coords `(1 -1 -1))
            "

")
  (concat "If you move point down to top of the diagram below, hold down _Space_ and either _CapsLock_ or the key next to your right pinky on the home row and tap *"
          (d-xkb--binding-from-coords `(1 1 2))
          "* you can place the diagram below at the top of your screen.

"))

(d-capture-inserted-text #'d-xkb-draw-keyboard-layer 0 t)

"
This is a Daselt-keyboard-diagram. It displays the modifiers in the Daselt-layout and should be read in the following way:

- The uppermost row displays the column coordinates. 

- One-letter-expressions designate modifiers that are applied while the corresponding key is held down (called /continuous modifiers/ in Daselt-lingo). You have already applied the *C(trl)*-modifier by holding down the _Space_-key, here displayed right in the middle of the lowest row of the diagram. The other continuous modifiers are placed in the number row.

- Numbers designate the corresponding layer of Daselt's layout that the other keys are shifted to while the corresponding key is held down. For instance, the _Alt_-keys shift keys to the second keyboard-layer, which houses uppercase-letters. If you want, you can try this out right now by holding down one of the _Alt_ keys and typing some text. You can undo your actions using *C-" (d-xkb--binding-from-coords '(1 -1 -2))"*.

- If an expression is of the form *A/B*, then *A* designates what happens when the key is held down while *B* is what happens when the key is tapped. For instance, the _4_- and _8_-keys in the QWERTY-layout, or whatever analogue you have on your keyboard, shift the keyboard to the "(if d-dfk-dual-functions-outside-main-keys "seventh" "third")" layer while held down and send *Compose*-signals when tapped, which allow you to write symbols used in your local language using those of the standard Latin alphabet (see [[https://www.x.org/releases/current/doc/libX11/i18n/compose/en_US.UTF-8.html][here]] for all available sequences).

- An expression of the form *dX* denotes a so-called /discrete modifier/, meaning a signal that makes Emacs apply the modifier X to the next signal it receives. For instance, the _Alt_-keys apply an *M*(eta)-modifier to the next signal if they are tapped.

- An expression of the form *⟨X⟩Y* means that *Y* is triggered when *X* is held down. In particular, the _Alt_-keys apply a discrete *s(uper)*-modifier when they are tapped with a *C*-modifier, so holding down _Space_ and tapping _Alt_ will apply *s* to the next received signal.

- The expressions of the form *<X>* at the end of slashed expressions are the names of the signals Emacs receives while the keys are tapped.

- The *⟨I⟩dA*-symbol at the very left of the layout designates a discrete *A(lt)*-modifier that should be there if you have an ISO-keyboard-layout, in which the left Shift-key is smaller than _CapsLock_ and allows for an extra key in the lower row. In an ANSI-layout, in which _LeftShift_ is longer than _CapsLock_. In the second case, _LeftShift_ has to house *?* and thus there is no place for another modifier. You can test out whether your system is configured correctly by tapping _LeftShift_ and if you find that its configuration does not fit your keyboard you should correct your your ~udevmon.yaml~ config file to point to the right d-dfk-configuration.

The signals (symbols and function signals) on this layer are generally constant over the other layers: *SPC* is always *SPC*, *<XF86Launch5>* is always *<XF86Launch5>* and so on.

"

(unless (eq d-xkb-layout 'd-xkb-main-layout) (concat "Note that some design compromises had to be made for layouts other than Daselt's main layout because dual functions on keys housing letters are error-prone." (if d-dfk-dual-functions-outside-main-keys "In particular, the modifiers for the third layer had to be put onto the _4_ and _8_ keys, and the " (d-xkb--binding-from-coords '(1 1 0)) "houses a H(yper)-modifier in the main layout that had to be taken out for the same reason. If you are willing to type slower or accept the possibility of occasional errors, you can re-introduce the more optimal modifier placement by setting ~d-dfk-reduced.yaml~ or ~d-dfk-reduced-iso.yaml~ to ~d-dfk.yaml`/`d-dfk-iso.yaml~ in your ~udevmon.yaml~ config file (provided you copied the unreduced ~d-dfk~ files into your ~dual-function-keys~ directory).")))

"* Keyboard Layout
** Layer *1* and Coordinates

"
(d-capture-inserted-text #'d-xkb-draw-keyboard-layer 1 t)

"
Here you can see the first layer of Daselt's layout. Notice the empty cells in the middle of the upper two rows and next to the key in the middle of the lower row. These exist due to the peculiar shape of a standard keyboard, in which the middle of the upper two rows is in-between two other keys while there is a key right in the middle of the lowest row (the B-key in QWERTY). This allows us to describe Daselt's bindings using an assignment of coordinates to each key given by the row the key is in and its distance from the middle of the keyboard. The coordinates of the keys are shown in the next diagram:

"
(d-capture-inserted-text #'d-xkb-draw-key-coordinates nil t)

"
The coordinates are chosen in such a way that all keys with the same /place coordinate/ (meaning the last coordinate) are in a column so that they can be pressed with the same finger: *1* and *2* with the index finger, *3* with the middle finger, *4* with the ring finger and *5* and *6* with the little finger (same for negative values).

From these key coordinates we can create coordinates for each signal in Daselt's layout by prefixing the key coordinates with the layer layer number as a third coordinate. Thus, *(1 0 2)* denotes the signal housed by the key under your index finger on layer 1, which in this case is " (d-xkb--binding-from-coords '(1 0 2)) ". Daselt is mostly structured according to position and this allows us to keep shortcuts independent from particular signals assigned to a key. Accordingly, we will from now on use position instead of the attached signal to write shortcuts. For instance, *C-(1 1 2)* allows you to execute a command.

** Layer 2

"
(d-capture-inserted-text #'d-xkb-draw-keyboard-layer 2 t)
"
As already mentioned, this layer consists mostly of uppercase variants of lowercase letters. The big exception are *Backspace* and *Return*, which are also on this layer to make them easier to apply. Note however that because these are on the layer that is shifted to by applying Shift, they are automatically endowed with shift modifiers, so programs will interpret them as *S(hift)-Backspace* and *S-Return*, which in some programs makes a difference (in Emacs differences only arise in mode-specific bindings). I haven't really found anything I can do against that so sometimes you might need to use normal _Backspace_ and _Enter_." (unless (eq d-xkb-layout 'd-xkb-main-layout) " Moreover, the only position I could give these bindings is on punctuation signs, which depending on your layout might not be on ideal keys. If that is the case, just view these keys as a free bonus — you don't need to use them but you can if you want.

You can lock the second layer, like pressing _CapsLock_ would in QWERTY, by holding down both _Alt_ keys at the same time.")

"

** Layer 3

"
(d-capture-inserted-text #'d-xkb-draw-keyboard-layer 3 t)

"
This layer houses punctuation marks, brackets and other special signs, most of which are in ASCII.

** Layer 4
"
(d-capture-inserted-text #'d-xkb-draw-keyboard-layer 4 t)
"
As you can see, this layer houses numbers, calculation signs and \"backup\" arrow keys. The placement of numbers is probably the least intuitive aspect of the layout. It is a design compromise and might take  a while to learn, but there is a system: lower numbers were given better positions than higher ones and even numbers are on the left while odd ones are on the right. Overall, this makes the typing of small numbers fairly easy while that of larger numbers is still workable though, admittedly, a bit cumbersome. To further alleviate this, a bash script ~d-toggle-layer-lock~ is in the ~scripts~ folder of Daselt, which allows for locking any layer using ~xdotools~. The binding of this script to keys in StumpWM is still work in progress because it runs into the [[https://github.com/stumpwm/stumpwm/wiki][Problems Stump has with XKB]]. Binding it to keys in other window managers might be easier though.

These four layers together house all signs found on a normal keyboard (except regional signs that can be typed using the *Compose* signal). Layers *5*, *6*, *7* and *8* house Greek letters, mathematical signs and Emoji but rather than go through them here it is better to explain how to draw them on the fly.

*The Emacs help menu is bound to *C-(1 0 -5)**, so the key under your left pinky (and *C-h* in the main layout). By typing *C-(1 0 -5) (3 0 -2)*, so by opening the help menu and pressing the key under your left index finger shifted to the third layer, you can execute ~d-xkb-draw-layer~. You will be asked to choose a layer based on its number. Since you might have forgotten where a particular number is, you can always look at the fourth layer by pressing *(1 0 2)*. Similarly, you can look at the 0-th layer (the modifier layer) by pressing (1 0 -2). The chosen layer will be displayed in a maximised window and you will be asked a yes-or-no question on whether you want to restore the previous window configuration or not. Note that, if you have set ~use-short-answers~ to t, you can press *(1 0 2)* instead of *y* and *(1 0 -2)* instead of *n* to answer yes-or-no questions. More generally, *y* and *n* are replaced by the values of *(1 0 2)* and *(1 0 -2)* in multiple-choice-queries. Since this replacement uses override advice, it can be disabled by setting ~d-emacs-replace-multiple-choice~ to ~nil~ (before starting ~d-emacs-mode~).

Note also that, if you choose to not return to the previous window configuration or press the wrong key, you can still navigate back using the shortcut *C-(7 0 -1)* (running ~tab-bar-history-back~).

* Shortcut Layout
 Here we can see the *C-1*-layer, the most basic layer:
"
(let ((current-prefix-arg t))
  (d-capture-inserted-text #'d-draw-bindlist-layer 'd-emacs-d-emacs-mode-map-bindlist "1" "C" "^s" "^M" "^S" "^H"))

"
Depending on your screen, this layer might be too large to display completely without line breaks unless you scale down your font size, which you can do using *C-(7 -1 -1)*.

This layer displays the archetypal assortment of shortcuts that Daselt is trying implement wherever it can. The other layers often implement the same general function on different structures, so if you know what the key does in general, you can often guess what it does in any particular layer.

 Let us take a quick look at the heuristics for other layers and modifiers. Not all bindings can be guessed using these but a lot can, and once you understand them and know how to call Daselt's help functions, you can start to explore its functionality by yourself.

** Heuristics
- *C-1* concerns letters, lines, screens, local marks, literal searches, Undo and Redo and help and internet searches.

- *C-2* concerns words, paragraphs, sentences and subsentences, bookmarks, regexp searches and finding and saving files.

- *C-3* is globally empty to allow for mode-specific bindings (except for *C-(1 0 2)*, which has a special function, see below).

- *C-4* and *C-5* concern balanced expressions. *C-4* is also for recentering windows and finding definitions.

- *C-6* concerns the navigation, folding and creation of outlines/headlines. For instance, this document has headlines. You can use *C-6* to navigate them. Can you guess which key does what? Note in particular that you can fold and unfold all sections in this tutorial (or other documents) using *C-(6 -1 -4)* and use *C-(6 -1 4)* to unfold any particular section.

- *C-7* concerns tabs, frames and buffers.

- *C-8* is also empty to allow for mode specific bindings.

Combinations using other modifiers usually fall into the same layer-specific paradigms. Each modifier has a specific role:

- *C* is by far the most easiest to apply continuously and thus houses the most often used commands and commands that are expected to be used repeatedly.

- *M* is expected to be applied discretely, by tapping an _Alt_ key, and thus is used for commands for which it is not expected that they are used repeatedly.

- *s* is in-between the two: it can easiest be applied discretely, by holding down _Space_ and tapping an _Alt_ key, but it can still fairly easily be applied continuously by holding down either _1_ or _0_ in the number row (or your keyboard equivalent). It is thus used both for discrete and for contiuous commands, in particular variants of commands that are on a *C*-layer. For instance, *C-(2 0 -3)* houses ~backward-kill-word~ while ~*s-(2 0 -3)*~ houses ~backward-kill-paragraph~.

- *H* is used for StumpWM commands or other window-manager- or desktop-environment-specific commands. The one exception to this are commands concerning Emacs windows, which are on *H-2* and *H-8*. This is because the *H*-modifier is the only continuous modifier apart from C that can be held down by using your thumbs in the main layout and because d-stump uses *H-1* to navigate windows.

- *A* is used for bad key combinations.

** Shortcut Help Functions
You can draw any layer of any bindlist using *C-(1 0 -5) (3 0 2)* (triggering ~d-draw-bindlist-layer~). You will be asked for a bindlist (by default the ~d-emacs-d-emacs-mode-map-bindlist~ which houses global bindings), a layer and modifiers. More generally, you can draw the bindings in all bindlists whose names match a regular expression with commands, coordinates and modifiers matching regular expressions using *C-(1 0 -5) (3 0 3)* (triggering ~d-draw-bindings-from-regexps~). This command allows for very fine-grained layout analyses. This command is /very powerful/ and allows for a fine-grained analysis of the structure of Daselt's layout (and your own [[*][user-generated bindlists][** Writing Your Own Bindlists]]). Look at its command description for a more complete overview and usage examples. If you are using the package ~embark~ then you can also look up key bindings through *M-(1 0 -3)* (triggering ~embark-bindings~).

** Bad Key Configs
Many keyboards simply don't register certain key combinations. Which ones they are depends on the keyboard and thus cannot be accounted for by Daselt. To cope with this limitation, Daselt introduces the variable ~d-xkb-bad-key-combinations-list~. Whenever you find a key combination that doesn't seem to work, check if it is registered using *C-(1 0 -5) k*, and if not, add it to the ~d-xkb-bad-key-combinations-list~. Then Daselt will automatically add a variant of that key combination in which the *C*-modifier has been replaced by an *A*-modifier, which you can apply discretely by tapping right Shift and, if you have an iso-layout, also by tapping left Shift, and in which each *H*-modifier is replaced by an *s-M-*combination.

"

(unless d-stump
  "** The *C-g* issue
Emacs uses *C-g* to interrupt running processes, and this function cannot easily be remapped. Thus, Daselt by default maps whatever binding would be on *C-g* in a bindlist to *C-(1 1 -2)*. This introduces an unwanted idiosyncrasy into the layout. Two solutions to this exist:

- using StumpWM on some other window manager that can remap keyboard shortcuts.

- using Emacs's translation capabilities.

The assumption here is that the first option is impossible or unwanted. In this case, *C-g* can still be translated within Emacs, restoring Daselt's layout. The problem is that this /does not remap *C-g*'s function of aborting running processes./ In other words, if *C-g* is translated to some other key, this key becomes the default key to quit command inputs and the like, but *C-g* (in its initial position) still has to be used to abort running processes. If you keep this in mind and use Emacs 30+, you can set ~d-emacs-translate-C-1-1--2-C-g~ to ~t~ and next time ~d-emacs-mode~ is started, *C-(1 1 -2)* can be used to quit things, freeing up *C-g* to house whatever binding it should have.")

"
** The Position of the ~d-emacs-mode-map~
Daselt uses the ~d-emacs-mode-map~ as a global shortcut map that supersedes most other maps because it is put into ~emulation-mode-map-alists~. You can turn this off by disabling the custom ~d-emacs-put-d-emacs-mode-map-into-emulation~. In that case, the bindings on layers 5-8, which contain symbols not usually found in a layout, will continue to be usable but those on lower layers will be shadowed by keymaps in lists above ~minor-mode-map-alist~. It is generally recommended to leave ~d-emacs-put-d-emacs-mode-map-into-emulation~ true, since the ~d-emacs-mode-map~ was designed with the assumption of its bindings being globally valid. An in-between approach is also possible by setting ~d-emacs-globalize-d-emacs-mode-map~ to t, which sets ~global-map~ to the value of ~d-emacs-mode-map~ while ~d-emacs-mode~ is on. This means that the ~d-emacs-mode-map~ is above most keymaps but can be overridden by ~minor-mode-override-map-alist~.

** Contextual Commands
A contextual binding is a binding to a command that acts differently in different modes or when certain preconditions are met. Daselt uses contextual commands for two purposes:

- To fine-grain the behavior of keys.

- To imitate the action of other keys when translation is impossible. For instance, the bindings from *C-(1 0 -2)* to *C-(1 0 2)* are meant to imitate the Arrow and Page Up/Down keys, but the Emacs command ~key-translate~ currently only translates /characters/. If keys cannot be translated from outside Emacs, for instance through StumpWM, commands are defined that make these keys act like they should in different major or minor modes. This is not an ideal solution, but sturdy enough for the author to be used for years. If you translate these keys outside of Emacs, you can turn off these contextual commands by setting the custom ~d-emacs-include-imitation-commands~ to nil. " (if d-stump "Since you have set ~d-stump~ to t, this custom is nil by default.")

"

Each contextual command is called ~d-emacs-PLACE~, where ~PLACE~ is the place of the corresponding key in the layout, given by a prefix and the coordinates (without brackets).
Since these commands (by default) override local maps, they might need to be adapted to fit your local situation. For this, each contextual command comes with a custom ~d-emacs-PLACE-contexts-list~, which you can use to modify the behavior of the command in specific major or minor modes. Let us take look at the most important contextual commands:

*** *~d-emacs-C-3-0-2)~
*C-(3 0 2)*, is supposed to be a /completion key/ similar to how TAB is usually used. It expands yasnippets, cdlatex-environments or abbrevs and follows links, and otherwise acts mode-specific (but usually goes to the next environment or item). You can customize the mode-specific behavior of this command using the custom ~d-emacs-C-3-0-2~.

Note that you can use *C-(3 0 2)* to follow any link in this tutorial. If you follow a link, you can use *C-(5 -1 -1)* to return to your previous position, even if it is a link pointing to an external source.

*** ~d-emacs-C-1-0-2~
As mentioned, *C-(1 0 2)* generally acts like the right arrow key. However, when point is at the end of the minibuffer, it instead exits the minibuffer. You can turn off this behavior by "
(if d-stump "overwriting the binding in the ~minibuffer-local-map~ using a user-defined bindlist." "modifying ~d-emacs-1-0-1-contexts-list~") 

"

*** Mode-Specific Bindings
In general, Daselt uses the layers *3* and *8* to house major-mode-specific bindings. With the modifiers *C*, *s*, *M* and occasionally *H*, these two layers provide more than enough space for even large, command-intensive modes and packages, such as Icicles, or to set aside specific modifiers for minor modes.

An exception to the restriction of major-mode-specific bindings to these two layers is in modes that are derived from ~special-mode~, meaning modes in which writing is often not possible. This allows us to put mode-specific bindings onto places without modifiers, which would insert symbols during the writing process. At the same time, these modes already use some of these places for their bindings, and while the commands used during the writing process can usually be neatly ordered using the heuristics in Daselt, this is not always true for commands in these special modes. Moreover, some packages based on ~special-mode~ use a large amount of keymaps. For instance, Gnus uses 87 keymaps, so rebinding all of them would be very time-intensive but rebinding only some commands would run the risk of overriding others. Thus, two strategies were used for ~special-mode~-derived major modes:

- for modes with a reasonable number of maps and bindings, a complete reconfiguration has been provided (that can be turned off by toggling the corresponding custom). Examples are ~special-mode~ itself, ~Info-mode~, ~help-mode~ and ~eww-mode~. For instance, in all of these modes you can use *(2 0 -2)* and *(2 0 2)* to navigate to the previous and next item.

- for modes from packages with a large number of maps, such as Gnus and Magit, a basic reconfiguration has been supplied that uses only the layers *5-8*, which house symbols that are uncommon on keyboards and thus most likely unused by existing bindings. This way, the original binding configuration of these packages can exist alongside Daselt's bindings.

** Exotic key Combinations
The [[[** Shortcut Help Functions]][Shortcut Help Functions]] Daselt provides should allow you to gain an understanding of its layout stricture reasonably quickly. There are a few bindings that require special mention though because otherwise they run the risk of not being found:

- *C-RET* is currently the only binding in ~d-emacs-mode-map~ that refers to a key not within the Daselt-coordinate-system and inserts a newline /after/ point when being tapped.

- Similarly, *s-(0 2 0)/s-SPC* inserts a space after point.

- *<XF86Launch5>*, which is sent when you tap _CapsLock_ or its mirrored analogue (*(0 0 -6)* and *(0 0 6)* in coordinates), is not globally used. If ~d--cdlatex~ is t, it is bound to ~cdlatex-math-symbol~ in ~LaTeX-mode~, ~org-mode~ and ~minibuffer-mode~ and to ~vterm-copy-mode~ in ~vterm,mode~. Moreover, *C-<XF86Launch5>* is bound to ~rectangle-mark-mode~. More generally, the idea is to use *<XF86Launch5>* for symbol insertion and often used minor modes. 

** Non-Native Commands
You might have noticed that a lot of shortcuts point to commands from packages that are not Emacs-native. This is intentional: in case you have not installed those packages, these bindings are supposed to act as a suggestion of the functionality that could be put there. You can assign to these shortcuts your own commands by putting them into a [[Writing Your Own Bindlists][user-defined bindlist]].

* Supported Packages
Daselt offers configurations for a wide variety of packages. A complete list of packages with separate configurations is stored in ~d-emacs-pkgs-list~. 
The configuration for any package can be turned on and off by toggling the corresponding d-emacs-custom, whose default is ~t~ if and only if that package is currently installed, and restarting ~d-emacs-mode~.

Packages with context-dependent behavior embedded in Daselt-commands are [[https://github.com/abo-abo/avy][Avy]], [[https://github.com/minad/corfu][Corfu]], and [[https://github.com/joaotavora/yasnippet][Yasnippet]]. There is a considerable synergy between Daselt and Avy in particular and for Avy, Corfu and Vertico, Daselt comes with special instructions that are shown below if and only if the d-emacs-custom for that package is ~t~.

"
(if (or d-emacs-avy d-emacs-corfu d-emacs-vertico)
    "** Quick Key Navigation
Daselt houses a special bindlist ~d-special-quick-keys-bindlist~ for quick keys that can be used with Avy, Corfu and [[https://github.com/minad/vertico][Vertico]]. This bindlist is automatically generated and by default includes all symbols on all layers specified by ~d-quick-key-layers-list~, which are layers *1-7* by default. It is highly recommended to use quick key navigation often and with many symbols because it helps you remember all symbols in the layout. Layer *8* is not included because there is not much structure in the Emoji layer that simplifies remembering its symbols. However, the ranking of keys in ~d-special-quick-keys-bindlist~ can be customed to your exact liking using the customs ~d-quick-key-coords-base-list~ and ~d-quick-key-layers-list~. 

*** *C-(1 0 4)*
*C-(1 0 4)* is another contextual key: it acts as ~avy-goto-word-1~ in most situations, but if Corfu or Vertico are showing completion candidates and ~d-emacs-corfu~ resp. ~d-emacs-vertico~ are *t*, it starts quick-key-completion for those completion candidates.

")

"** In-Buffer-Completion
In-buffer-completion in Daselt is supposed to be contextual as well: *C-(1 0 2)* is supposed to choose a completion candidate, *C-(1 0 -2)* to quit the completion process and *C-(1 0 -3)/C-(1 0 3)* to cycle candidates. This behavior is currently implemented for Corfu by putting the ~corfu-map~ above the ~d-emacs-mode-map~ in ~emulation-mode-map-alists~. If you are using another completion framework you are advised to similarly configure it.
Daselt puts the ~corfu-map~ above the ~d-emacs-mode-map~ in
*C-(1 0 2)* houses a contextual command that usually moves one character forward, but 

- if point is at the end of the minibuffer, *C-(1 0 2)* exits.

- similarly, if d-emacs-corfu is ~t~ and corfu displays some completions, *C-(1 0 2)* picks the currently active completion. This can be a bit confusing at first, but makes picking Corfu-completions almost effortless.

*C-(1 0 -2)*, *C-(1 0 3)* and *C-(1 0 -3)* are similarly contextual:

- while Corfu is showing completion candidates, *C-(1 0 -2)* stops an active Corfu-completion-process, *C-(1 0 -3)* picks the previous completion and *C-(1 0 3)* picks the next.

- in the first line of the minibuffer, *C-(1 0 -3)* goes to the previous element in the minibuffer history. Similarly, *C-(1 0 3)* goes to the next if at the last line.
"

(if d-emacs-yasnippet
    "** Yasnippet
It has already been [[** *C-(3 0 2)*][mentioned]] that *C-(3 0 -2)* is used to expand yasnippets (as well as expanding abbrevs and following links). Moreover, while you fill the fields of a snippet you can use *C-(3 0 -3)* to navigate to the previous field and *C-(3 0 3)* to navigate to the next.

")

"** Recommended Packages
Though vanilla Emacs suffices to run Daselt, it can be greatly improved by adding some more packages:

- [[https://github.com/rolandwalker/back-button][Back-Button]] to navigate from and to previous positions within and between buffers.

- [[https://gitlab.com/nameiwillforget/avy-act][Avy-Act]] To make Avy-Act from a distance (from the creator of Daselt).

- [[https://github.com/Fuco1/smartparens][Smartparens]] to interact with balanced expressions.

- [[https://github.com/leoliu/easy-kill][Easy-Kill]] to quickly mark and kill text.

- [[https://github.com/oantolin/embark][Embark]], particularly or its command ~embark-bindings~.

- A minibuffer completion framework. Vertico is very popular and solid. A more powerful but involved framework is [[https://www.emacswiki.org/emacs/Icicles][Icicles]]. Daselt has fairly complete configurations for both.

* Customization
** Writing Your Own Bindlists
The best way to override bindings bound by ~d-emacs~ is to create a user-defined bindlist for the same map. An example-file is in ~D-EMACS-DIRECTORY/pkg-configs/d-emacs-special-example-user-defined-bindlist.el~, complete with detailed instructions for how to write ~bindlists~-files. Note that you can use the command ~d-draw-free-places-from-regexps~ to visualize the free places in the layout similarly to how you can visualize bindings using ~d-draw-bindings-from-regexps~.

** Importing your bindings
To import your keybindings, the function `d--convert-bindings-to-bindlist' has been provided. To use it, simply mark the bindings you want to convert and call the command, and it will put them into bindlists, or mark nothing to canvert all bindings in the current buffer. It should convert most kinds of bindings, including `:bind'-sections of `use-package'-configs, but you are encouraged to review the generated bindlists for consistency and completeness. Once your bindings are organized into bindlists, you can put them into user-defined `bindlists' files, as described in ~D-EMACS-DIRECTORY/pkg-configs/d-emacs-special-example-user-defined-bindlist.el~ and they will be applied automatically when `d-emacs-mode' is started next time.

Putting your bindings into bindlists has a variety of advantages:

- they can be sorted using `d--sort-and-format-bindlists-in-file' or automatically whenever a bindlist-file is saved if `d-sort-bindlists-at-file-save' is true. The sorting function also tells you instances where it finds the same place occupied by different bindings in the bindlist (so that one would override the other).

- they (and their free binding places) can be visualized using the help functions already introduced.

- they can be quickly navigated to using *s-C-(1 1 -4)*/~d-find-bindlists-file~.

- if there is an error in any bindlist, even if the error is due to an unbalanced expression, it does not result in an error when starting ~d-emacs-mode~, only a warning telling you the file where the error occured (similar is true for all files ~d-emacs-mode~ loads except for the core functionalities).

- the configuration `d-emacs' provides can be de-applied at any time by turning off `d-emacs-mode' and fine-tuned using the customs it automatically generates.

** Using ~d-emacs~ as an init-manager
~d-emacs~ can be used as an alternative to `use-package' to manage your init. The main differences are again that ~d-emacs~ is organized to recurse throughout the ~D-EMACS-DIRECTORY/pkg-configs/d-emacs~-directory and so allows you to spread your config over several files and folders, automatically generating customs for every folder it encounters, so that it allows you to always re-set your config with different parts turned on and off, depending on the customs you have enabled, and that any file can be quickly navigated to using *M-C-(1 1 4)*/~d-find-pkg-file-by-type~. This functionality might be put into its own package at some point.

* Instructions for other parts of Daselt
** d-stump
"
(if d-stump
    (format "Since the custom of Daselt's component d-stump is set to t, it is assumed that you want to use Daselt's StumpWM config. To do so, just read d-stump's configuration file into your StumpWM init like this:
#+begin_src elisp
(load %s/d-stump/d-stump.lisp)
#+end_src
Note that you can use all helper functions for d-stump-bindlists just like for d-emacs-bindlists. Bindings in d-stump generally use the H-modifier or are in the root map, which is accessed by tapping the keys *(0 -1 -6)* and *(0 -1 6)*. If you want to change bindings or the d-stump config in general, it is recommended you write up your changes in ~%s/pkg-configs/d-stump~, then use ~d-stump--generate-init~ to generate your init, this way you can apply the helper functions in Emacs to the bindlists you write as well.

One great annoyance about StumpWM is that it cannot access the higher layers of an xkb-layout (see here: [[https://github.com/stumpwm/stumpwm/wiki][Problems Stump has with XKB]]). Since Daselt houses most of its special symbols on its third layer and its numbers on the fourth, this makes direct interfacing with StumpWM fairly annoying. If you need to communicate with StumpWM often, it is recommended to instead [[https://kaashif.co.uk/2015/06/28/hacking-stumpwm-with-common-lisp/][use SLIME]] to connect to StumpWM from within Emacs. However, for everyday usage this should be unnecessary, and you can always write permanent changes into d-stumps pkg-configs, then re-generate your init, then reload it in StumpWM using *(0 -1 6) (0 -1 -4)*" d-directory d-directory d-emacs-directory)
  "Since you have the custom ~d-stump~ set to nil, it is assumed that you do not plan on using Daselt's StumpWM component. Remember that if you change your mind you can re-generate this tutorial to find here installation and usage instructions.

")

"** d-tri"
(if d-tri
    (format "Since the ~d-tri~-custom is set to t, it is assumed that you want to use Daselt's Tridactyl config. For this, use ~source~ in Tridactyl's commandline to read ~%s/d-tri~. If for some reason Tridactyl doesn't read the file, try copying the contents of it and using ~source --clipboard~ in the Tridactyl commandline. d-tri only has one bindlist, but it can be inspected through Daselt's helper functions like any other." d-directory)
  "Since you have the custom ~d-tri~ set to nil, it is assumed that you do not plan on using Daselt's Tridactyl component. Remember that if you change your mind you can re-generate this tutorial to find here installation and usage instructions."))

;;; d-tutorial.el ends here
