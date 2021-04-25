Forth SuperCollider
-------------------

`hsc3-forth` is a simple [Forth](http://www.forth.org/) interpreter.

The only data type is the [SuperCollider](http://audiosynth.com/) `Unit Generator`.

[Why Forth SuperCollider?](?t=hsc3-graphs&e=lib/sc/graph/jmcc-why-supercollider.scd)

~~~~
: rvb 5 0 do 0.05 0 0.05 rand.ir 2 clone 1 allpassn loop ; 0.2 dust.ar
50 * 200 3200 rand.ir 0.003 resonz 10 clone mix dup 0.046 0.048 delayn
0.1 0 0.1 rand.ir lfnoise1.kr 0.04 * 0.05 + 15 combl 7 clone mix rvb
0.2 * + play
~~~~

<!-- ![](sw/hsc3-graphs/svg/why-supercollider.svg) -->

There is an hsc3-forth [emacs](http://www.gnu.org/software/emacs/) mode,
a derivative of [forth-mode](http://www.gnu.org/software/gforth/).

There is an OSX (10.9.3) binary
([hsc3-forth](sw/hsc3-forth/osx/hsc3-forth.xz).[xz](http://tukaani.org/xz/)),
or to build type:

~~~~
cd ~/sw/hsc3-forth/hs ; prefix=~/opt make install
~~~~

An environment variable locates the hsc3-forth library files:

~~~~
export HSC3_FORTH_DIR=$HOME/sw/hsc3-forth/fs
~~~~

To hear the above type `C-cC-r` in emacs, or in a shell type:

~~~~
hsc3-forth < ~/sw/hsc3-forth/help/graph/jmcc-why-supercollider.fs
~~~~

To quieten there is the word `stop`.

There is a
[tutorial](?t=hsc3-forth&e=help/tutorial.fs), and
[translations](?t=hsc3-forth&e=help/jmcc.fs) of graphs by
James McCartney <!-- (http://audiosynth.com/autobio/emu.jpg) -->
from the `SC2` manual.

hsc3-forth is `case-insensitive`, in emacs type `C-xC-l` or `C-xC-u`.

hsc3-forth follows the [haskell](http://haskell.org/) SuperCollider ([hsc3](?t=hsc3)) rules.

hsc3-forth requires `scsynth` to be listening at the standard UDP port (57110).

<!--
HSC3-FORTH is partial, it is known to work as far as the translated
graphs at [HSC3-GRAPHS](?t=hsc3-graphs), search for `FS` in the
[INDEX](?t=hsc3-graphs&e=md/ix.md).
-->

<!-- HSC3-FORTH is for [FORTH IMPACT](http://forthimpact.bandcamp.com/). -->

<!--
([AG](http://www.alexandergarsden.com/) and [SD](http://www.samueldunscombe.com/)
-->

initial announcement:
[[local](?t=hsc3-forth&e=help/announce.text)]
2014-10-15 04:52:53 GMT

<!--
[haskell-art](https://lurk.org/groups/haskell-art/messages/topic/znDbu2Xon6WryqsEvlaZf/),
[gmane](http://article.gmane.org/gmane.comp.lang.haskell.art/1025),
[bham](http://www.listarc.bham.ac.uk/lists/sc-users/msg42055.html)
-->

© [rd](http://rohandrape.net/), 2014-2021, [gpl](http://gnu.org/copyleft/).
