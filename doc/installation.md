# Installation

1. Install Gambit and its dependencies, as well as Emacs and its configuration. This is easiest done via [chjize](https://github.com/pflanze/chjize). Follow the instructions in the chjize repository. 

        make gambit
        # and, necessary for some of the modules in chj-scheme:
        make chj

    This does:

    * install all necessary Debian packages
    * get, compile (in C++ mode) and install (a compatible version of) [Gambit with a number of patches](https://github.com/pflanze/gambc)
    * a working Emacs configuration, including gambit.el, paredit.el, and the custom config that Christian uses.
    * verify signatures on all packages (either they are Debian packages, or carry signatures by Christian which are verified via [git-sign](https://github.com/pflanze/git-sign))

2. Set up Emacs:

    * Choose or create a unix user to use
    * As that user, set up ~/.emacs, ~/.emacs.d/, and ~/bin or PATH to have all the settings and tools; easiest by using [chj-home](https://pflanze@github.com/pflanze/chj-home) which is easiest to install via chjize as mentioned above, then as that user running:
    
            /opt/chj/chjize/bin/mod-user
            trash ~/.emacs.d  # if already created earlier
            # log out, and in again, run the command that it prints. log
            # out and in again.

1. Get chj-schemelib

        git clone https://github.com/pflanze/chj-schemelib
    
1. Clone the project using it

        git clone ...foo.git
        cd foo
        ln -s ../chj-schemelib

1. Start emacs from that same directory (or, from a running emacs instance, open that directory), then start Gambit inside it. This relies on step 2 above.

        emacs
        M-x run-scheme

    This will compile all used modules then give you a Gambit repl inside emacs. Run the test suite over all modules that are in use:
    
        (run-tests)

