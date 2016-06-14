# emacs-config
My personal emacs configuration file.

### Prerequisite
This configuration best works with gnu emacs 24.4+ gui version.

### Installation of emacs
To install gnu emacs gui version for mac use following link.
http://emacsformacosx.com/

Please make sure you don't have other emacs init file. For GnuEmacs init file could be at `~/.emacs` or `~/.emacs.el` or `~/.emacs.d/init.d`. If you have above init file then remove those.

Add init.el file following to `~/.emacs.d/` location and restart emacs. Make sure to have internet connection.

### Set up for elpy
Elpy needs some python packages to be installed in order to work properly. The packages needs to be installed are mentioned in the elpy github site as follows.
* https://github.com/jorgenschaefer/elpy

If you are using virtualenv while working with python I would recomend to install packages mentioned for elpy in base python. Then you can create virtualenv with option `--system-site-packages`. By doing so virtualenv will try to look up elpy dependent packages in base python. Advantage of this approach is that don't have to install elpy dependent packages in each virtualenvs and secondly your virtualenv can have packages dependent only for your project.

You can look up for my [blog](https://techfillip.wordpress.com/2016/06/10/emacs-elpy-convenient-python-set-up/) on proper set up of elpy as well.

Search for `pyvenv-work` in the init.el and use your virtual environment name.

### Key configurations
I use EVIL package because I like VIM mode. So VIM keys will work for editing. I have used `jk` to act as VIM `ESC` in insert mode. Please find following keys as per my configurations.
(All keys are prefixed with <SPC> as I have set <SPC> as EVIL-LEADER)

I     :    Start editing emacs init file
T     :    Start ansi-term with zsh
f     :    Open a file
c     :    Comment or uncomment a region'comment-or-uncomment-region

b stands for buffer
bk    :    Kill a buffer
br    :    Rename a buffer
bR    :    Reload a buffer
bs    :    Buffer switch

e stands for elpy
ed    :    Go to definition in elpy
es    :    Open elpy shell (iPython)
er    :    Send region to shell (iPython)
ec    :    Send current statement to shell (iPython)

p stands for project
pp    :    Projectile project switch
pf    :    Find a file in project
pg    :    Grep inside project
pi    :    Empyt projectile cache

t stands for tree
tt    :    Toggle project explorer

w stands for workspace
ws    :    Switch workspace
wr    :    Rename a workspace
wk    :    Kill a workspace
wl    :    Next workspace (l as per vim)
wh    :    Previous workspace (h as per vim)

You can always check init.el above defined keys.

### References
* http://www.jesshamrick.com/2012/09/18/emacs-as-a-python-ide/
* https://realpython.com/blog/python/emacs-the-best-python-editor/
* http://aaronbedra.com/emacs.d/
* https://github.com/wasamasa/dotemacs/blob/master/init.org#evil
