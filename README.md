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

### References
* http://www.jesshamrick.com/2012/09/18/emacs-as-a-python-ide/
* https://realpython.com/blog/python/emacs-the-best-python-editor/
* http://aaronbedra.com/emacs.d/
* https://github.com/wasamasa/dotemacs/blob/master/init.org#evil
