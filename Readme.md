What's this?
============

company-mode backend for Dlang, using [DCD](https://github.com/Hackerpilot/DCD).

Installation
------------

First, follow setup section of DCD's readme.
And `M-x package-install company-dcd` on your Emacs.

Configuration
-------------

1: Make sure `dcd-client` and `dcd-server` binaries are in your `executable-path`. Otherwise, please set the variables  `company-dcd-client-executable` and `company-dcd-server-executable` using `M-x customize`.

2: Add the following to your `init.el`:
```emacs
(require 'company-dcd)
(add-hook 'd-mode-hook 'company-dcd-mode)
```

Keybinds
--------

* Show ddoc with `C-c ?`
* Goto definition with `C-c .`
* Search symbol with `C-c s` (If region is active, search for the region string.)
* After goto definition or search symbol, you can pop to previous position with `C-c ,`

Known issues
------------

None. If you find something curious, fell free to create an issue.
