
What's this?
======
company-mode backend for Dlang, using DCD(https://github.com/Hackerpilot/DCD).

Installation
------------
From melpa.

Configuration
------------

Make sure that the `dcd-client` and `dcd-server` binaries are in your `executable-path`. Otherwise, please
set the variables  `dcd-client-executable` and `dcd-server-executable` using `M-x customize`.
And insert following to your init.el.
```
(require 'company-dcd)
(add-hook 'd-mode-hook 'company-dcd-mode)
```

Keybinds
------------
* Show ddoc with `C-c ?`
* Goto definition with `C-c .`
* After goto definition, you can pop to previous position with `C-c ,`

Known issues
------------
Currently, calltip completion requires one extra key push.
Pushing any key after symbol completion will execute calltip completion.


ToDo
------------
Search for symbol is not supported yet.
