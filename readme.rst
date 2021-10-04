#################
Revert Buffer All
#################

This package provides a functionality to revert all buffers, without prompting for feedback.

Available via `melpa <https://melpa.org/#/revert-buffer-all>`__.


Motivation
==========

When performing any action on a large project,
you may want to revert all buffers without being prompted for input.

This package provides the ``revert-buffer-all`` command to do this with a single manual action.


Usage
=====

When running ``revert-buffer-all``, all buffers are reverted without prompting.
Progress is displayed in the message buffer as multiple large files can take some time.

Finally a summary message is shown, including the number of buffers that were reverted,
buffers that were closed and any errors in the unlikely case there is an error reverting the buffer.


Key Bindings
------------

It's suggested to bind this to a key you're not likely to press by accident, e.g:

.. code-block:: elisp

   (global-set-key (kbd "C-S-M-r") 'revert-buffer-all)


Evil users may wish to bind this to the leader key:

.. code-block:: elisp

   (evil-define-key 'normal 'global (kbd "<leader>r") 'revert-buffer-all)


Details
=======

While reverting all buffers may seem like a simple operation, this package handles details such as:

- Reporting errors without this stopping other buffers from being reverted.

- Undo data is cleared to prevent accidentally entering the previous buffers state while undoing.

- Support for packages that restore undo data in packages such as
  `undo-fu-session <https://gitlab.com/ideasman42/emacs-undo-fu-session>`__.

- When a buffers file no longer exists, the buffer is removed.


Installations
=============

.. code-block:: elisp

   (use-package revert-buffer-all
     :commands (revert-buffer-all))
