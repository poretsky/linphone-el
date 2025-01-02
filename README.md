# Software SIP-telephone in Emacs

This is an Emacs interface to the well-known Linphone application. It
uses console version of the Linphone as a backend. In major Linux
distributions this console version is usually provided as
`linphone-nox` or `linphone-nogtk` package. And also the `amixer`
utility is used for audio system tuning.


## Installation

Place this archive in your home directory and include the following
lines in your `~/.emacs` file:

```elisp
(add-to-list 'load-path "~/linphone-el/lisp")
(autoload 'linphone "linphone" "Internet telephone" t)
```


## Usage

You can activate Linphone from within Emacs by typing
`M-x linphone <RET>`. The control panel with several buttons will be
popped up. At first you have to register your account by pressing
`Register` button and providing the account data:

- your identity (full SIP address in the form user@some.host),
- proxy host name if it differs from the one included in your identity,
- password.

After successful registration the main control panel
will change and suggest more options. To make an outgoing call
press `Call` button and enter target SIP address. If you just want
to stand by waiting for incoming calls, press `Standby` button.
The control panel will vanish and Linphone will continue proceeding
in background. When incoming call will be encountered,
the corresponding control panel will appear in a separate window.
Then you can switch to this window and accept or reject the call
by pressing an appropriate button. You can change your account
at any time by pressing `Unregister` button on the main control panel
and then registering again with new data.

## Note

This interface doesn't allow configuring the backend program itself.
You should do it either using native means or by editing it's
configuration file directly.
